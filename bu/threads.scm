(use srfi-13 srfi-18 posix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-between str start-str end-str)
  (let* ((start-pos (+ (string-length start-str)
                       (string-contains-ci str start-str)))
         (end-pos (string-contains-ci str end-str start-pos)))
    (string-copy str start-pos end-pos)))

(define (cmd . strs)
; (print "CMD: "(apply string-append strs))
  (let-values (((o i p) (process (apply string-append strs))))
    (let loop ()
      (unless (char-ready? o)
        (thread-sleep! 0.1)
        (loop)))
    (read-all o)))

(define (cmd-curl url . strs)
  (let* ((url (string-append "\"https://www.mediafire.com/api/1.3/"
                             url "&session_token=" (session) "\""))
         (params (apply string-append strs))
         (res (cmd "set -f; nice -n 19 curl -s -g " params url )))
    (print "RES: " res)
    res))

(define (sha1-of-string str)
  (string-copy 
    (cmd "echo -n '" str "' | openssl dgst -r -sha1")
    0 40))

(define (sha256-of-file filename)
  (let ((res (cmd "set -f; nice -n 19 openssl dgst -r -sha256 \"" 
                                                    filename "\"")))
    (string-copy res 0 64)))

(define (string-url-encode str)
  (string-translate* str '((" " . "%20") ("#" . "%23") ("&" . "%26")
                           ("{" . "%7B") ("}" . "%7D"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Workers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *work* (make-queue))
(define *work-mutex* (make-mutex))

(define-syntax work-mutex-guarded-run
  (syntax-rules ()
    ((work-mutex-guarded-run <body> ...)
       (begin
         (mutex-lock! *work-mutex*)
         (let ((result  (begin <body> ...)))
           (mutex-unlock! *work-mutex*)
           result)))))

(define-syntax schedule-work
  (syntax-rules ()
    ((schedule-work <body> ...)
       (add-work (lambda ()
         <body> ...)))))

(define (add-work fun)
  (work-mutex-guarded-run
    (queue-add! *work* fun)))

(define (get-work)
  (work-mutex-guarded-run
    (and (not (queue-empty? *work*))
         (queue-remove! *work*))))

(define (get-work-or-wait)
  (or (get-work)
      (begin (thread-sleep! 0.2) (get-work-or-wait))))

(define (worker)
  ((get-work-or-wait))
  (worker))

(thread-start! worker)
(thread-start! worker)
(thread-start! worker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mediafire
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *app-id*   "44501")
(define *api-key*  "dia6jjylyxo5esfj61an33wtwxj7d8npddo2noff")
(define *email*    "wiffel@tilient.net")
(define *password* "ttT1l1ent")

;;; Session Token ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *session* "0")
(define *session-time* 0)

(define (session)
  (let ((age (- (current-seconds) *session-time*)))
    (when (> age 400)
      (set! *session-time* (current-seconds))
      (create-mediafire-session)))
  *session*)

(define (create-mediafire-session)
  (let* ((signature (sha1-of-string
                      (string-append *email* *password* 
                                     *app-id* *api-key*)))
         (url (string-append "user/get_session_token.php"
                             "?signature=" signature
                             "&email=" *email*
                             "&password=" *password*
                             "&application_key=" *api-key*
                             "&application_id=" *app-id*
                             "&token_version=1"))
         (response (cmd-curl url))
         (token (string-between response 
                                "<session_token>" "</session_token>")))
    (set! *session* token)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-mediafire-folder folder)
  (let* ((ix     (string-index-right folder #\/))
         (name   (if ix (string-copy folder (+ 1 ix)) folder))
         (parent (if (and ix (> ix 0))
                   (string-append "&parent_key="
                                  (create-mediafire-folder 
                                    (string-copy folder 0 ix)))
                   ""))
         (url    (string-append
                   "folder/create.php?"
                   "foldername=" (string-url-encode name)
                   parent
                   "&action_on_duplicate=replace"))
         (response (cmd-curl url)))
    (string-between response "<folder_key>" "</folder_key>")))

(define (create-mediafire-sub-folder parent-key folder-name)
  (let* ((url (string-append
                   "folder/create.php?"
                   "foldername=" (string-url-encode folder-name)
                   "&parent_key=" parent-key
                   "&action_on_duplicate=replace"))
          (response (cmd-curl url)))
    (string-between response "<folder_key>" "</folder_key>")))

(define (file-up-to-date? parent-path name)
  (let* ((filepath (string-append parent-path "/" name))
         (url    (string-append
                    "upload/check.php?" 
                    "filename=" (string-url-encode name)
                    "&size=" (number->string (file-size filepath))
                    "&hash=" (sha256-of-file filepath)
                    "&path=" (string-url-encode parent-path)))
         (response (cmd-curl url))
         (file-exists (equal?  "yes" 
                               (string-between response 
                                  "<file_exists>" "</file_exists>"))))
    (and file-exists 
         (equal?  "no" (string-between response 
                         "<different_hash>" "</different_hash>")))))

(define (upload-file filepath target-path target-name)
  (print "[u] " filepath)
  (cmd "set -f; nice -n 19 curl -s -g --limit-rate 3M -m " 
       (number->string (+ (quotient (file-size filepath) 250000) 15))
       " -F \"file=@\\\"" filepath 
       "\\\";filename=\\\"" target-name "\\\"\" "
       "\"https://www.mediafire.com/api/1.3/"
       "upload/simple.php?"
       "path=" (string-url-encode target-path)
       "&action_on_duplicate=replace"
       "&session_token=" (session) "\""))
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Walker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (upload-file-in fullpath path name folder-key) 
  (schedule-work
    (if (file-up-to-date? path name)
      (print "[.] " fullpath))
      (schedule-work
        (upload-file fullpath path name))))

(define (upload-dir-in fullpath path name folder-key) 
  (schedule-work
    (print "[d] " fullpath)
    (let ((key (create-mediafire-sub-folder folder-key name)))
      (for-each 
        (lambda (name) 
          (schedule-work
            (upload-in fullpath name key)))
        (directory fullpath #t)))))

(define (upload-in path name folder-key) 
  (let ((fullpath (string-append path "/" name)))
    (if (directory? fullpath)
      (upload-dir-in fullpath path name folder-key) 
      (upload-file-in fullpath path name folder-key) )))

;;;;;;;;;;

(define (upload path)
  (let* ((ix        (string-index-right path #\/))
         (base-path (if ix (string-copy path 0 ix) 
                           (current-directory) ))
         (name      (if ix (string-copy path (+ 1 ix)) 
                           path ))
         (key       (create-mediafire-folder path)))
    (upload-in base-path name key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(upload "/home/wiffel/dev/mediafire")
