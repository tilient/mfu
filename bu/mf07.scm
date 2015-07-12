(use srfi-1 srfi-13 posix)

;;; Params  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *app-id*   "44501")
(define *api-key*  "dia6jjylyxo5esfj61an33wtwxj7d8npddo2noff")
(define *email*    "wiffel@tilient.net")
(define *password* "ttT1l1ent")

;;; Session Token ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *session* "0")
(define *session-time* 0)

(define (session)
  (let ((age (- (current-seconds) *session-time*)))
    (when (> age 400)
      (set! *session-time* (current-seconds))
      (create-mediafire-session)))
  *session*)

;;; Tools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (try-it fun)
  (let loop ((ix 3))
    (if (zero? ix)
      (begin
        (print "*** ERROR *** Giving up ***")
        #f)
      (handle-exceptions exn
        (begin
          (print "*** ERROR *** Retrying ***")
          (sleep ix)
          (loop (- ix 1)))
        (fun)))))


(define (string-escape str)
  (string-translate* str '( (" " . "\\ "))))

(define (string-url-encode str)
  (string-translate* str '((" " . "%20")
                           ("#" . "%23")
                           ("&" . "%26")
;                          ("[" . "%5B")
;                          ("]" . "%5B")
                           ("{" . "%7B")
                           ("}" . "%7D"))))

(define (cmd . strs)
; (print "CMD: "(apply string-append strs))
  (call-with-input-pipe 
    (apply string-append strs) 
    (cut read-string #f <>)))

(define (cmd-curl url . strs)
  (let ((url (string-append "\"https://www.mediafire.com/api/1.3/"
                            url "&session_token=" (session) "\""))
        (params (apply string-append strs)))
;   (print "-->>" url)
;   (print "..>>" params)
    (let ((response (cmd "set -f; nice -n 19 curl -s -g " params url )))
;     (print "==>>" response)
      (if (equal? (string-copy response 0 13) "<?xml version")
        response
        (  begin
          (print "*** ERROR ***")
          (apply cmd-curl url strs))))))


(define (sha256-of-file filename)
  (let ((res (cmd "set -f; nice -n 19 openssl dgst -r -sha256 \"" 
                                                    filename "\"")))
    (string-copy res 0 64)))

(define (sha1-of-string str)
  (let ((res (cmd "echo -n '" str "' | openssl dgst -r -sha1")))
    (string-copy res 0 40)))

(define (string-between str start-str end-str)
  (let* ((start-pos (+ (string-length start-str)
                       (string-contains-ci str start-str)))
         (end-pos (string-contains-ci str end-str start-pos)))
    (string-copy str start-pos end-pos)))

;;; Downloader ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-worker)
  (let-values 
    (((b-pipe-from-scm-to-proc t-pipe-from-scm-to-proc) (create-pipe))
     ((b-pipe-from-proc-to-scm t-pipe-from-proc-to-scm) (create-pipe))
     ((pid) (process-fork)))
    (when (= 0 pid)
      (file-close b-pipe-from-proc-to-scm)
      (file-close t-pipe-from-scm-to-proc)
      (let ((i (open-input-file*  b-pipe-from-scm-to-proc ))
            (o (open-output-file* t-pipe-from-proc-to-scm )))
        (write-line "DONE" o)
        (flush-output o)
        (let loop ((cmd (read-line i )))

          (try-it (lambda ()
            (print "CMD: " cmd)
            (call-with-input-pipe 
              cmd
              (cut read-string #f <>))))

          (write-line "DONE" o)
          (flush-output o)
          (loop (read-line i)))))
    (file-close b-pipe-from-scm-to-proc)
    (file-close t-pipe-from-proc-to-scm)
    (list (open-input-file* b-pipe-from-proc-to-scm)
          (open-output-file* t-pipe-from-scm-to-proc))))

(define *workers* (list (create-worker) (create-worker) (create-worker)))

(define (free-worker)
  (let ((free (filter (lambda (w) (char-ready? (first w))) *workers*)))
    (if (null? free)
      (begin
        (sleep 1)
        (free-worker))
      (let ((w (first free)))
        (read-line (first w))
        (second w)))))
  
(define (schedule-work cmd)
  (let ((w (free-worker)))
    (write-line cmd w)
    (flush-output w)))

;;; MediaFire ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (create-mediafire-sub-folder parent-key folder-name)
  (let* ((url (string-append
                   "folder/create.php?"
                   "foldername=" (string-url-encode folder-name)
                   "&parent_key=" parent-key
                   "&action_on_duplicate=replace"))
          (response (cmd-curl url)))
    (string-between response "<folder_key>" "</folder_key>")))

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
  (schedule-work
    (string-append "set -f; nice -n 19 curl -s -g --limit-rate 3M -m " 
                   (number->string 
                     (+ (quotient (file-size filepath) 250000) 15))
                   " -F \"file=@\\\"" filepath 
                   "\\\";filename=\\\"" target-name "\\\"\" "
                   "\"https://www.mediafire.com/api/1.3/"
                   "upload/simple.php?"
                   "path=" (string-url-encode target-path)
                   "&action_on_duplicate=replace"
                   "&session_token=" (session) "\"")))
                

(define (upload-file-if-needed filepath target-path target-name)
  (if (file-up-to-date? target-path target-name)
    (print "[ ] " filepath)
    (upload-file filepath target-path target-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (upload-dir-in folder-key path name) 
  (let ((fullpath (string-append path "/" name)))
    (if (directory? fullpath)
      (let ((sub-folder-key (create-mediafire-sub-folder
                              folder-key name)))
        (for-each 
          (lambda (n) (upload-dir-in sub-folder-key fullpath n))
          (directory fullpath #t)))
      (upload-file-if-needed fullpath path name))))

(define (upload-dir path)
  (let ((key (create-mediafire-folder path)))
    (for-each 
      (lambda (name) (upload-dir-in key path name))
      (directory path #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main)
; (upload-dir "/home/wiffel/kashbah"))
; (upload-dir "/backups")
; (upload-dir "/home/wiffel/Pictures")
; (upload-dir "/home/wiffel/Music")
  (upload-dir "/home/wiffel/Videos"))

(main)
(quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
