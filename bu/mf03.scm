(use srfi-13 posix)

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
      (print "== NEW SESSION ==")
      (set! *session-time* (current-seconds))
      (create-mediafire-session)))
  *session*)

;;; Tools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (let ((response (cmd "set -f; curl -s --globoff " params url)))
;     (print "==>>" response)
      (if (equal? (string-copy response 0 13) "<?xml version")
        response
        (  begin
          (print "*** ERROR ***")
          (apply cmd-curl url strs))))))


(define (sha256-of-file filename)
  (let ((res (cmd "set -f; openssl dgst -r -sha256 \"" filename "\"")))
    (string-copy res 0 64)))

(define (sha1-of-string str)
  (let ((res (cmd "echo -n '" str "' | openssl dgst -r -sha1")))
    (string-copy res 0 40)))

(define (string-between str start-str end-str)
  (let* ((start-pos (+ (string-length start-str)
                       (string-contains-ci str start-str)))
         (end-pos (string-contains-ci str end-str start-pos)))
    (string-copy str start-pos end-pos)))

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
  (let* ((url (string-append "upload/simple.php?"
                             "path=" (string-url-encode target-path)
                             "&action_on_duplicate=replace"))
         (response (cmd-curl 
                     url  "-F \"file=@\\\"" filepath 
                     "\\\";filename=\\\"" target-name "\\\"\" ")))
    (member (string-between response "<result>" "</result>")
            '("0" "Success"))))

(define (upload-file-if-needed filepath target-path target-name)
  (if (file-up-to-date? target-path target-name)
    (print "[ ] " filepath)
    (upload-file filepath target-path target-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (upload-dir-B folder-key path name) 
  (let ((fullpath (string-append path "/" name)))
    (if (directory? fullpath)
      (let ((sub-folder-key (create-mediafire-sub-folder
                              folder-key name)))
        (for-each 
          (lambda (n) (upload-dir-B sub-folder-key fullpath n))
          (directory fullpath #t)))
      (upload-file-if-needed fullpath path name))))

(define (upload-dir path)
  (let ((key (create-mediafire-folder path)))
    (for-each 
      (lambda (name) (upload-dir-B key path name))
      (directory path #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main)
  (upload-dir "/home/wiffel/Music"))

(main)
(quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
