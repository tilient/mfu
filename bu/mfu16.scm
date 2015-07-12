(use srfi-1 srfi-13 srfi-18 posix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *app-id*   "44501")
(define *api-key*  "dia6jjylyxo5esfj61an33wtwxj7d8npddo2noff")
(define *email*    "wiffel@tilient.net")
(define *password* "ttT1l1ent")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-between str start-str end-str)
  (let* ((pos       (string-contains-ci str start-str))
         (start-pos (and pos (+ pos (string-length start-str))))
         (end-pos (and start-pos
                       (string-contains-ci str end-str start-pos))))
    (if end-pos
      (string-copy str start-pos end-pos)
      "-not-found-")))

(define (xml-value str key)
  (string-between str
                  (string-append "<" key ">")
                  (string-append "</" key ">")))

(define (xml-number-value str key)
  (string->number (xml-value str key)))

(define (xml-value? str key val)
  (equal? val (xml-value str key)))

(define (xml-value-in? str key vals)
  (member (xml-value str key) vals))

(define (string-sh-escape str)
  (string-translate* str '(("\\" . "\\\\") ("`" . "\\`")
                           ("$" . "\\$"))))

(define (string-url-encode str)
  (string-translate* str '((" " . "%20") ("#" . "%23") ("&" . "%26")
                           ("`" . "%60") ("{" . "%7B") ("}" . "%7D"))))

(define (sha1-of-string str)
  (string-copy
    (call-with-input-pipe
      (string-append "echo -n '" str "' | openssl dgst -r -sha1")
      (cut read-string #f <>))
    0 40))

(define (sha256-of-file filename)
  (string-copy
    (call-with-input-pipe
      (string-append "set -f; nice -n 19 openssl dgst -r -sha256 \"" 
                     (string-sh-escape filename) "\"")
      (cut read-string #f <>))
    0 64))

(define (try-it fun)
  (let loop ((ix 3))
    (if (zero? ix)
      (print "*** ERROR *** Giving up ***")
      (handle-exceptions exn
        (begin
          (print "*** ERROR *** Retrying ***")
          (sleep ix)
          (loop (- ix 1)))
        (fun)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MediaFire - Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mediafire-api . parameters)
  (let* ((cmd  (string-append 
                  "set -f; nice -n 19 curl -s -g --limit-rate 3M -m 120 "
                  "\"https://www.mediafire.com/api/1.3/"
                  (apply string-append parameters)
                  "&token_version=1"
                  "&session_token=" (session) "\""))
         (result (call-with-input-pipe cmd (cut read-string #f <>))))
    (print "CMD: " cmd)
    (print "RES: " result)
    result))

(define (mediafire-api-call result-wanted . parameters)
  (xml-value
    (mediafire-api (apply string-append parameters))
    result-wanted))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MediaFire - Session Token
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *signature* 
  (sha1-of-string 
    (string-append *email* *password* *app-id* *api-key*))) 

(define *session* "0")
(define *session-time* 0)

(define (session)
  (let ((age (- (current-seconds) *session-time*)))
   (when (> age 400)
     (set! *session-time* (current-seconds))
     (set! *session* (mediafire-api-call "session_token"
                       "user/get_session_token.php"
                       "?signature=" *signature*
                       "&email=" *email*
                       "&password=" *password*
                       "&application_key=" *api-key*
                       "&application_id=" *app-id*))))
  *session*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MediaFire - Folders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-mediafire-root-folder folder)
  (let* ((ix          (string-index-right folder #\/))
         (name        (if ix (string-copy folder (+ 1 ix)) folder))
         (has-parent  (and ix (> ix 0)))
         (parent-part (if has-parent 
                        (string-append 
                          "&parent_key=" 
                          (create-mediafire-root-folder 
                            (string-copy folder 0 ix))) 
                        "")))
    (mediafire-api-call "folder_key"
      "folder/create.php?"
      "foldername=" (string-url-encode name)
      "&action_on_duplicate=replace"
      parent-part)))

(define (create-mediafire-folder key path)
  (print "[dir   ] "  path)
  (if key
    (mediafire-api-call "folder_key"
      "folder/create.php?"
      "foldername=" (string-url-encode (pathname-strip-directory path))
      "&action_on_duplicate=replace"
      "&parent_key=" key)
    (create-mediafire-root-folder path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MediaFire - Files
;;;   Upload Process Workers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file-up-to-date? file-path file-size file-sig)
  (let* ((file-name    (pathname-strip-directory file-path)) 
         (parent-path  (pathname-directory file-path))
         (response     (mediafire-api "upload/check.php?" 
                          "filename=" (string-url-encode file-name)
                          "&resumable=yes"
                          "&size="    (number->string file-size)
                          "&hash="    file-sig
                          "&path="    (string-url-encode parent-path)))
         (file-exists  (xml-value? response "file_exists" "yes"))
         (same-hash    (xml-value? response "different_hash" "no"))
         (needs-update (not (and file-exists same-hash)))
         (nr-of-units  (xml-number-value response "number_of_units"))
         (unit-size    (xml-number-value response "unit_size")))
    (values needs-update nr-of-units unit-size)))

(define (simple-upload-file file-path file-size)
  (let* ((target-name (pathname-strip-directory file-path)) 
         (target-path (pathname-directory file-path))
         (cmd   (string-append 
                  "set -f; nice -n 19 curl -s -g --limit-rate 3M -m 120" 
                  " -F \"file=@\\\"" 
                  (string-sh-escape file-path)
                  "\\\";filename=\\\"" 
                  (string-sh-escape target-name) "\\\"\" "
                  "\"https://www.mediafire.com/api/1.3/"
                  "upload/simple.php?"
                  "path=" (string-url-encode target-path)
                  "&action_on_duplicate=replace"
                  "&session_token=" (session) "\""))
         (result (call-with-input-pipe cmd (cut read-string #f <>)))
         (ok     (xml-value-in? result "result" '("0" "Success"))))
;   (print "CMD: " cmd)
;   (print "RES: " result)
    (unless ok
      (sleep 1)
      (print "[re-upl] " file-path)
      (simple-upload-file file-path file-size))))

(define (upload-file file-path file-size file-sig nr-of-units unit-size)
  (let ((target-name (pathname-strip-directory file-path)) 
        (target-path (pathname-directory file-path)))
    (let unit-loop ((ix 0))
      (when (< ix nr-of-units)
        (print "[upload: " (number->string (+ 1 ix)) 
               "/" (number->string nr-of-units) "] " file-path)
        (let* ((dd-part   (string-append 
                            "set -f; dd if=\"" 
                            (string-sh-escape file-path)
                            "\" bs=" (number->string unit-size)
                            " count=1 skip=" (number->string ix)
                            " 2> /dev/null "))
               (hash-cmd (string-append 
                           dd-part
                           "| openssl dgst -r -sha256"))
               (hash (string-copy (call-with-input-pipe 
                                    hash-cmd (cut read-string #f <>)) 
                                  0 64))
               (cmd   (string-append 
                        dd-part
                        " | curl -s -g --limit-rate 3M -m 60"
                        " -H \"x-unit-hash:" hash "\""
                        " -H \"x-filehash:" file-sig "\""
                        " -H \"x-filesize:" 
                        (number->string file-size) "\""
                        " -H \"x-unit-id:" (number->string ix) "\""
                        " -H \"x-unit-size:" 
                        (number->string
                          (if (= ix (- nr-of-units 1))
                            (let ((ps (modulo file-size unit-size)))
                              (if (zero? ps) unit-size ps))
                            unit-size)) 
                        "\" -F file=@- -F filename=\"" 
                        (string-sh-escape target-name) "\" "
                        "\"https://www.mediafire.com/api/1.3/"
                        "upload/resumable.php?"
                        "path=" (string-url-encode target-path)
                        "&resumable=yes"
                        "&action_on_duplicate=replace"
                        "&session_token=" (session) "\""))
               (result (call-with-input-pipe 
                         cmd (cut read-string #f <>)))
               (ok     (xml-value-in? result "result" '("0" "Success"))))
;   (print "CMD: " cmd)
;   (print "RES: " result))
;         (if ok
            (unit-loop (+ 1 ix))
;           (unit-loop ix))
          )))))

(define *upload-workers* '())

(define (nr-of-active-upload-workers)
  (let ((ready-workers (filter char-ready? *upload-workers*)))
    (for-each 
      (lambda (p)
        (process-wait (string->number(read-line p)))
        (close-input-port p))
      ready-workers)
    (set! *upload-workers*
          (lset-difference eq? *upload-workers* ready-workers))
    (length *upload-workers*)))

(define (wait-for-nr-of-active-upload-workers-to-become nr)
  (when (> (nr-of-active-upload-workers) nr)
    (thread-sleep! 0.1)
    (wait-for-nr-of-active-upload-workers-to-become nr)))

(define (file-upload-worker file-path)
  (try-it (lambda ()
    (print "[check ] " file-path)
    (let ((file-sig  (sha256-of-file file-path))
          (file-size (file-size file-path)))
      (let-values (((needs-update nr-of-units unit-size)
                      (file-up-to-date? file-path file-size file-sig)))
        (when needs-update
          (print "[upload] " file-path)
          (if (< file-size 10485760)
            (simple-upload-file file-path file-size)
            (upload-file file-path file-size file-sig
                         nr-of-units unit-size))))))))

(define (create-file-upload-worker file-path)
  (wait-for-nr-of-active-upload-workers-to-become 2)
  (thread-sleep! (/ (random 200) 100))
  (let-values (((b-pipe t-pipe) (create-pipe)))
    (process-fork (lambda ()
      (let ((o (open-output-file* t-pipe)))
        (file-close b-pipe)
        (file-upload-worker file-path)
        (write-line (number->string (current-process-id))  o)
        (close-output-port o)
        (exit))))
    (file-close t-pipe)
    (set! *upload-workers* 
      (cons (open-input-file* b-pipe) *upload-workers*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collect Directories and Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (upload-directory path #!optional key)
  (if (directory? path)
    (let ((dir-key  (create-mediafire-folder key path)))
      (for-each 
        (lambda (name) 
          (upload-directory (string-append path "/" name) dir-key))
        (sort (directory path #t) string<) ))
    (unless (zero? (file-size path)) ;; MediaFire bug
      (create-file-upload-worker path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *dirs* 
  '(
;   "/home/wiffel/Pictures"
;   "/home/wiffel/Music"
    "/home/wiffel/kashbah/sss"))

(define (main)
  (for-each upload-directory (sort *dirs* string<))
  (wait-for-nr-of-active-upload-workers-to-become 0)
  (quit))

(main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

