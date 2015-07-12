(use srfi-1 srfi-13 srfi-18 posix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *app-id*   "44501")
(define *api-key*  "dia6jjylyxo5esfj61an33wtwxj7d8npddo2noff")
(define *email*    "wiffel@tilient.net")
(define *password* "ttT1l1ent")

(define *mfu-dirs* "mfu_dirs.txt")
(define *mfu-dir-keys* "mfu_dir_keys.txt")
(define *mfu-files* "mfu_files.txt")
(define *mfu-file-sigs* "mfu_file_sigs.txt")
(define *mfu-files-up* "mfu_files_up.txt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-between str start-str end-str)
  (let* ((start-pos (+ (string-length start-str)
                       (string-contains-ci str start-str)))
         (end-pos (string-contains-ci str end-str start-pos)))
    (string-copy str start-pos end-pos)))

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
      (begin
        (print "*** ERROR *** Giving up ***")
        #f)
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
  (let* ((cmd   (string-append 
                   "set -f; nice -n 19 curl -s -g "
                   "\"https://www.mediafire.com/api/1.3/"
                   (apply string-append parameters)
                   "&token_version=1"
                   "&session_token=" (session) "\""))
         (result (call-with-input-pipe cmd (cut read-string #f <>))))
;   (print "CMD: " cmd)
;   (print "RES: " result)
    result))

(define (mediafire-api-call result-wanted . parameters)
  (string-between 
    (mediafire-api (apply string-append parameters))
    (string-append "<" result-wanted ">")
    (string-append "</" result-wanted ">")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MediaFire - Session Token
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *signature* 
  (sha1-of-string (string-append *email* *password* *app-id* *api-key*))) 

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

(define (create-mediafire-folder folder)
  (let* ((ix     (string-index-right folder #\/))
         (name   (if ix (string-copy folder (+ 1 ix)) folder))
         (parent (if (and ix (> ix 0))
                   (string-append 
                     "&parent_key="
                     (create-mediafire-folder (string-copy folder 0 ix)))
                   "")))
    (mediafire-api-call "folder_key"
      "folder/create.php?"
      "foldername=" (string-url-encode name)
      "&action_on_duplicate=replace"
      parent)))

(define (create-mediafire-sub-folder key folder)
  (mediafire-api-call "folder_key"
    "folder/create.php?"
    "foldername=" (string-url-encode folder)
    "&action_on_duplicate=replace"
    "&parent_key=" key))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MediaFire - Files
;;;   Upload Process Workers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *done-marker* "--- DONE --- asdf2fs6d4fad4fg ---")

(define (file-up-to-date? file-path file-size file-sig)
  (let* ((file-name   (pathname-strip-directory file-path)) 
         (parent-path (pathname-directory file-path))
         (response    (mediafire-api
                          "upload/check.php?" 
                          "filename=" (string-url-encode file-name)
                          "&size="    file-size
                          "&hash="    file-sig
                          "&path="    (string-url-encode parent-path))))
    (and (equal? "yes" (string-between response 
                         "<file_exists>" "</file_exists>"))
         (equal? "no" (string-between response 
                         "<different_hash>" "</different_hash>")))))

(define (upload-file file-path file-size)
  (let* ((target-name (pathname-strip-directory file-path)) 
         (target-path (pathname-directory file-path))
         (time-limit  (+ (quotient (string->number file-size) 200000) 30))
         (cmd   (string-append 
                   "set -f; nice -n 19 curl -s -g --limit-rate "
                   "3M -m " (number->string time-limit) 
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
         (ok     (member (string-between result "<result>" "</result>")
                         '("0" "Success"))))
;   (print "CMD: " cmd)
;   (print "RES: " result)
    (unless ok
      (sleep 1)
      (upload-file file-path file-size))))

(define *upload-workers* '())

(define (nr-of-active-upload-workers)
  (let ((ready-workers (filter char-ready? *upload-workers*)))
    (for-each read-line ready-workers)
    (for-each close-input-port ready-workers)
    (set! *upload-workers*
          (lset-difference eq? *upload-workers* ready-workers))
    (length *upload-workers*)))

(define (wait-for-nr-of-active-upload-workers-to-become nr)
  (when (> (nr-of-active-upload-workers) nr)
    (thread-sleep! 0.1)
    (wait-for-nr-of-active-upload-workers-to-become nr)))

(define (create-upload-worker file-path file-size file-sig)
  (wait-for-nr-of-active-upload-workers-to-become 2)
  (let-values (((b-pipe t-pipe) (create-pipe))
               ((pid) (process-fork)))
    (if (= 0 pid) ; child
      (let ((o (open-output-file* t-pipe)))
        (file-close b-pipe)
        (try-it (lambda ()
          (unless (file-up-to-date? file-path file-size file-sig)
            (upload-file file-path file-size))))
        (write-line *done-marker* o)
        (quit))
      (set! *upload-workers* 
           (cons (open-input-file* b-pipe) *upload-workers*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collect Directories and Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scan-directories path od of)
  (if (directory? path)
    (begin
      (write-line path od)
      (for-each 
        (lambda (name) 
          (scan-directories (string-append path "/" name) od of))
        (directory path #t)))
    (write-line path of)))

(define (collect-directories-and-files path)
  (let ((od (open-output-file (string-append path "/" *mfu-dirs*)))
        (of (open-output-file (string-append path "/" *mfu-files*))))
    (scan-directories path od of)
    (close-output-port od)
    (close-output-port of)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collect File Signatures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (collect-file-signatures path)
  (let ((o-s (open-output-file (string-append path "/" *mfu-file-sigs*)))
        (i-f (open-input-file (string-append path "/" *mfu-files*))))
    (for-each
      (lambda (filename)
        (let ((filesize (file-size filename)))
          (unless (zero? filesize) ;; MediaFire bug
            (print "[sha256] " filename)
            (write-line filename o-s)
            (write-line (number->string filesize) o-s)
            (write-line (sha256-of-file filename) o-s))))
      (read-lines i-f))
    (close-input-port i-f)
    (close-output-port o-s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create Directories @ MediaFire
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-root-dir o name)        
  (print  "[Create] "  name)
  (let ((key (create-mediafire-folder name)))
    (write-line name o)
    (write-line key o)
    key))

(define (create-dir o key name fullpath)        
  (print "[create] "  fullpath)
  (let ((key (create-mediafire-sub-folder key name)))
    (write-line fullpath o)
    (write-line key o)
    key))

(define (create-dirs i o parent key line)
  (if (eof-object? line)
    line
    (let-values (((base name _) (decompose-pathname line)))
      (if (string= base parent)
        (create-dirs 
          i o parent key 
          (create-dirs 
            i o line (create-dir o key name line) (read-line i)))
        line))))

(define (create-mediafire-directories path)
  (let ((i (open-input-file (string-append path "/" *mfu-dirs*)))
        (o (open-output-file (string-append path "/" *mfu-dir-keys*))))
    (let loop ((root-path (read-line i)))
      (unless (eof-object? root-path)
        (loop (create-dirs 
                i o root-path 
                (create-root-dir o root-path) (read-line i)))))
    (close-input-port i)
    (close-output-port o)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check & Upload Files @ MediaFire
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-and-upload-mediafire-files path)
  (let ((i (open-input-file (string-append path "/" *mfu-file-sigs*))))
    (let loop ((file-path (read-line i)))
      (unless (eof-object? file-path)
        (let ((file-size (read-line i))
              (file-sig  (read-line i)))
          (print "[upload] " file-path)
          (create-upload-worker file-path file-size file-sig))
        (loop (read-line i)))
      (close-input-port i)
      (wait-for-nr-of-active-upload-workers-to-become 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *path* "/home/wiffel/dev/mediafire")

(define (main)
  (collect-directories-and-files *path*)
  (collect-file-signatures *path*)
  (create-mediafire-directories *path*)
  (check-and-upload-mediafire-files *path*)
  (quit))

(main)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

