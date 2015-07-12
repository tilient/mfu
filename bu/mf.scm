(use srfi-13 medea simple-sha1 sha2 message-digest posix)
(load "curl.so")
(import curl)

;;; MediaFire ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *app-id*    "44501")
(define *api-key*   "dia6jjylyxo5esfj61an33wtwxj7d8npddo2noff")
(define *email*     "wiffel@tilient.net")
(define *password*  "ttT1l1ent")

(define (create-mediafire-session)
  (let ((curl (curl-init))
        (tmp-filename "/run/shm/mf01")
        (signature (string->sha1sum
                     (string-append *email* *password* 
                                    *app-id* *api-key*))))
    (curl-set-url curl
      (string-append "https://www.mediafire.com/api/1.3/"
                     "user/get_session_token.php?"
                     "signature=" signature
                     "&email=" *email*
                     "&password=" *password*
                     "&application_key=" *api-key*
                     "&application_id=" *app-id*
                     "&token_version=1&response_format=json"))
    (let ((file (curl-set-write-file curl tmp-filename)))
      (curl-set-verbose curl)
      (curl-perform curl)
      (curl-close-write-file file)
      (curl-cleanup curl)
      (let ((response (with-input-from-file tmp-filename read-json)))
        (cdr (assq 'session_token (cdar response)))))))

(define (create-mediafire-folder session folder)
  (let* ((ix     (string-index-right folder #\/))
         (name   (if ix (string-copy folder (+ 1 ix)) folder))
         (parent (if ix 
                   (string-append "&parent_key="
                                  (create-mediafire-folder 
                                     session 
                                     (string-copy folder 0 ix)))
                   ""))
         (url (string-append
                 "http://www.mediafire.com/api/1.3/"
                 "folder/create.php?"
                 "foldername=" name
                 parent
                 "&session_token=" session
                 "&action_on_duplicate=replace"
                 "&token_version=1&response_format=json"))
         (tmp-filename "/run/shm/mf01")
         (curl (curl-init)))
    (let ((file (curl-set-write-file curl tmp-filename)))
      (curl-set-verbose curl)
      (curl-set-url curl url)
      (curl-perform curl)
      (curl-close-write-file file)
      (curl-cleanup curl)
      (let ((response (with-input-from-file tmp-filename read-json)))
        (cdr (assq 'folder_key (cdar response)))))))

(define (main)
  (let* ((session    (create-mediafire-session))
         (folder-key (create-mediafire-folder session "aa/bb/cc")))
    (print "--done--")))


;  ;;; check file ;;;
;  
;  (define *filepath*  
;          "/home/wiffel/Public/pictures/NewOrleans/IMG_0080.png")
;    
;  (define *url* (string-append
;                   "https://www.mediafire.com/api/1.3/"
;                   "upload/check.php?" 
;                   "filename=sss.png"
;                   "&size=" (number->string (file-size *filepath*))
;                   "&hash=" (message-digest-file 
;                              (sha256-primitive) *filepath*)
;                   "&path=/fff"
;                   "&session_token=" *session-token*
;                   "&response_format=json"))
;  
;  (define *curl* (curl-init))
;  (curl-set-url *curl* *url*)
;  (define *file* (curl-set-write-file *curl* "/run/shm/mf01"))
;  (curl-perform *curl*)
;  (curl-close-write-file *file*)
;  (curl-cleanup *curl*)
;  (define *res* (with-input-from-file "/run/shm/mf01" read-json))
;  (print *res*)
;  (define *is-old* (equal? "no" (cdr (assq 'different_hash
;                                     (cdr (assq 'response *res*))))))
;  (print *is-old*)
;  
;  (unless *is-old*
;  
;    ;;; upload file ;;;
;    
;    (define *url* (string-append
;                     "https://www.mediafire.com/api/1.3/"
;                     "upload/simple.php?"
;                     "path=/fff"
;                     "&session_token=" *session-token*
;                     "&action_on_duplicate=replace"
;                     "&response_format=json"))
;    
;    (define *curl* (curl-init))
;  ; (curl-set-verbose *curl*)
;    (curl-set-url *curl* *url*)
;    (define *post* (curl-post-file *curl* "sss.png" *filepath*))
;    (curl-perform *curl*)
;    (curl-formfree *post*)
;    (curl-cleanup *curl*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(main)
(quit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


