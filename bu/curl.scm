(module curl (curl-init
              curl-cleanup
              curl-perform
              curl-set-verbose
              curl-set-url
              curl-set-write-file
              curl-close-write-file
              curl-post-file
              curl-formfree)
  (import scheme chicken)
  (import foreign)

(foreign-declare "#include \"curl.h\"")

(define curl-init
  (foreign-lambda c-pointer "curl_easy_init"))

(define curl-cleanup
  (foreign-lambda void "curl_easy_cleanup" c-pointer))

(define curl-perform
  (foreign-lambda int "curl_easy_perform" c-pointer))

(define curl-set-url
  (foreign-lambda* void ((c-pointer curl) (c-string url))
    "curl_easy_setopt(curl, CURLOPT_URL, url);"))

(define curl-set-write-file
  (foreign-lambda* c-pointer ((c-pointer curl) (c-string filename))
    "FILE* file = fopen(filename, \"wb\");
     curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void*)file);
     C_return(file);"))

(define curl-close-write-file
  (foreign-lambda int "fclose" c-pointer))

(define curl-set-verbose
  (foreign-lambda* void ((c-pointer curl))
    "curl_easy_setopt(curl, CURLOPT_VERBOSE, 1L);"))

(define curl-post-file
  (foreign-lambda* c-pointer 
          ((c-pointer curl) (c-string filename) (c-string file_path))
    "struct curl_httppost* post = NULL;
     struct curl_httppost* last = NULL;

     curl_formadd(&post, &last,
             CURLFORM_COPYNAME, filename,
             CURLFORM_FILENAME, filename,
             CURLFORM_CONTENTTYPE, \"application/octet-stream\",
             CURLFORM_FILE, file_path,
             CURLFORM_END);
     curl_easy_setopt(curl, CURLOPT_HTTPPOST, post);
     C_return(post);"))

(define curl-formfree
  (foreign-lambda void "curl_formfree" c-pointer))

)
