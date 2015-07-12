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
          (print "CMD: " cmd)
          (call-with-input-pipe 
            cmd
            (cut read-string #f <>))
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

