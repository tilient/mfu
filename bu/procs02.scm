(use srfi-1 srfi-13 posix)

(define *done-marker* "--- DONE --- asdf2fs6d4fad4fg ---")

(define (create-worker)
  (let-values 
    (((b-pipe-a t-pipe-a) (create-pipe))
     ((b-pipe-b t-pipe-b) (create-pipe))
     ((pid)               (process-fork)))
    (when (= 0 pid)
      (file-close b-pipe-b)
      (file-close t-pipe-a)
      (let ((i (open-input-file*  b-pipe-a))
            (o (open-output-file* t-pipe-b)))
        (write-line *done-marker* o)
        (flush-output o)
        (let loop ((cmd (read-line i )))
          (write-line
            (call-with-input-pipe cmd (cut read-string #f <>))
            o)
          (write-line *done-marker* o)
          (flush-output o)
          (loop (read-line i)))))
    (file-close b-pipe-a)
    (file-close t-pipe-b)
    (vector (open-input-file* b-pipe-b)
            (open-output-file* t-pipe-a)
            (lambda (str) #t)
            "")))

(define *workers* (list (create-worker) 
                        (create-worker)
                        (create-worker)))

(define (stop-workers)
  (free-worker)
  (free-worker)
  (free-worker))

(define (get-work-result i) 
  (let loop ((str  (read-line i))
             (strs ""))
    (if (string= *done-marker* str)
      strs
      (loop (read-line i) (string-append strs "\n" str)))))

(define (free-worker)
  (let ((free (filter 
                (lambda (w) (char-ready? (vector-ref w 0))) 
                *workers*)))
    (if (null? free)
      (begin
        (sleep 1)
        (free-worker))
      (let* ((w   (first free))
             (wo  (vector-ref w 0))
             (wi  (vector-ref w 1))
             (fun (vector-ref w 2))
             (cmd (vector-ref w 3)))
        (if (fun (get-work-result wo))
          w
          (begin
            (print "RETRY: " cmd)
            (write-line cmd wi)
            (flush-output wi)
            (free-worker)))))))
  
(define (schedule-work cmd fun)
  (let* ((w  (free-worker))
         (wo (vector-ref w 1)))
    (vector-set! w 2 fun)
    (vector-set! w 3 cmd)
    (write-line cmd wo)
    (flush-output wo)))

;;;;;;;;;;

(schedule-work "for ix in 1 2 3 4; do echo $ix; sleep 1; done"
  (lambda (str) (print ">>>" str "<<<") #t))

(schedule-work "ls -la" 
  (lambda (str) (print ">>>" str "<<<") #t))

(schedule-work "for ix in 10 20 30 40; do echo $ix; sleep 2; done"
  (lambda (str) (print ">>>" str "<<<") #t))

(stop-workers)
