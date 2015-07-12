(use srfi-1 srfi-13 posix)

;;;;;;;;;;


(define (create-worker)
  (let ((pid (process-fork)))
    (when (= 0 pid) ; child
      (let ((ip (open-input-file* (file-open "fif" open/read))))
        (let loop ((ix 1))
          (if (char-ready? ip)
            (let ((cmd (read-line ip)))
              (print "CMD: " cmd)
              (let loop ((ix 0))
                (let ((res (call-with-input-pipe 
                             cmd
                             (cut read-string #f <>))))
                  (print "RES: " res)
                  (let ((ok (handle-exceptions exn #f
                              (member (string-between 
                                        response "<result>" "</result>")
                                      '("0" "Success")))))
                    (unless (or ok (> ix 2))
                      (loop (+ 1 ix)))))))
            (sleep 1))
          (loop (+ 1 ix))))
      (quit))))

(create-worker)
(create-worker)
(create-worker)

(handle-exceptions exn #f
  (create-fifo "fif"))

(define *fd*fifo* (file-open "fif" open/rdwr))
(define *ip*fifo* (open-input-file* *fd*fifo*))
(define *op*fifo* (open-output-file* *fd*fifo*))

(define (schedule-work cmd)
  (let loop ((ix 1))
    (when (char-ready? *ip*fifo*)
      (print ".")
      (sleep 1)
      (loop (+ 1 ix))))
  (write-line cmd *op*fifo*)
  (flush-output cmd *op*fifo*)
  (sleep 1)
  (print "***"))

