(use srfi-1 srfi-13 posix)

;;;;;;;;;;

(define (create-worker)
  (let ((pid (process-fork)))
    (when (= 0 pid) ; child
     (let loop ((ix 1))
        (print (call-with-input-pipe "flock -s 3" 
                                     (cut read-string #f <>)))
        (let ((cmd (call-with-input-pipe 
                     "head -n 1 fif"
                     (cut read-string #f <>))))
          (print (call-with-input-pipe "flock -u 3" 
                                       (cut read-string #f <>)))
          (print "CMD: " cmd)
          (let ((res (call-with-input-pipe 
                      cmd
                      (cut read-string #f <>))))
            (print "RES: " res)))
        (loop (+ 1 ix)))
      (quit))
    ; parent
    ))

(create-fifo "fif")
(define *fd*fifo* (file-open "fif" open/rdwr))

(print *fd*fifo*)

(define (schedule-work cmd)
  (file-write *fd*fifo* cmd)
  (file-write *fd*fifo* "\n"))

(print (call-with-input-pipe "flock 3" (cut read-string #f <>)))
(schedule-work "ls -la")
(schedule-work "ls -la")
(schedule-work "for ix in 1 2 3 4 5; do echo $ix; sleep 1; done")
(schedule-work "for ix in 10 20 30 40; do echo $ix; sleep 2; done")
(schedule-work "ls -la")
(print (call-with-input-pipe "flock -u 3" (cut read-string #f <>)))

(create-worker)
(create-worker)



(define ip (open-input-file* fd))
(define op (open-output-file* fd))



(schedule-work "\n")

;(close-output-port p)
