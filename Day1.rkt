#lang racket

(define (extract-nums str)
  (letrec ([start 0]
           [first-found false]
           [second-found false]
           [end (- (string-length str) 1)]
           [loop (lambda ()
                   (when (and (< start end) (or (not first-found) (not second-found)))
                     (if (not (char-numeric? (string-ref str start)))
                         (set! start (+ 1 start))
                         (set! first-found true))
                     (if (not (char-numeric? (string-ref str end)))
                         (set! end (- end 1))
                         (set! second-found true))
                     (loop)))])
    (begin
      (loop)
      (string->number (list->string
                       (list (string-ref str start) (string-ref str end)))))))

(define (solve file-name)
  (begin 
    (let ([sum 0])
      (with-input-from-file file-name
        (lambda ()
          (for ([l (in-lines)])
            (set! sum (+ sum (extract-nums l))))))
      (list sum))))
  

(solve "input1.txt")
