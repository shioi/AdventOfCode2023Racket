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
      sum)))

;;part two
(define number-hash
  (for/hash ([i
              '(("one" #\1) ("two" #\2) ("three" #\3) ("four" #\4) ("five" #\5) ("six" #\6) ("seven" #\7) ("eight" #\8 ) ("nine" #\9))])
    (values (car i) (cdr i))))


(define (convert-to-number str)
  (letrec ([i 0]
           [res-list '()]
           [len (string-length str)]
           [loop (lambda ()
                   (when (< i len)
                     (if (char-numeric? (string-ref str i))
                         (set! res-list (cons (string-ref str i) res-list))
                         (letrec ([j (+ i 1)]
                               [inner-loop (lambda ()
                                             (begin
                                             (when (< j len)
                                               (let ([val (substring str i (+ 1 j))])
                                                 (if (hash-has-key? number-hash val)
                                                     (set! res-list (cons (car (hash-ref number-hash val)) res-list))
                                                     (begin
                                                       (set! j (+ j 1))
                                                       (inner-loop)))))))])
                           (inner-loop)))
                     (set! i (+ i 1))
                     (loop)))])
    (begin
      (loop)
      (list->string (reverse res-list)))))

(define (solve-part2 file-name)
  (begin 
    (let ([sum 0])
      (begin
        (with-input-from-file file-name
          (lambda ()
            (for ([l (in-lines)])
              (let ([val (convert-to-number l)])
                (set! sum (+ sum
                (string->number (list->string (list (string-ref val 0) (string-ref val ( - (string-length val) 1)))))))
                (list sum))))))
      sum)))

(solve "input1.txt")

