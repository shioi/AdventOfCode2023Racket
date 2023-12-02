#lang racket

; (RGB) -> ( 0 12 12)

(define RED 12)
(define GREEN 13)
(define BLUE 14)

(define (is-possible? value)
  (and (<= (mcar value) RED) (<= (mcar (mcdr value)) BLUE) (<= (mcdr (mcdr value)) GREEN)))

(define (convert-to-list str)
  (let* ([splitted (string-split str ",")]
         [res (mcons 0 (mcons 0 0))])
    (begin
      (for ([ele splitted])
                (let* ([color-number (string-split ele)]
                       [color-times (string->number (car color-number))])
                  (cond
                    [(string-ci=? (cadr color-number) "red") (set-mcar! res color-times)]
                    [(string-ci=? (cadr color-number) "blue") (set-mcar! (mcdr res) color-times)]
                    [(string-ci=? (cadr color-number) "green") (set-mcdr! (mcdr res) color-times)]))))
    res))


(define (read-and-result filename)
  (let ([s 0])
    (with-input-from-file filename
      (thunk
       (for ([l (in-lines)])
         (letrec ([splitted-value (string-split l ":")]
                  [gameid (string->number (cadr (string-split (car splitted-value))))]
                  [sets (string-split (cadr splitted-value) ";")]
                  [loop (lambda (lst)
                          (if (not (empty? lst))
                              (and (is-possible? (convert-to-list (car lst))) (loop (cdr lst)))
                              #t))])
           (if (loop sets)
               (begin
                 (print l)
                 (println gameid)
                 (set! s (+ s gameid)))
              #f)))))
    s))
          
          
(read-and-result "input2.txt")
