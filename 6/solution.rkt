#lang racket

(require "../file-io.rkt")
(require rackunit)

(define (remove-to-length lst lng)
  (let ([lst-length (length lst)])
    (if (<= lst-length lng)
        lst
        (drop-right lst (- lst-length lng)))))

(check-equal? '(1 2 3 4 5) (remove-to-length '(1 2 3 4 5) 10))
(check-equal? '(1 2 3 4 5) (remove-to-length '(1 2 3 4 5) 5))
(check-equal? '(1 2 3) (remove-to-length '(1 2 3 4 5) 3))

(define (no-duplicates lst)
  (not (check-duplicates lst)))

(check-true (no-duplicates '(1 2 3)))
(check-false (no-duplicates '(3 2 3)))

(define (find-marker ip marker-length [most-recent empty] [count 0])
  (let ([next-char (read-char ip)]
        [most-recent-length (length most-recent)]) 
    (cond [(and (= marker-length most-recent-length) (no-duplicates most-recent))  count]
          [(eof-object? next-char) -1]
          [else (find-marker ip marker-length (remove-to-length (cons next-char most-recent) marker-length) (add1 count))])))

(define puzzle-file "puzzle-input")

(printf "solution a: ~s\n" (call-with-input-file puzzle-file (curryr find-marker 4)))
(printf "solution b: ~s\n" (call-with-input-file puzzle-file (curryr find-marker 14)))
