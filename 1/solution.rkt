#lang racket
(require "../file-io.rkt")
  
(define (find-max-a cal-list)
  (car (sort (find-max-b cal-list) >)))

(define (remove-smallest lst)
    (cdr (sort lst <)))

(define (find-max-b cal-list [current-elf 1] [current-max '(0 0 0)] [current-sum 0])
  (if (empty? cal-list)
      current-max
      (let* ([current (string-trim (car cal-list))]
             [stick-with-elf  (non-empty-string? current)]
             [the-rest (cdr cal-list)])
        (if stick-with-elf
            (find-max-b the-rest current-elf current-max (+ (string->number current) current-sum))
            (find-max-b the-rest (add1 current-elf) (remove-smallest (cons current-sum current-max)) 0)))))

(printf "solution a: ~s\n" (find-max-a (read-puzzle-lines "puzzle-input")))
(printf "solution b: ~s\n" (foldl + 0 (find-max-b (read-puzzle-lines "puzzle-input"))))
