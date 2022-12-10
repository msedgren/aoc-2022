#lang racket
(require "../file-io.rkt")


(define lines (read-puzzle-lines "puzzle-input"))

(define split-lines
  (map (lambda (in)
         (let* ([len (string-length in)]
                [half (/ len 2)])
           (cons (substring in 0 half) (substring in half))))
       lines))

(define (intersect-split-line in)
  (let ([comp-a-items (list->set (string->list (car in)))]
        [comp-b-items (list->set (string->list (cdr in)))])
    (set-intersect comp-a-items comp-b-items)))

(define duplicates (map car (map set->list (map intersect-split-line split-lines))))

(define (calc-item-value in)
  (let ([numeric-raw (char->integer in)])
    (if (>= numeric-raw 97)
        (- numeric-raw 96)
        (- numeric-raw 38))))

(printf "solution a: ~s\n" (apply + (map calc-item-value duplicates)))


(define (create-groups in [groups (list (list))])
  (cond [(empty? in) groups]
        [(= 3 (length (car groups))) (create-groups (cdr in) (cons (list (car in)) groups))]
        [else (create-groups (cdr in) (cons (cons (car in) (car groups)) (cdr groups)))]))

(define grouped-lines (create-groups lines))

(define (intersect-group in)
  (let ([a-items (list->set (string->list (car in)))]
        [b-items (list->set (string->list (cadr in)))]
        [c-items (list->set (string->list (caddr in)))])
    (set-intersect a-items b-items c-items)))

(define groups (map car (map set->list (map intersect-group grouped-lines))))

(printf "solution b: ~s\n" (apply + (map calc-item-value groups)))
