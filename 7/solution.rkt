#lang racket

(require "../file-io.rkt")

(define raw-input (read-puzzle-lines "puzzle-input"))

(define (natural-string? x)
  (regexp-match #px"^\\d+$" x))

(define (clean-input in)
  (string-replace in #px"^\\$ " ""))

(define (add-to-total total max-size candidate)
  (if (<= candidate max-size) (+ total candidate) total))

(define total-space 70000000)
(define upgrade-space 30000000)

(define (find-dir-size-to-delete  directories)
  (let* ([sorted-dirs (sort directories <)]
         [total-used (last sorted-dirs)]
         [free-space (- total-space total-used)]
         [needed-space (- upgrade-space free-space)])
    (findf (curry <= needed-space) sorted-dirs)))

(define (fs-size input max-size [total 0] [directory-path empty] [all-dirs empty])
  (if (empty? input)
      (cons (+ total (apply + (filter (curry >= max-size) directory-path))) (find-dir-size-to-delete (append all-dirs directory-path)))
      (let* ([cleaned-in (clean-input (car input))]
             [command (string-split cleaned-in)]
             [next-input (cdr input)])
        (cond [(or (equal? "ls" cleaned-in) (equal? "dir" (car command)))
               (fs-size next-input max-size total directory-path all-dirs)]
              [(natural-string? (car command))
               (fs-size next-input max-size total (map (curry + (string->number (car command))) directory-path) all-dirs)]
              [(equal? "cd .." cleaned-in)
               (fs-size next-input max-size (add-to-total total max-size (car directory-path)) (cdr directory-path) (cons (car directory-path) all-dirs))]
              [else (fs-size next-input max-size total (cons 0 directory-path) all-dirs)]))))

(fs-size raw-input 100000)
