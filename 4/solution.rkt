#lang racket

(require "../file-io.rkt")

(define lines (read-puzzle-lines "puzzle-input"))
(define line-groups (map (lambda (x)
                                   (map string->number (flatten (map (curryr string-split "-") x))))
                                 (map (curryr string-split ",") lines)))

(define (a-within-b? a-start a-end b-start b-end)
  (and (>= a-start b-start)
       (<= a-start b-end)
       (>= a-end b-start)
       (<= a-end b-end)))

(define (overlaps-completely? group)
  (let ([elf-a-start (car group)]
        [elf-a-end (cadr group)]
        [elf-b-start (caddr group)]
        [elf-b-end (cadddr group)])
  (or (a-within-b? elf-a-start elf-a-end elf-b-start elf-b-end)
      (a-within-b? elf-b-start elf-b-end elf-a-start elf-a-end))))

(define (a-overlaps-b? a-start a-end b-start b-end)
  (or (and (>= a-start b-start)
           (<= a-start b-end))
      (and (>= a-end b-start)
           (<= a-end b-end))))

(define (overlaps-some? group)
  (let ([elf-a-start (car group)]
        [elf-a-end (cadr group)]
        [elf-b-start (caddr group)]
        [elf-b-end (cadddr group)])
  (or (a-overlaps-b? elf-a-start elf-a-end elf-b-start elf-b-end)
      (a-overlaps-b? elf-b-start elf-b-end elf-a-start elf-a-end))))

(printf "solution a: ~s\n" (length (filter overlaps-completely? line-groups)))
(printf "solution b: ~s\n" (length (filter overlaps-some? line-groups)))
