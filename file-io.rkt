#lang racket

(require racket/contract)

(define (read-puzzle-lines puzzle-input)
  (call-with-input-file puzzle-input read-lines))

(define (read-lines in [lines '()])
  (let ([line (read-line in)])
    (if (eof-object? line) (reverse lines) (read-lines in (cons line lines)))))

(provide read-puzzle-lines)
