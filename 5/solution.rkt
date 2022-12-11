#lang racket

(require "../file-io.rkt")

(define puzzle-lines (read-puzzle-lines "puzzle-input"))

(define (create-cargo-list lst)
  (map string-split lst))

;; Split the puzzle lines into two sections.
;; The first is the puzzle area and the secoon is the moves
(define (convert-lines lines [cargo empty])
  (if (non-empty-string? (car lines))
      (convert-lines (cdr lines) (cons (car lines) cargo))
      (cons (reverse cargo) (cdr lines))))

;; Given a mutable vector cargo area is will take the initial rows
;; and use them to populate the cargo-area
(define (populate-cargo-area rows cargo-area)
  (for ([row rows])
    (for
        ([i (in-inclusive-range 0 (length row))]
         [column row])
      (cond [(non-empty-string? column) (vector-set! cargo-area i (cons column (vector-ref cargo-area i)))]))))

;; Given lines for the cargo area generate a vector that contains a list for each area;
;; The top of the area is the first element in the list.
(define (convert-cargo-lines lines)
  (let* ([rows (drop-right (map (lambda (line)
                                  (map string-trim  (regexp-match* #px"(^|\\s{1}).{3}" line)))
                                lines)
                           1)]
         [rows-backward (reverse rows)]
         [cargo-area (make-vector (length (car rows)) empty)])
    (populate-cargo-area rows-backward cargo-area)
    cargo-area))

(define (remove-words-trim-and-split x)
  (map string->number (string-split (string-trim (string-replace x  #px"\\D+" " ")))))

;; Contains the puzzle input used to solve the problem. Up until and including this point, the entire
;; program has been dedicated to parsing the input file :(
(define puzzle-input
  (let ([puzzle-input (convert-lines puzzle-lines)])
    (cons (convert-cargo-lines (first puzzle-input))
          (map remove-words-trim-and-split (rest puzzle-input)))))


(define (move-single cargo-area from to)
  (let* ([from-index (sub1 from)]
         [to-index (sub1 to)]
         [from-items (vector-ref cargo-area from-index)]
         [from-item (car from-items)]
         [from-rest (cdr from-items)]
         [to-items (vector-ref cargo-area to-index)])
    (vector-set! cargo-area to-index (cons from-item to-items))
    (vector-set! cargo-area from-index from-rest)
    cargo-area))

(define (move cargo-area amount from to)
  (if (<= amount 0)
      cargo-area
      (move (move-single cargo-area from to) (sub1 amount) from to)))

(define solution-a (vector-copy (first puzzle-input)))

(for
    ([instruction (rest puzzle-input)])
  (move solution-a (car instruction) (cadr instruction) (caddr instruction)))

(printf "solution a: ~s\n" (string-replace (string-join (map car (vector->list solution-a)))  #px"\\[|\\]" ""))

(define (move-9001 cargo-area amount from to)
  (let*-values ([(from-index) (sub1 from)]
                [(to-index) (sub1 to)]
                [(from-items) (vector-ref cargo-area from-index)]
                [(from-a from-b) (split-at from-items amount)]
                [(to-items) (vector-ref cargo-area to-index)])
    (vector-set! cargo-area to-index (append from-a to-items))
    (vector-set! cargo-area from-index from-b)
    cargo-area))

(define solution-b (vector-copy (first puzzle-input)))

(for
    ([instruction (rest puzzle-input)])
  (move-9001 solution-b (car instruction) (cadr instruction) (caddr instruction)))

(printf "solution b: ~s\n" (string-replace (string-join (map car (vector->list solution-b)))  #px"\\[|\\]" ""))
