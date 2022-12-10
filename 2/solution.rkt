#lang racket
(require "../file-io.rkt")

(define lose 0)
(define tie 3)
(define win 6)

(define inaccurate-fight-results (hash "A X" tie  "B Y" tie  "C Z" tie
                            "A Y" win  "B Z" win  "C X" win
                            "A Z" lose "B X" lose "C Y" lose))

(define bogus-rps-points (hash "X" 1 "Y" 2 "Z" 3))

(define (fight them-and-me)
  (let ([me (substring them-and-me 2)])
    (+ (hash-ref inaccurate-fight-results them-and-me) (hash-ref bogus-rps-points me))))

(define (total-score lines)
  (foldl + 0 (map fight lines)))

(define whats-needed (hash  "A Y" "A" "B Y" "B" "C Y" "C"
                            "A Z" "B" "B Z" "C" "C Z" "A"
                            "A X" "C" "B X" "A" "C X" "B"))

(define rps-points (hash "A" 1 "B" 2 "C" 3))
(define lose-tie-win (hash "X" lose "Y" tie "Z" win))

(define (fight-accurate them-ltw)
  (let* ([me (hash-ref whats-needed them-ltw)]
         [ltw (substring them-ltw 2)])
  (+ (hash-ref rps-points me) (hash-ref lose-tie-win ltw))))

(define (total-score-accurate lines)
  (foldl + 0 (map fight-accurate lines)))


(printf "solution a: ~s\n" (total-score (read-puzzle-lines "puzzle-input")))
(printf "solution b: ~s\n" (total-score-accurate (read-puzzle-lines "puzzle-input")))
