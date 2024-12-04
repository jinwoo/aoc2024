#lang racket

(define (read-input)
  (with-input-from-file "./input/day03.txt" #:mode 'text
    (lambda () (string-join (sequence->list (in-lines)) "\n"))))

(define (calc-in-range str (start 0) (end #f))
  (let ([pairs (regexp-match* #px"mul\\((\\d+),(\\d+)\\)" str
                              start end
                              #:match-select cdr)])
    (foldl (lambda (pair acc)
             (let ([n1 (string->number (car pair))]
                   [n2 (string->number (cadr pair))])
               (+ acc (* n1 n2))))
           0 pairs)))

(define (part1 str)
  (calc-in-range str))

(define (part2 str)
  (let loop ([enabled #t] [start 0] [acc 0])
    (if enabled
        (match (regexp-match-positions #px"don't\\(\\)" str start)
          [(list* (cons dont-start _) _)
           (loop #f
                 dont-start
                 (+ acc (calc-in-range str start dont-start)))]
          [#f (+ acc (calc-in-range str start))])
        (match (regexp-match-positions #px"do\\(\\)" str start)
          [(list* (cons _ do-end) _)
           (loop #t do-end acc)]
          [#f acc]))))

(define (solve)
  (let ([input (read-input)])
    (display (part1 input)) (newline)
    (display (part2 input)) (newline)))

(solve)
