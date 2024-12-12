#lang racket/base

(require racket/list)

(define (read-input)
  (with-input-from-file "./input/day01.txt" #:mode 'text
    (lambda ()
      (let loop ([lefts '()] [rights '()])
        (let* ([left (read)]
               [right (read)])
          (if (eof-object? left)
              (list (reverse lefts) (reverse rights))
              (loop (cons left lefts) (cons right rights))))))))

(define (part1 lefts rights)
  (let ([lefts (sort lefts <)]
        [rights (sort rights <)])
    (foldl (lambda (left right acc)
             (+ acc (abs (- left right))))
           0
           lefts rights)))

(define (part2 lefts rights)
  (foldl (lambda (left acc)
           (+ acc
              (* left
                 (count (lambda (right) (= left right)) rights))))
         0
         lefts))

(define (solve)
  (let ([input (read-input)])
    (display (apply part1 input)) (newline)
    (display (apply part2 input)) (newline)))

(solve)
