#lang racket/base

(require racket/list
         racket/match
         racket/set)

(define (read-input)
  (with-input-from-file "./input/day08.txt" #:mode 'text
    (lambda ()
      (let ([grid (make-hasheqv)])
        (let loop ([y 0] [max-x #f])
          (let ([line (read-line)])
            (if (eof-object? line)
                (list grid max-x y)
                (begin
                  (for ([c line]
                        [x (in-naturals)])
                    (unless (char=? c #\.)
                      (hash-update! grid c
                                    (lambda (v)
                                      (cons (cons x y) v))
                                    '())))
                  (loop (+ y 1) (or max-x (string-length line)))))))))))

(define (add-antinodes-1 grid freq max-x max-y antinodes)
  (let ([positions (hash-ref grid freq)])
    (let loop ([positions positions])
      (unless (null? positions)
        (match-let ([(cons x1 y1) (car positions)])
          (let inner ([rest-positions (cdr positions)])
            (unless (null? rest-positions)
              (match-let ([(cons x2 y2) (car rest-positions)])
                (define (add x y)
                  (when (and (<= 0 x) (< x max-x)
                             (<= 0 y) (< y max-y))
                    (set-add! antinodes (cons x y))))
                (add (+ x1 (- x1 x2))
                     (+ y1 (- y1 y2)))
                (add (+ x2 (- x2 x1))
                     (+ y2 (- y2 y1)))
                (inner (cdr rest-positions)))))
          (loop (cdr positions)))))))

(define (part1 grid max-x max-y)
  (let ([freqs (remove-duplicates (hash-keys grid))]
        [antinodes (mutable-set)])
    (for ([freq freqs])
      (add-antinodes-1 grid freq max-x max-y antinodes))
    (set-count antinodes)))

(define (add-antinodes-2 grid freq max-x max-y antinodes)
  (let ([positions (hash-ref grid freq)])
    (let loop ([positions positions])
      (unless (null? positions)
        (match-let ([(cons x1 y1) (car positions)])
          (let inner ([rest-positions (cdr positions)])
            (unless (null? rest-positions)
              (match-let ([(cons x2 y2) (car rest-positions)])
                (define (add x y)
                  (and (<= 0 x) (< x max-x)
                       (<= 0 y) (< y max-y)
                       (set-add! antinodes (cons x y))))
                (let ([dx (- x1 x2)]
                      [dy (- y1 y2)])
                  (let add-loop-1 ([mult 0])
                    (when (add (+ x1 (* mult dx))
                               (+ y1 (* mult dy)))
                      (add-loop-1 (+ mult 1)))))
                (let ([dx (- x2 x1)]
                      [dy (- y2 y1)])
                  (let add-loop-2 ([mult 0])
                    (when (add (+ x2 (* mult dx))
                               (+ y2 (* mult dy)))
                      (add-loop-2 (+ mult 1)))))
                (inner (cdr rest-positions)))))
          (loop (cdr positions)))))))

(define (part2 grid max-x max-y)
  (let ([freqs (remove-duplicates (hash-keys grid))]
        [antinodes (mutable-set)])
    (for ([freq freqs])
      (add-antinodes-2 grid freq max-x max-y antinodes))
    (set-count antinodes)))

(define (solve)
  (match-let ([(list grid max-x max-y) (read-input)])
    (display (part1 grid max-x max-y)) (newline)
    (display (part2 grid max-x max-y)) (newline)))

(solve)
