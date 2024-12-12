#lang racket/base

(require racket/match
         racket/sequence)

(struct guard
  (pos direction))

(define (read-input)
  (with-input-from-file "./input/day06.txt" #:mode 'text
    (lambda ()
      (let* ([rows (map string->list (sequence->list (in-lines)))]
             [grid (make-hash)]
             [gd #f]
             [num-rows (length rows)]
             [num-cols (length (car rows))])
        (do ([rows rows (cdr rows)]
             [r 0 (+ r 1)])
            ((null? rows) (list grid gd num-rows num-cols))
          (do ([row (car rows) (cdr row)]
               [c 0 (+ c 1)])
              ((null? row))
            (let ([pos (cons r c)])
              (case (car row)
                [(#\#) (hash-set! grid pos 'obj)]
                [(#\^) (set! gd (guard pos 'up))]
                [(#\v) (set! gd (guard pos 'down))]
                [(#\<) (set! gd (guard pos 'left))]
                [(#\>) (set! gd (guard pos 'right))]))))))))

(define (grid-turn-right g)
  (case (guard-direction g)
    [(up) (guard (guard-pos g) 'right)]
    [(right) (guard (guard-pos g) 'down)]
    [(down) (guard (guard-pos g) 'left)]
    [(left) (guard (guard-pos g) 'up)]))

(define (move-guard grid g)
  (match-let* ([(cons row col) (guard-pos g)]
               [pos (case (guard-direction g)
                      [(up) (cons (- row 1) col)]
                      [(down) (cons (+ row 1) col)]
                      [(left) (cons row (- col 1))]
                      [(right) (cons row (+ col 1))])]
               [val (hash-ref grid pos #f)])
    (if (and val (eq? val 'obj))
        (grid-turn-right g)
        (guard pos (guard-direction g)))))

(define (count-visited grid)
  (foldl (lambda (v acc)
           (if (eq? v 'visited) (+ acc 1) acc))
         0
         (hash-values grid)))

(define (part1 grid g num-rows num-cols)
  (let loop ([g g])
    (match-let ([(cons row col) (guard-pos g)])
      (if (or (negative? row)
              (negative? col)
              (>= row num-rows)
              (>= col num-cols))
          (count-visited grid)
          (begin
            (hash-set! grid (guard-pos g) 'visited)
            (loop (move-guard grid g)))))))

(define (stuck-in-loop? grid g num-rows num-cols)
  (let loop ([g g])
    (match-let ([(and (cons row col) pos) (guard-pos g)])
      (cond
        [(or (negative? row)
             (negative? col)
             (>= row num-rows)
             (>= col num-cols))
         #f]
        [(memq (guard-direction g) (hash-ref grid pos '()))]
        [else
         (hash-update! grid (guard-pos g)
                       (lambda (val) (cons (guard-direction g) val))
                       '())
         (loop (move-guard grid g))]))))

(define (part2 grid g num-rows num-cols)
  (let ([guard-pos (guard-pos g)])
    (for/fold ([acc 0])
              ([row (in-range num-rows)])
      (for/fold ([acc acc])
                ([col (in-range num-cols)])
        (let ([pos (cons row col)])
          (if (and (not (equal? (cons row col) guard-pos))
                   (let ([grid (hash-copy grid)])
                     (hash-set! grid pos 'obj)
                     (stuck-in-loop? grid g num-rows num-cols)))
              (+ acc 1)
              acc))))))

(define (solve)
  (match-let ([(list grid g num-rows num-cols) (read-input)])
    (display (part1 (hash-copy grid) g num-rows num-cols)) (newline)
    (display (part2 (hash-copy grid) g num-rows num-cols)) (newline)))

(solve)
