#lang racket

(define (string->vector s)
  (list->vector (string->list s)))

(define (read-input)
  (with-input-from-file "./input/day04.txt" #:mode 'text
    (lambda ()
      (let loop ([acc '()])
        (let ([line (read-line)])
          (if (eof-object? line)
              (list->vector (reverse acc))
              (loop (cons (string->vector line) acc))))))))

(define (extract-right grid len row col)
  (let ([r (vector-ref grid row)])
    (and (<= col (- (vector-length r) len))
         (apply string
                (for/list ([c (range len)])
                  (vector-ref r (+ col c)))))))

(define (extract-left grid len row col)
  (let ([r (vector-ref grid row)])
    (and (>= col (- len 1))
         (apply string
                (for/list ([c (range len)])
                  (vector-ref r (- col c)))))))

(define (extract-down grid len row col)
  (and (<= row (- (vector-length grid) len))
       (apply string
              (for/list ([r (range len)])
                (vector-ref (vector-ref grid (+ row r))
                            col)))))

(define (extract-up grid len row col)
  (and (>= row (- len 1))
       (apply string
              (for/list ([r (range len)])
                (vector-ref (vector-ref grid (- row r))
                            col)))))

(define (extract-down-right grid len row col)
  (and (<= row (- (vector-length grid) len))
       (<= col (- (vector-length (vector-ref grid row)) len))
       (apply string
              (for/list ([i (range len)])
                (vector-ref (vector-ref grid (+ row i))
                            (+ col i))))))

(define (extract-down-left grid len row col)
  (and (<= row (- (vector-length grid) len))
       (>= col (- len 1))
       (apply string
              (for/list ([i (range len)])
                (vector-ref (vector-ref grid (+ row i))
                            (- col i))))))

(define (extract-up-right grid len row col)
  (and (>= row (- len 1))
       (<= col (- (vector-length (vector-ref grid row)) len))
       (apply string
              (for/list ([i (range len)])
                (vector-ref (vector-ref grid (- row i))
                            (+ col i))))))

(define (extract-up-left grid len row col)
  (and (>= row (- len 1))
       (>= col (- len 1))
       (apply string
              (for/list ([i (range len)])
                (vector-ref (vector-ref grid (- row i))
                            (- col i))))))

(define (find-xmas grid)
  (define xmas "XMAS")
  (define len (string-length xmas))
  (define (count-match s)
    (if (and s (string=? s xmas)) 1 0))
  (let ([count 0])
    (do ([row 0 (+ row 1)])
        ((>= row (vector-length grid)) count)
      (do ([col 0 (+ col 1)])
          ((>= col (vector-length (vector-ref grid row))))
        (let ([r  (count-match (extract-right grid len row col))]
              [l  (count-match (extract-left grid len row col))]
              [d  (count-match (extract-up grid len row col))]
              [u  (count-match (extract-down grid len row col))]
              [dr (count-match (extract-down-right grid len row col))]
              [dl (count-match (extract-down-left grid len row col))]
              [ur (count-match (extract-up-right grid len row col))]
              [ul (count-match (extract-up-left grid len row col))])
          (set! count (+ count r l d u dr dl ur ul)))))))

(define (part1 grid)
  (find-xmas grid))

(define (find-x-mas grid)
  (define (match-shape row col)
    (and (<= row (- (vector-length grid) 3))
         (<= col (- (vector-length (vector-ref grid row)) 3))
         (let ([s1 (extract-right grid 3 row       col)]
               [s2 (extract-right grid 3 (+ row 1) col)]
               [s3 (extract-right grid 3 (+ row 2) col)])
           (or (and (regexp-match #rx"M.S" s1)
                    (regexp-match #rx".A." s2)
                    (regexp-match #rx"M.S" s3))
               (and (regexp-match #rx"S.M" s1)
                    (regexp-match #rx".A." s2)
                    (regexp-match #rx"S.M" s3))
               (and (regexp-match #rx"M.M" s1)
                    (regexp-match #rx".A." s2)
                    (regexp-match #rx"S.S" s3))
               (and (regexp-match #rx"S.S" s1)
                    (regexp-match #rx".A." s2)
                    (regexp-match #rx"M.M" s3))))))
  (let ([count 0])
    (do ([row 0 (+ row 1)])
        ((>= row (vector-length grid)) count)
      (do ([col 0 (+ col 1)])
          ((>= col (vector-length (vector-ref grid row))))
        (when (match-shape row col)
          (set! count (+ count 1)))))))

(define (part2 input)
  (find-x-mas input))

(define (solve)
  (let ([input (read-input)])
    (display (part1 input)) (newline)
    (display (part2 input)) (newline)))

(solve)
