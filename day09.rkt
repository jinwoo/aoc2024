#lang racket/base

(require racket/vector)

(define (read-input)
  (let ([line (with-input-from-file "./input/day09.txt" #:mode 'text
                (lambda ()
                  (read-line)))]
        [base (char->integer #\0)])
    (map (lambda (c)
           (- (char->integer c) base))
         (string->list line))))

(define (decode ns)
  (let loop ([acc '()] [ns ns] [what 'block] [id 0])
    (if (null? ns)
        (list->vector (reverse acc))
        (let* ([block? (eq? what 'block)]
               [x (if block? id #f)])
          (loop (for/fold ([acc acc])
                          ([_ (in-range (car ns))])
                  (cons x acc))
                (cdr ns)
                (if block? 'space 'block)
                (if block? (+ id 1) id))))))

(define (last-block-index-from disk i)
  (let loop ([i i])
    (and (>= i 0)
         (if (vector-ref disk i)
             i
             (loop (- i 1))))))

(define (first-space-index-from disk i)
  (let ([len (vector-length disk)])
    (let loop ([i i])
      (and (< i len)
           (if (vector-ref disk i)
               (loop (+ i 1))
               i)))))

(define (compact disk)
  (define (compacted? disk i j)
    (> (first-space-index-from disk i)
       (last-block-index-from disk j)))
  (let loop ([i 0] [j (- (vector-length disk) 1)])
    (if (compacted? disk i j)
        (checksum disk)
        (let ([i (first-space-index-from disk i)]
              [j (last-block-index-from disk j)])
          (vector-set! disk i (vector-ref disk j))
          (vector-set! disk j #f)
          (loop i j)))))

(define (checksum disk)
  (let ([len (vector-length disk)])
    (let loop ([acc 0] [i 0])
      (let ([x (vector-ref disk i)])
        (if (or (>= i len) (not x))
            acc
            (loop (+ acc (* i x)) (+ i 1)))))))

(define (part1 disk)
  (compact disk))

(define (solve)
  (let ([disk (decode (read-input))])
    (display (part1 (vector-copy disk))) (newline)))

(solve)
