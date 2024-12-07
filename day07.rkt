#lang racket

(define (read-input)
  (with-input-from-file "./input/day07.txt" #:mode 'text
    (lambda ()
      (for/list ([line (in-lines)])
        (match-let ([(list result nums) (string-split line ": ")])
          (let ([nums (string-split nums)])
            (cons (string->number result)
                  (map string->number nums))))))))

(define (calc nums ops)
  (foldl (lambda (num op acc)
           (op acc num))
         (car nums)
         (cdr nums)
         ops))

(define (matches? result nums ops)
  (= (calc nums ops) result))

(define (ops-combinations-2 n)
  (let loop ([n n])
    (if (zero? n)
        '(())
        (append-map (lambda (ops)
                      (list (cons + ops) (cons * ops)))
                    (loop (- n 1))))))

(define (sum-matching-results input comb-fn)
  (foldl (lambda (line acc)
           (let ([result (car line)]
                 [nums (cdr line)])
             (if (findf (lambda (ops)
                          (matches? result nums ops))
                        (comb-fn (- (length nums) 1)))
                 (+ acc result)
                 acc)))
         0
         input))

(define (part1 input)
  (sum-matching-results input ops-combinations-2))

(define (num-digits n)
  (if (zero? n)
      1
      (let loop ([n n] [acc 0])
        (if (zero? n)
            acc
            (loop (quotient n 10) (+ acc 1))))))

(define (concat n1 n2)
  (let ([multiplier (expt 10 (num-digits n2))])
    (+ (* n1 multiplier) n2)))

(define (ops-combinations-3 n)
  (let loop ([n n])
    (if (zero? n)
        '(())
        (append-map (lambda (ops)
                      (list (cons + ops) (cons * ops) (cons concat ops)))
                    (loop (- n 1))))))

(define (part2 input)
  (sum-matching-results input ops-combinations-3))

(define (solve)
  (let ([input (read-input)])
    (display (part1 input)) (newline)
    (display (part2 input)) (newline)))

(solve)
