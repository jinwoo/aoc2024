#lang racket

(define (parse-rule s)
  (match (string-split s "|")
    [(list p1 p2)
     (cons (string->number p1) (string->number p2))]
    [else (error "invalid rule" s)]))

(define (parse-pages s)
  (map string->number (string-split s ",")))

(define (read-input)
  (with-input-from-file "./input/day05.txt" #:mode 'text
    (lambda ()
      (let loop ([part 'rules] [rules '()] [pages-list '()])
        (let ([line (read-line)])
          (cond
            [(eof-object? line)
             (cons (reverse rules) (reverse pages-list))]
            [(and (eq? part 'rules) (string=? line ""))
             (loop 'pages-list rules pages-list)]
            [(eq? part 'rules)
             (let ([rule (parse-rule line)])
               (loop part (cons rule rules) pages-list))]
            [(eq? part 'pages-list)
             (let ([pages (parse-pages line)])
               (loop part rules (cons pages pages-list)))]
            [else (error "invalid state" part)]))))))

(define (correct-page-order? rules pages)
  (andmap (lambda (rule)
            (let ([pos1 (index-of pages (car rule))]
                  [pos2 (index-of pages (cdr rule))])
              (or (not (and pos1 pos2))
                  (< pos1 pos2))))
          rules))

(define (part1 rules pages-list)
  (foldl (lambda (pages acc)
           (if (correct-page-order? rules pages)
               (+ acc
                  (list-ref pages (quotient (length pages) 2)))
               acc))
         0
         pages-list))

(define (correct-page-order rules pages)
  (sort pages (lambda (a b)
                (correct-page-order? rules (list a b)))))

(define (part2 rules pages-list)
  (foldl (lambda (pages acc)
           (if (correct-page-order? rules pages)
               acc
               (+ acc
                  (list-ref (correct-page-order rules pages)
                            (quotient (length pages) 2)))))
         0
         pages-list))

(define (solve)
  (match (read-input)
    [(cons rules pages-list)
     (display (part1 rules pages-list)) (newline)
     (display (part2 rules pages-list)) (newline)]))

(solve)
