#lang racket/base

(require racket/port)

(define (read-input)
  (with-input-from-file "./input/day02.txt" #:mode 'text
    (lambda ()
      (let loop ([acc '()])
	(let ([line (read-line)])
	  (if (eof-object? line)
	      (reverse acc)
	      (let ([report (with-input-from-string line
			      (lambda ()
				(let inner-loop ([inner-acc '()])
				  (let ([n (read)])
				    (if (eof-object? n)
					(reverse inner-acc)
					(inner-loop (cons n inner-acc)))))))])
		(loop (cons report acc)))))))))

(define (safe-report? report)
  (let* ([a1 (car report)]
	 [a2 (cadr report)]
	 [range (cond [(< a1 a2) '(1 . 3)]
		      [(> a1 a2) '(-3 . -1)]
		      [else #f])])
    (and range
	 (let loop ([xs report])
	   (or (null? xs)
	       (null? (cdr xs))
	       (let ([x1 (car xs)]
		     [x2 (cadr xs)])
		 (and (<= (car range) (- x2 x1) (cdr range))
		      (loop (cdr xs)))))))))

(define (part1 data)
  (foldl (lambda (report acc)
           (if (safe-report? report)
               (+ acc 1)
               acc))
         0
         data))

(define (remove-nth xs n)
  (let loop ([acc '()] [xs xs] [i 0])
    (cond [(null? xs) (reverse acc)]
	  [(= i n) (loop acc
			 (cdr xs)
			 (+ i 1))]
	  [else (loop (cons (car xs) acc)
		      (cdr xs)
		      (+ i 1))])))

(define (loose-safe-report? report)
  (or (safe-report? report)
      (let ([n (length report)])
	(let loop ([i 0])
	  (and (< i n)
	       (or (safe-report? (remove-nth report i))
		   (loop (+ i 1))))))))

(define (part2 data)
  (foldl (lambda (report acc)
           (if (loose-safe-report? report)
               (+ acc 1)
               acc))
         0
         data))

(define (solve)
  (let ([data (read-input)])
    (display (part1 data)) (newline)
    (display (part2 data)) (newline)))

(solve)
