#lang sicp

;;;;;;;;;;;Q1;;;;;;;;;;
(define (is_even x y z)

  (cond ((and (even? x) (even? y)) #t)
        ((and (even? y) (even? z)) #t)
        ((and (even? x) (even? z)) #t)
        (else #f)
  ))
        
(is_even 2 4 6)

;;;;;;;;;;Q2;;;;;;;;;;

(define (cal_tax salary)
  (cond ((< salary 20000) 0)
        ((and (>= salary 20000) (< salary 50000)) (* 0.1 salary))
        ((and (>= salary 50000) (< salary 500000)) (* 0.2 salary))
        ((>= salary 500000) (* 0.3 salary))
  ))

(cal_tax 60000)

;;;;;;;;;;;Q3;;;;;;;;

(define (mod a b)
  (cond ((< a b) a)
        (else (mod (- a b) b))
  ))     
  

(define (GCD x y)
  (cond ((= y 0) x)
        (else (GCD y (mod x y)))
  ))

(GCD 21 7)

;;;;;;;;;Q4;;;;;;;;;

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (good-enoughc? guess x)
(< (abs (- (cube guess) x)) 0.001))

(define (averagec x y)
(/ (+ x y) 3))

(define (improvec guess x)
(averagec (/ x (square guess)) (* 2 guess)))

(define (cube-iter guess x)
  (if (good-enoughc? guess x)
          guess
          (cube-iter (improvec guess x) x)))

(cube-iter 1.0 8)


;;;;;;;;;;;;Q5;;;;;;;;;;;;;;

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x) guess (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 4)

;If we replace the if with new-if, the sqrt-iter function will enter an infinite loop.
;This is because new-if evaluates both the then-clause and else-clause, leading to an infinite recursion of the sqrt-iter function.
;Therefore, we get the error: Interactions disabled; out of memory.