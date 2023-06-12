#lang sicp


;-----------Q1-------------

;a)

;(A 1 5)
;(A (- 1 1) (A 1 (- 5 1)))
;(A 0 (A 1 4))
;(A 0 (A (- 1 1) (A 1 (- 4 1))))
;(A 0 (A 0 (A 1 3)))
;(A 0 (A 0 (A (- 1 1) (A 1 (- 3 1)))))
;(A 0 (A 0 (A 0 (A 1 2))))
;(A 0 (A 0 (A 0 (A (- 1 1) (A 1 (- 2 1)))))
;(A 0 (A 0 (A 0 (A 0 (A 1 1)))))
;(A 0 (A 0 (A 0 (A 0 2))))
;(A 0 (A 0 (A 0 4)))
;(A 0 (A 0 8))
;(A 0 16)
;32

;b)

;(A 2 3)
;(A (- 2 1) (A 2 (- 3 1)))
;(A 1 (A 2 2))
;(A 1 (A (- 2 1) (A 2 (- 2 1))))
;(A 1 (A 1 (A 2 1)))
;(A 1 (A 1 2))
;(A 1 (A (- 1 1) (A 1 (- 2 1))))
;(A 1 (A 0 (A 1 1)))
;(A 1 (A 0 2))
;(A 1 4)
;(A (-1 1) (A 1 (- 4 1)))
;(A 0 (A 1 3))
;(A 0 (A (- 1 1) (A 1 ( - 3 1))))
;(A 0 (A 0 (A 1 2)))
;(A 0 (A 0 (A (-1 1) (A 1 (- 2 1)))))
;(A 0 (A 0 (A 0 (A 1 1))))
;(A 0 (A 0 (A 0 2)))
;(A 0 (A 0 4))
;(A 0 8)
;16


;------------Q2-----------------
;a) Recursive

(define (f-recursive n)
 (cond ((< n 3) n)
       (else (+ (f-recursive (- n 1)) (* 2 (f-recursive (- n 2))) (* 3 (f-recursive (- n 3)))))))

(f-recursive 5)

;b) Iterative

(define (f-iterative n)
  (define (f-loop n1 n2 n3 nth)
    (if (= n nth)
        n1
        (f-loop (+ n1 (* 2 n2) (* 3 n3)) n1 n2 (+ 1 nth))))
  (if (< n 3)
      n
      (f-loop 2 1 0 2)))

(f-iterative 5)


;-------------Q3---------------

(define (double x)
  (+ x x))

(define (half x)
  (/ x 2))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mult (double a) (half b)))
        (else (+ a (fast-mult a (- b 1))))))

(fast-mult 4 2)

;------------Q4----------------

(define (fast-mult-i x y)
  (fast-mult-iter x y 0))

(define (fast-mult-iter a b c)
  (cond ((= b 0)
         c)
        ((even? b)
         (fast-mult-iter (double a) (half b) c))
        (else
         (fast-mult-iter a (- b 1) (+ c a)))))

(fast-mult-i 4 2)

;-----------Q5----------------

;The code generates very large numbers.
;In many language, operation like multiplication can work only on fixnum numbers.
;These large numbers are called Arbitrary-precision arithmetic or bignum arithmetic.
;On top of taking large memory space, computations on bignum is much slower to perform than on fixnum.
;While on hardware, operations like multiplication and remainder on fixnum can be seen as O(1),
;on bignum they have a much slower complexity of O(Nlog(N)log(log(N))
;By contrast the original algorithm doesn’t try to fully compute the exp before computing the remainder,
;but break down the problem intod smaller numbers of roughly the same size.
;Alyssa P. Hacker version of expmod:
;Gives a much larger intermediate result, which could require more memory than available on the computer
;This large intermediate result requires the use of special algorithm for multiplications and remainders that are much slower than computation on smaller fixnum numbers
