#lang sicp
;Arbaz Khan
;2020076
;Assignment#3

;-------------Q1---------------

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;------------Q2---------------

(define (inc n)
  (+ n 1))

(define (double f)
  (lambda (x)
    (f (f x))))

(((double (double double)) inc) 5)

;------------Q3----------------

(define (reverse-l lst)
   (if (null? lst) nil
       (append (reverse-l (cdr lst)) (list (car lst)))))

(reverse-l (list 1 2 3 4))

;-----------Q4-----------------

(define (square n)
  (* n n))

(define (tree-map fn tree)
   (map (lambda (node)
          (if (list? node) (tree-map fn node)
              (fn node))) tree))

(define (square-tree tree) (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;-----------Q5----------------

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
           (append rest(map(lambda (x) (cons (car s) x))rest)))))


(subsets (list 1 2 3))