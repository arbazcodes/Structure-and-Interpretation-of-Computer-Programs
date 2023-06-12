#lang sicp

( define (last items)
   (if (null? (cdr items))
       items
       (last (cdr items))))

(define (reverse list)
  (define (reverse-iter rs ls)
      (if (null? ls)
          rs
          (reverse-iter (cons (car ls) rs) (cdr ls))
      )
  )
  (reverse-iter nil list)
)

(last (list 1 2 3 4 5))
(reverse (list 1 2 3 4 5))




(map (lambda (x) (* x x)) (list 1 2 3))



(define (sqt tree)
  (if (null? tree) nil
      (if (not (pair? tree)) (* tree tree) (cons (sqt (car tree)) (sqt (cdr tree))))))


(sqt
(list 1
(list 2 (list 3 4) 5)
(list 6 7)))

(define (sq term tree)
      (map (lambda (subtree)
             (if (pair? subtree) (sq term subtree)
                 (term subtree))) tree))

(sq (lambda (x) (* x x x))
(list 1
(list 2 (list 3 4) 5)
(list 6 7)))

