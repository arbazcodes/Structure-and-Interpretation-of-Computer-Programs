#lang sicp

;---------------Q1-----------------

(define (same-parity x . y)
     (define (collect rem list)
         (cond ((null? list) nil)
               ((= (remainder (car list) 2) rem) (cons (car list) (collect rem (cdr list))))
               (else (collect rem (cdr list)))))
     (cons x (collect (remainder x 2) y)))

(same-parity 1 2 3 4 5 6 7 8 9 10)

;---------------Q2-----------------

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs)) nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
(accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
(map (lambda (x) (dot-product x v)) m))


(define s (list (list 1 2 3 4) (list 4 5 6 6)
(list 6 7 8 9)))

(define (transpose mat)
(accumulate-n cons nil mat))

(matrix-*-vector s (list 1 2 3 4))
(transpose s)

(define (matrix-*-matrix m n)
(let ((cols (transpose n)))
(map (lambda (v) (matrix-*-vector cols v)) m)))

(matrix-*-matrix s s)


;----------------Q4--------------------

(define (enumerate-interval low high)
(if (> low high)
nil
(cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))(cons (car sequence)(filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (flatmap proc seq)
(accumulate append nil (map proc seq)))

(define (ordered-triples n)
  
  (define (sum-equal? pair)
  (if (= (+ (car pair) (cadr pair) (caddr pair)) n) #t #f))

  (filter sum-equal? (accumulate append nil (map (lambda (k)
                                    (accumulate append nil (map (lambda (i)
                                                                  (map (lambda (j)
                                                                         (list k i j))
                                                                       (enumerate-interval 1 n)))
                                                                (enumerate-interval 1 n))))
                                  (enumerate-interval 1 n)))))

 
       

(ordered-triples 6)


;----------------------Q5-------------------------

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
(and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))
  )
) 

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))
  )
)

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
(cond ((=number? a1 0) a2)
((=number? a2 0) a1)
((and (number? a1) (number? a2))
(+ a1 a2))
(else (list '+ a1 a2))))

(define (make-product m1 m2)
(cond ((or (=number? m1 0) (=number? m2 0)) 0)
((=number? m1 1) m2)
((=number? m2 1) m1)
((and (number? m1) (number? m2)) (* m1 m2))
(else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv '(* x y (+ x 3)) 'x)


;----------------Q6----------------
;a) The partial-tree procedure takes a list of elements elts and an integer n as arguments.
;If n is 0, an empty tree is returned.
;Otherwise, it recursively constructs a balanced binary tree by calculating the size of the left subtree,
;constructing the left subtree recursively, and then using the remaining elements to construct the right subtree.
;The constructed left and right subtrees are combined with the root element to form a balanced binary tree,
;and the remaining elements in the original list after constructing the tree are returned as the cdr of the cons pair.
;The list->tree procedure uses partial-tree to convert a list of elements into a balanced binary tree.


;b) Assuming all other calls are of constant order we can conclude that the overall time complexity is of the order O(n).