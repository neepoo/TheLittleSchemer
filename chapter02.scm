#lang scheme
; Do It, Do It Again, and Again, and Again...
(define l '(Jack Sprat could eat no chicken fat))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))  ;(null? l) if l is the null list
  )
)
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f)
     )
   )
  )

(lat? l)

(define l2 '(bacon (and eggs)))
(lat? l2)
(null? '())

; member like in in Python

(define member?
  (lambda (a lat)
  (cond
    ((null? lat) #f)
    (else
     (or (eq? a (car lat))
         (member? a (cdr lat))
           ))
   )
 )
)
