#lang scheme

(define member?
  (lambda (a lat)
    (cond
       ( (null? lat) #f)
       ( else (or ( eq? (car lat) a) (member? a (cdr lat)) ) )
    )
  )
)

(define set?
  (lambda (lat)
    (cond
      ( (null? lat) #t )
      ( else
        (cond
          ( (member? (car lat) (cdr lat)) #f)
          ( else (set? (cdr lat)))
        )
      )
    )
  )
)

(define makeset
  (lambda (lat)
    (cond
      ( (null? lat) '())
      ( (member? (car lat) (cdr lat)) (makeset (cdr lat)))
      ( else (cons (car lat) (makeset (cdr lat))))
      )))

(makeset '(apple peach pear peach plum apple lemon peach)) ; (pear plum apple lemon peach)

(define multirember
  (lambda (a lat)
    (cond
      ( (null? lat) '())
      ( (eq? (car lat) a) (multirember a (cdr lat)))
      ( else (cons (car lat) (multirember a (cdr lat))))
      )
    )
  )

(define makeset?
  (lambda (lat)
    (cond
      ( (null? lat) '())
      ( else (cons (car lat) (makeset? (multirember (car lat) (cdr lat)))))
    )))

(makeset? '(apple peach pear peach plum apple lemon peach)) ; (pear plum apple lemon peach)

(define subset?
  (lambda (s1 s2)
    (cond
      ((null? s1) #t)
      ((member? (car s1) s2) (subset? (cdr s1) s2))
      (else #f))))

(subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))
(subset? '(5 chicken wings) '(5 hamburgers 
2 pieces fried chicken and 
light duckling wings))

(define eqset?
  (lambda (s1 s2)
    (cond
      ( (subset? s1 s2) (subset? s2 s1))
      ( else #f)
    )
  )
)


(define intersect?
  (lambda (s1 s2)
    (cond
      ((null?) #f)
      (else
       (cond
         ((member? (car s1) s2) #t)
         (else (intersect? (cdr s1) s2)))))))

(define intersect
  (lambda (s1 s2)
    (cond
      ((null? s1) '())
      ((member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
      (else (intersect (cdr s1) s2)))))

(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese)) ; (add macaroni)

(define union
  (lambda (s1 s2)
    (cond
      ( (null? s1) s2)
      ( (member? (car s1) s2) (union (cdr s1) s2))
      ( else (cons (car s1) (union (cdr s1) s2))))))

(union '(stewed tomatoes and 
macaroni casserole) '(macaroni and cheese))