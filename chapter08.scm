#lang scheme

(define rember-ff
  (lambda (test?)
    (lambda (a l)
      (cond
        ( (null? l) '() )
        ( (test? (car l) a) (cdr l))
        ( else (cons (car l) ( (rember-ff test?) a (cdr l) )))
      )
    )
  )
)

((rember-ff eq?) 'tuna '(shrimp salad and tuna salad))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ( (null? l) '() )
        ( (test? (car l) old) (cons new (cons old (cdr l))) )
        ( else (cons (car l) ((insertL-f test?) new old (cdr l))) )
      )
    )
  )
)

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ( (null? l) '() )
        ( (test? (car l) old) (cons old (cons new (cdr l))))
        ( else (cons (car l) ((insertR-f test?) new old (cdr l))))
      )
    )
  )
)

(define seqL
  (lambda (new old l)
    (cons new (cons old l))
  )
)

(define seqR
  (lambda (new old l)
    (cons old (cons new l))
  )
)

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ( (null? l) '())
        ( (eq? (car l) old) (seq new old (cdr l)) )
        ( else (cons (car l) ((insert-g seq) new old (cdr l))))
      )
    )
  )
)

(define seqS
  (lambda (new old l)
    (cons new l)
  )
)

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))
(define subst (insert-g seqS))
(+ 1 2)

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ( (null? lat) '() )
        ( (test? a (car lat)) ( (multirember-f test?) a (cdr lat) ))
        ( else (cons (car lat) ( (multirember-f test?) a (cdr lat) )))
      )
    )
  )
)