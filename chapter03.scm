#lang scheme
;what is (rember a lat)                                (lamb chops and jelly)
;where a is mint                                        "Rember" stands for remove a member
;and
; lat is (lamb chops and mint jelly)
; MySolution
(define remberFirst
  (lambda (a lat)
    (cond 
         ( (null? lat) '() )
         ( (eq? (car lat) a) (cdr lat))
         (  else (remberFirst a (cdr lat)))
     )
   )
)

; Book Solution
(define rember 
(lambda (a lat) 
(cond 
( (null? lat) (quote ())) 
(else (cond 
( ( eq? ( car lat) a) ( cdr lat)) 
(else ( rember a 
( cdr lat))))))))

(define a 'bacon)
(define lat '(bacon lettuce and tomato))

(remberFirst a lat)
(rember a lat)

; Use cons to build lists
(define remberCons
  (lambda (a lat)
  (cond
    ((null? lat) '())
    ((eq? a (car lat)) (cdr lat))
    ((cons (car lat) (remberCons a (cdr lat))))
    )
 ))

(remberCons a lat)

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      ( else (cons (car (car l)) (firsts (cdr l))))
     )
   )
)

(define items '((apple peach pumpkin) 
(plum pear cherry) 
(grape raisin pea) 
(bean carrot eggplant)))

(firsts items)


(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ( (eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat))))
      )
    )
)

(define new 'topping)
(define old 'fudge)
(define l3 '(ice cream with fudge for dessert))
(insertR new old l3)


;(subst new old lat) replaces the first occurrence of old in the lat with new
;For example, 
;where 
;new is topping 
;old is fudge 
;and 
;lat is (ice cream with fudge for dessert) 
;the value is 
;(ice cream with topping for dessert) 
(define subst
  (lambda (new old lat)
    (cond
      ( (null? lat) '())
      ( (eq? (car lat) old) (cons new (cdr lat)))
      ( else  (cons (car lat) (subst new old (cdr lat)))) 
      )
    )
)
(define l4 '(ice cream with fudge for dessert) )
(subst new old l4)

;Now try subst2 
;Hint: 
; (subst2 new o1 o2 lat) 
;replaces either the first occurrence of o1 or 
;the first occurrence of o2 by new 
;For example, 
;where 
; new is vanilla 
; o1 is chocolate 
; o2 is banana 
;and 
; lat is (banana ice cream with chocolate topping) 
;the value is 
;(vanilla ice cream with chocolate topping)

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ( (null? lat) '())
      ( (eq? (car lat) o1) (cons new (cdr lat)))
      ( (eq? (car lat) o2) (cons new (cdr lat)))
      ( else  (cons (car lat) (subst2 new o1 o2 (cdr lat)))) 
      )
    )
)

(define newFruit 'vanilla)
(define o1 'chocolate)
(define o2 'banana)
(define latFruit '(banana ice cream with chocolate topping))
(subst2 newFruit o1 o2 latFruit) ; (vanilla ice cream with chocolate topping)

;Write the function multirember which gives 
;as its final value the lat with all occurrences 
;of a removed
(define multirember
  (lambda (a lat)
    (cond
      ( (null? lat) '())
      ( (eq? (car lat) a) (multirember a (cdr lat)))
      ( else (cons (car lat) (multirember a (cdr lat))))
      )
    )
  )
(define cup 'cup)
(define members '(coffee cup tea cup and hick cup))
(multirember cup members) ;(coffee tea and hick)


(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat))))
     )
    )
)

(multisubst 'x 'y '(a b c d))
;; => (a b c d)
(multisubst 'x 'b '(a b c d))
;; => (a x c d)
(multisubst 'x 'b '(a b c b d))
;; => (a x c x d)
(multisubst 'x 'b '(b b b b))
;; => (x x x x)
