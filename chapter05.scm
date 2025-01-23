#lang scheme
;What is ( rember* a l}                                        ((coffee) ((tea)) (and (hick))}
;where a is cup 
;and 
; l is ((coffee) cup ((tea) cup) (and (hick}) cup) 
; "rember*" is pronounced "rember-star."

;(rember* 'cup '((coffee) cup ((tea) cup) (and (hick}) cup))


(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))
   )
)
(display "------------------------rember*----------------------------------")
(newline)
(display"When recurring on a list of S-expressions, l, ask three ")
(newline)
(display"question about it: (null? l), ( atom? ( car l)), and else.")
(newline)

(define rember*
  (lambda (a l)
    (cond
      ( (null? l) '() )
      ( (atom? (car l))
        (cond
          ( (eq? (car l) a) (rember* a (cdr l)))
          ( else (cons (car l) (rember* a (cdr l))))
        )
      )
     ( else (cons (rember* a (car l)) (rember* a (cdr l))))
    )
  )
)

(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))


(display "------------------------(insertR* new old l)----------------------------------")
(newline)

(define insertR*
  (lambda (new old l)
    (cond
      ( (null? l) '())
      ( (atom? (car l))
        (cond
          ( (eq? (car l) old) (cons old ( cons new (insertR* new old (cdr l))) ))
          ( else (cons (car l) (insertR* new old (cdr l))))
        )
      )
      ( else (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
    )
  )
)

(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

(display"-------------------------------- (occur* a l) -----------------------------------------------------")
(newline)
(define occurs*
  (lambda (a l)
    (cond
      ( (null? l) 0)
      ( (atom? (car l))
        (cond
          ( (eq? (car l) a) (+ 1 (occurs* a (cdr l))))
          ( else (occurs* a (cdr l)))
        )
      )
      (else (+ (occurs* a (car l)) (occurs* a (cdr l))))
    )
  )
)

(occurs* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))

(display "-----------------------------------------(subst* new old l)---------------------------------")
(newline)
(define l '((banana) 
           (split ((((banana ice))) 
                   (cream (banana)) 
                   sherbet)) 
           (banana) 
           (bread) 
           (banana brandy)))

(define subst*
  (lambda(new old l)
    (cond
      ( (null? l) '() )
      ( (atom? (car l))
        (cond
          ( (eq? (car l) old) (cons new (subst* new old (cdr l)))  )
          ( else (cons (car l) (subst* new old (cdr l))))
        )
      )
      ( else (cons (subst* new old (car l)) (subst* new old (cdr l))))
    )
  )
)
(subst* 'orange 'banana l)

(display "--------------------------(member* a l)---------------------------------------------------")
(newline)
(define member*
  (lambda (a l)
    (cond
      ( (null? l) #f )
      ( (atom? (car l))
        (cond
          ( (eq? (car l) a) #t)
          ( else (member* a (cdr l)))
        )
      )
      ( else (or (member* a (car l)) (member* a (cdr l))))
    )
  )
)
(member* 'chips '((potato) (chips ((with) fish) (chips))))


(display "--------------------------(leftmost l)---------------------------------------------------")
(newline)

(define leftmost
  (lambda (l)
    (cond
      ( (null? l) '())
      ( (atom? (car l) ) (car l))
      ( else (leftmost (car l)))
      )))
(leftmost '((potato) (chips ((with) fish) (chips)))) ; potato
(leftmost '(((() four)) 17 (seventeen))) ; ()
(leftmost '(((hot) (tuna (and))) cheese)) ; hot

(display "eqan: which is true if its 
two arguments ( a1 and a2) are the same 
atom. Remember to use = for numbers and 
eq? for all other atoms.")
(newline)

(define eqan?
  (lambda (a1 a2)
    (cond
      ( (and (number? a1) (number? a2)) (= a1 a2)) ; a1 and a2 are numbers, compare using =
      ( (or (number? a1) (number? a2)) #f) ; a1 or a2 type is number
      ( else (eq? a1 a2))
    )
  )
)

(define eqlist?
  (lambda (l1 l2)
    (cond
      ( (and (null? l1) (null? l2)) #t ) ;; case 0, 0
      ( (and (null? l1) (atom? (car l2))) #f ) ;; case 0, 1
      ( (and (atom? (car l1)) (null? l2)) #f) ;; case 1, 0
      ( (and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))  ) ;;case 1, 1 Check if the first element in both lists is an atom, and use `eqan` to verify both elements ate equal
      ( (atom? (car l1)) #f) ;; 
      ( (null? l2) #f)
      ( (atom? (car l2)) #f)
      ( else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
    )
  )
)

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((atom? s1) #f)    ;; this and next line can be simplify as ((or (atom? s1) (atom? s2)) #f)
      ((atom? s2) #f)
      (else (eqlist? s1 s2))
      ))
  )

(define equalXXX?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((atom? s1) #f)    ;; this and next line can be simplify as ((or (atom? s1) (atom? s2)) #f)
      ((atom? s2) #f)
      (else (eqlistXXX? s1 s2))
      ))
  )

(display "Now, rewrite elist? using equal?")
(newline)
(define eqlistXXX?
  (lambda (l1 l2)
    (cond
      ( (and (null? l1) (null? l2)) #t)
      ( (or (null? l1) (null? l2)) #f)
      ( else (and (equalXXX? (car l1) (car l2)) (eqlistXXX? (cdr l1) (cdr l2))))
      )))

(eqlistXXX? '(strawberry ice cream) '(strawberry ice cream))  ;; #t
(eqlistXXX? '(strawberry ice cream) '(strawberry cream ice)) ;; #f
(eqlistXXX? '(banana ((split))) '((banana) (split))) ;; #f
(eqlistXXX? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) ;; #f
(eqlistXXX? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) ;; #t

(display "Simplify only after the function is correct.")
(newline)
(define rember
  (lambda (s l)
    (cond
      ( (null? l) '() )
      ( else
        (cond
          ( (equalXXX? (car l) s) (cdr l) )
          ( else (cons (car l) (rember s (cdr l))) )
        )
      )
    )
  )
)

(rember 'beef '(beef ((sausage)) (and (soda))))