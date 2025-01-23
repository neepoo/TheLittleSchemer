#lang scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))
   )
)

(define add1
  (lambda (x) (+ x 1)))

;(add1 22) 23
(define sub1
  (lambda (n) (- n 1)))
;(sub1 70) 69

;(zero? 0) #t

; Try to write the function +
;  Hint: it use zero? add1 and sub1
(define plus
  (lambda (a b)
    (cond
      ( (zero? b) a)
      ( else (plus (add1 a) (sub1 b)))
     )
   )
)

(plus 1 2); 3
(plus 34 35); 69

(define sub
  (lambda (a b)
    (cond
      ( (zero? b) a)
      ( else (sub (sub1 a) (sub1 b)) )
     )
   )
)

(sub 1 2); -1
(sub 35 34); 1

; ------------------------------- MUL ---------------------------------------
(define mul
  (lambda (n m)
    (cond
      ( (zero? m) 0)
      ( (plus n (mul n (sub1 m))))
     )
   )
)
; ( mul 12 3) = 12 + ( mul 12 2)
;           = 12 + 12 + ( mul 12 1)
;           = 12 + 12 + 12 + ( mul 12 0)
;           = 12 + 12 + 12 + 0
(mul 1 0)
(mul 9 9)
(mul 3 3)

; ---------------------------- tupPlus ---------------------------------------

(define tupPlus
  (lambda (t1 t2)
    (cond
      ( (or (null? t1) (null? t2)) '())
      ( else (cons (plus (car t1) (car t2)) (tupPlus (cdr t1) (cdr t2))))
     )
    ))

(tupPlus '(3 7) '(4 6)) ; (7 13)
(tupPlus '(3 4) '(3 5)) ; (6 9)

; ------------------------------- gt -------------------------------
(define gt
  (lambda (a b)
    (cond
      ( (zero? a) #f)
      ( (zero? b) #t)
      ( else (gt (sub1 a) (sub1 b)))
    )
  )
)
(display "---gt---")
(newline)
(gt 1 0); #t
(gt 1 2) ; #f

; ------------------------------- eq ----------------------------------
(define eq
  (lambda (n m)
    (cond
      ( (zero? n) (zero? m) )
      ( (zero? m) #f )
      ( else (eq (sub1 n) (sub1 m)))
    )
  )
)
(display "---eq---")
(newline)
(eq 0 0) ; #t
(eq 1 1) ; #t
(eq 2 3) ; #f
(eq 3 2) ; #f
(eq 5 4) ; #f

(display "-------------------pow---------------------")
(newline)
(define pow
  (lambda (a b)
    (cond
      ( (eq b 1) a)
      ( (zero? b) 1 )
      
      ( else (mul a (pow a (sub1 b))))
     )
   )
)

(pow 1 1) ; 1
(pow 2 3) ; 8
(pow 5 3) ; 125

(display "--------------------------div--------------------------")
(newline)
(define div
  (lambda (n m)
    (cond
      ( (< n m) 0 )
      ( else (add1 (div (sub n m) m)))
    )
  )
)
(div 15 4)

(display "-------------------------------length------------------------------------")
(newline)
(define length
  (lambda (lat)
    (cond
      ( (null? lat) 0 )
      ( else (add1 (length (cdr lat))) )
    )
  )
)
(length '(a b c d e))

(display "----------------------------------(pick n lat) --------------------------------")
(newline)
(define pick
  (lambda (n lat)
    (cond
      ( (eq? n 1) (car lat))
      ( else (pick (sub1 n) (cdr lat)))
     )
  )
)

(pick 1 '(a b c d))
(pick 2 '(a b c d))
(pick 3 '(a b c d))
(pick 4 '(a b c d))

(display "---------------------------- (rempick n lat) -------------------")
(newline)

(define rempick
        (lambda (n lat)
          (cond
            ( (eq? n 1) (cdr lat))
            ( else (cons (car lat) (rempick (sub1 n) (cdr lat))))
            )))

(rempick 3 '(hotdogs with hot mustard))

(display "-------------------------------- no-nums ------------------------------------------")
(newline)

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat))))
      )))

(no-nums '(5 pears 6 prunes 9 dates)) ; (pears prunes dates)

(display "-------------------------------- all-nums ------------------------------------------")
(newline)

(define all-nums
  (lambda (lat)
    (cond
      ( (null? lat) '())
      ( (number? (car lat)) (cons (car lat) (all-nums  (cdr lat))))
      (else (all-nums (cdr lat)))
      )))

(all-nums '(5 pears 6 prunes 9 dates)) ; (5 6 9)

(display "--------------------------------- occur  --------------------------------------------------")
(newline)

(define occur
  (lambda (a lat)
    (cond
      ( (null? lat) 0)
      ( (eq? (car lat) a) (+ 1 (occur a (cdr lat))) )
      (else (occur a (cdr lat)))
      )))

(occur 'a '())  ; => 0
(occur 'a '(b c d e))  ; => 0
(occur 'a '(a b c d))  ; => 1
(occur 'a '(a b a c a d a))  ; => 4
(occur 'a '(1 a 2 b a 3 a 4))  ; => 3
(occur 'a '(a (a) b (a c)))  ; => 1
