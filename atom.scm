#lang scheme
(define atom? 
(lambda (x) 
(and (not (pair? x)) (not (null? x)))))


(atom? (quote ()))
(cons '((help) this) '(is very ((hard) to learn)))
null? (quote ())
(null? '(a b c))
(quote(a b c))

(null? '(spaghetti))
(atom? 'Harry)
(atom? (quote Harry))

(atom? (car '(Harry had a heap of apples)))
(atom? (cdr '(Harry had a heap of apples)))
(atom? (cdr '(Harry)))

(atom? (car (cdr '(swing low sweet cherry oat))))

(eq? 'Harry 'Harry)