#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Übung 07
;; 10.12.10 - 17.12.10
;; Constantin Schomburg

;; Ausgaben-Formatierung
(define-namespace-anchor a)(define ns (namespace-anchor->namespace a))(define (fix-length str limit expand)(let ((l (string-length str)))(cond ((> l limit) str) ((= l limit) str)(else (fix-length (string-append str expand) limit expand)))))(define (header s)(printf "~%~a~%~a~%" s (fix-length "" (string-length s) "=")))(define (<< text)(printf "  ~a  =>  " text))(define (<<! expression)(<< expression)(display (eval expression ns))(newline))(define (<<? expression expected)(let ((value (eval expression ns)))(<< expression)(display value)(printf "  [~a]~%" (if (equal? value expected) "PASS" "FAIL"))))



;;#################
;;### Aufgabe 1 ###
;;#################
(header "Aufgabe 1")

(define (my-cons x y)
  (lambda (msg)
    (cond ((eq? msg 'CAR) x)
          ((eq? msg 'CDR) y)
          ((eq? msg 'PAIR) #t)
          ((eq? msg 'FLIP) (my-cons y x))
          (else (error "pair cannot" msg)))))

(define (my-car p) (p 'CAR))
(define (my-cdr p) (p 'CDR))
(define (my-pair p) (and (procedure? p) (p 'PAIR)))

;; Abstraktionsbarrieren bieten den Vorteil, zu Grunde liegende Datenstrukturen und Funktionen zu verstecken, so dass auf einer höheren Ebene kein Code mehr umgeschrieben werden muss, wenn man sich z.B. entschließt, auf eine andere Datenstruktur umzusteigen.

;; Flip unter Verwendung der Abstraktion
(define (flip pair)
  (my-cons (my-cdr pair) (my-car pair)))

(<<? '(my-car (flip (my-cons 'x 'y))) 'y)

;; Flip innerhalb der Barriere
(define (flip2 pair) (pair 'FLIP))

(<<? '(my-car (flip2 (my-cons 'x 'y))) 'y)



;;#################
;;### Aufgabe 2 ###
;;#################
(header "Aufgabe 2")

;; (cons (car population ...) ergibt sich daraus, dass die erste Zelle stets gleich bleibt (da der (nicht-existentte) linke Nachbar immer tot ist
;; Map wird aufgerufen mit:
;;   - einer Funktion, die den neuen Status der jeweiligen Zelle berechnet
;;   - dem Rest der Population (startet bei zweiter Zelle) mit dem neuen Zuwachs einer toten Zelle
;;   - der Population

(define (sierpinski-step population)
  (cons (car population)
        (map (lambda (curr last)
               (cond ((eq? last curr) '_)
                     (else 'X)))
             (append (cdr population) '(_))
             population)))

(<<? '(sierpinski-step '(X X X X)) '(X _ _ _ X))
(<<? '(sierpinski-step '(X _ _ X _ X)) '(X X _ X X X X))
(<<? '(sierpinski-step '(_ X _ X _ _ _)) '(_ X X X X _ _ _))

(define (sierpinski n)
  (define (iter i population)
    (display population) (newline)
    (if (= i n) population
        (iter (+ i 1) (sierpinski-step population))))
  (iter 1 '(X))
  (cond (#f #f)))

(<< "(sierpinski 32)") (display "Bitte manuell ausführen!\n")



;;#################
;;### Aufgabe 3 ###
;;#################
(header "Aufgabe 3 Hilfsfunktionen")

;; Aus Vorlesung unverändert
(define (=number? exp num)
  (and (number? exp) (= exp num)))


;; Aus Vorlesung unverändert
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; Aus Vorlesung: Summe
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

;; Aus Vorlesung: Produkt
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;; Eigene Division
(define (make-quotient d1 d2)
  (cond ((and (number? d1) (number? d2)) (/ d1 d2))
        ((=number? d2 1) d1)
        (else (list '/ d1 d2))))
(define (quotient? x)
  (and (pair? x) (eq? (car x) '/)))
(define (dividend d) (cadr d))
(define (divisor d) (caddr d))

(<<? '(quotient? (make-quotient 3 'X)) #t)
(<<? '(make-quotient 'X 1) 'X)

;; Eigene Subtraktion
(define (make-difference d1 d2)
  (cond ((and (number? d1) (number? d2)) (- d1 d2))
        ((=number? d2 0) d1)
        (else (list '- d1 d2))))
(define (difference? x)
  (and (pair? x) (eq? (car x) '-)))
(define (minuend d) (cadr d))
(define (subtrahend d) (caddr d))

(<<? '(subtrahend (make-difference 3 'X)) 'X)
(<<? '(make-difference 'X 0) 'X)

;; Eigene Potenzierung
(define (make-exponentiation e1 e2)
  (cond ((and (number? e1) (number? e2)) (expt e1 e2))
        ((=number? e2 1) e1)
        ((=number? e2 0) 1)
        (else (list '^ e1 e2))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(<<? '(subtrahend (make-difference 3 'X)) 'X)
(<<? '(make-difference 'X 0) 'X)

;; Ableitungsfunktion mit Division
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((difference? exp)
         (make-difference (deriv (minuend exp) var)
                          (deriv (subtrahend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((quotient? exp)
         (make-quotient (make-difference (make-product (deriv (dividend exp) var)
                                                       (divisor exp))
                                         (make-product (dividend exp)
                                                       (deriv (divisor exp) var)))
                        (make-product (divisor exp)
                                      (divisor exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (make-difference (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(header "Aufgabe 3 Derive")
(<<? '(deriv '(/ (* 5 x) x) 'x) '(/ (- (* 5 x) (* 5 x)) (* x x)))
(<<? '(deriv '(^ x 5) 'x) '(* 5 (^ x 4)))


;;### Aufgabenteil 3c ###
(define (products->exponentiations exp)
  (cond ((null? exp) '())
        ((not (pair? exp)) exp)
        ((and (product? exp) (equal? (multiplier exp) (multiplicand exp)))
         (make-exponentiation (multiplier exp) 2))
        (else (list (car exp)
                    (products->exponentiations (cadr exp))
                    (products->exponentiations (caddr exp))))))

(header "Aufgabe 3c")
(<<? '(products->exponentiations '(* x x)) '(^ x 2))
(<<? '(products->exponentiations (deriv '(/ (* 5 x) x) 'x)) '(/ (- (* 5 x) (* 5 x)) (^ x 2)))