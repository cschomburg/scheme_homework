#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Übung 04
;; 19.11.10 - 26.11.10
;; Constantin Schomburg

;;#################
;;### Aufgabe 1 ###
;;#################

;;### Aufgabenteil 1a) ###
;; Let öffnet einen neuen Block, in dem pi als lokale Variable neu definiert wird.
;; Da sich (* pi d) in diesem Block befindet, wird das lokale Pi verwendet und nicht im globalen Environment nachgeschaut.

;;### Aufgabenteil 1b) ###
;; Die lokalen Variablen im let-Aufruf werden simultan definiert, also werden bei der Auswertung die Werte aus den Variablen höheren Gültigkeitsbereiches genommen statt der neu definierten. Beispiel pi: die anderen Variablen-Definitionen verwenden den Wert aus dem Global Environment statt aus let.

;; Oberfläche eines Zylinders
(define (cylinder-area d h)
  (let ((pi 3.14159)
        (r (/ d 2)))
    (let ((k (* pi r r))
          (s (* 2 pi r h)))
      (let ((b (* 2 k)))
        (+ b s)))))

;(cylinder-area 2 3) ;; 25.13272


;;### Aufgabenteil 1c) ###

;; Umfang eines Kreises, Lambda-Schreibweise
(define circle-circumference-lambda (lambda (d)
  ((lambda (pi)
    (* pi d)) 3.14159)))

;; Oberfläche eines Zylinders, Lambda-Schreibweise
(define cylinder-area-lambda (lambda (d h)
  ((lambda (pi r)
     ((lambda (k s)
        ((lambda (b)
           (+ b s))
         (* 2 k)))
      (* pi r r) (* 2 pi r h)))
   3.14159 (/ d 2))))



;;#################
;;### Aufgabe 2 ###
;;#################

;; Osterdatum eines Jahres
(define (osterdatum j)
  (let ((a (remainder j 19))
        (b (remainder j 4))
        (c (remainder j 7))
        (k (quotient j 100)))
    (let ((p (quotient (+ (* 8 k) 13) 25))
          (q (quotient k 4)))
      (let ((M (remainder (- k -15 p q) 30))
            (N (remainder (- k -4 q) 7)))
        (let ((d (remainder (+ (* 19 a) M) 30)))
          (let ((e (remainder (+ (* 2 b) (* 4 c) (* 6 d) N) 7)))
            (let ((o (+ 22 d e)))
              (cond ((> o 31) (display (- o 31)) (display ". April"))
                    (else (display o) (display ". März")))
              (newline))))))))

;; let-Ausdrücke: 6



;;#################
;;### Aufgabe 3 ###
;;#################

;; Komposition von zwei Funktionen
(define (compose f1 f2)
  (lambda (x)
    (f1 (f2 x))))

;((compose (lambda (x) (expt x 2))
;         (lambda (x) (- (* 2 x) 3)))
; 6) ;; 81


;;### Aufgabenteil 3b) ###

(define (h^k h k)
  (cond ((= k 1) (lambda (x) (h x)))
        ((> k 1) (lambda (x) (h ((h^k h (- k 1)) x))))))

(define (square x) (* x x))
(define f (h^k square 4))

;(f 2) ;; 65536



;;#################
;;### Aufgabe 4 ###
;;#################

;; Eine Iterationsfunktion, die bei start-value und start-index beginnt und beide so lange mittels der jeweiligen Funktion (next-value / next-index) erhöht, bis finished? ein #t zurückgibt. Dabei wird der aktuelle Wert (curr-value) zurückgegeben.
;;   start-value: der Wert, bei dem begonnen wird
;;   next-value: eine Funktion, die Wert und Index bekommt und daraufhin den nächsten Wert zurückgibt
;;   start-index: Der Index, bei dem begonnen wird
;;   next-index: eine Funktion, die Wert und Index bekommt und daraufhin den nächsten Index zurückgibt
;;   finished?: Eine Funktion, die Wert und Index bekommt und daraufhin #t ausgibt, wenn die Iteration beendet werden soll.

(define (iterate start-value next-value start-index next-index finished?)
  (define (inner-iter cur-val cur-index)
    (if (finished? cur-val cur-index)
        cur-val
        (inner-iter (next-value cur-val cur-index) (next-index cur-val cur-index))))
  (inner-iter start-value start-index))


;;### Aufgabenteil 4b) ####

(define (phi n)
  (iterate 1.
           (lambda (cur-val cur-index) (+ 1 (/ 1 cur-val)))
           0
           (lambda (cur-val cur-index) (+ cur-index 1))
           (lambda (cur-val cur-index) (>= cur-index n))))


;;### Aufgabenteil 4c) ####

(define (h^k-iterate h k)
  (lambda (x)
    (iterate x
             (lambda (cur-val cur-index) (h cur-val))
             k
             (lambda (cur-val cur-index) (- cur-index 1))
             (lambda (cur-val cur-index) (< cur-index 1)))))
 


;;##################
;;### Testreihen ###
;;##################

(require "tests.rkt")
(test #f (lambda (>> t)
           
           (>> "Aufgabe 1")
           (t "Umfang lambda" (circle-circumference-lambda 2) 6.28318)
           (t "Zyl.-Fläche lambda/let" (cylinder-area-lambda 2 3) (cylinder-area 2 3))
         
           (>> "Aufgabe 3")
           (t "Komposition" ((compose (lambda (x) (expt x 2)) (lambda (x) (- (* 2 x) 3))) 6) 81)
           (t "h^k: f(2)" (f 2) 65536)
         
           (>> "Aufgabe 4")
           (t "Phi(0)"  (phi  0) 1.0)
           (t "Phi(1)"  (phi  1) 2.0)
           (t "Phi(2)"  (phi  2) 1.5)
           (t "Phi(10)" (phi 10) 1.6179775280898876)
           (t "h^k-iterate" ((h^k-iterate square 4) 2) 65536)))
