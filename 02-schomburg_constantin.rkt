#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Übung 02
;; 05.11.10 - 12.11.10
;; Constantin Schomburg

;; Quadratfunktion für Aufgaben 1 + 3
(define (sq x) (* x x))



;;#################
;;### Aufgabe 1 ###
;;#################

;; Distanz zwischen zwei Punkten
(define (distance x1 y1 x2 y2)
  (sqrt (+ (sq (- x1 x2)) (sq (- y1 y2)))))

;; Enthält der erste Kreis den zweiten Kreis?
(define (contains? x1 y1 r1 x2 y2 r2)
  (<= (distance x1 y1 x2 y2) (- r1 r2)))

;; Liegt der Punkt innerhalb des Kreises?
(define (is-in? x y x1 y1 r1)
  (<= (distance x y x1 y1) r1))



;;#################
;;### Aufgabe 2 ###
;;#################
;; Rekursive vs. iterative Prozesse
;; Bei einem rekursiven Prozess muss die Prozedur auf aufgerufene "Sub-Prozeduren" warten, um weiterzurechnen.
;; Dagegen verlaufen bei einem iterativen Prozess die Prozeduren nacheinander ab: die erste muss nicht warten,
;; die letzte gibt das vollständige Ergebnis zurück.
;;   Ein rekursiver Prozess benötigt mehr Speicher, ist aber schneller.

;; Rekursiver Prozess
(define (sum-rec n)
  (cond ((<= n 0) 0)
        (else (+ n (sum-rec (- n 1))))))

;; Iterativer Prozess
(define (sum-iter n curr-sum)
  (cond ((<= n 0) curr-sum)
        (else (sum-iter (- n 1) (+ curr-sum n)))))



;;#################
;;### Aufgabe 3 ###
;;#################

;; Gibt eine Meldung aus, wenn der erste Wert vom zweiten abweicht
(define (check-expect value expected)
  (cond ((not (= value expected)) (display expected) (display " erwartet, ") (display value) (display " erhalten") (newline))))

;; Berechnet die Quadrat-Quersumme einer Zahl (rekursiver Prozess)
(define (quadrat-quersumme n)
  (cond ((= n 0) 0)
        (else (+ (sq (remainder n 10))
                 (quadrat-quersumme (quotient n 10))))))

(check-expect (quadrat-quersumme 5)    25)
(check-expect (quadrat-quersumme 13)   10)
(check-expect (quadrat-quersumme 4204) 36)



;;#################
;;### Aufgabe 4 ###
;;#################

;; Überprüfung einer ISBN (iterativer Prozess)
(define (isbn-test isbn)
  (define (isbn-iter n i current-value)
    (cond ((= i 0) current-value)
          (else (isbn-iter (quotient n 10)
                           (- i 1)
                           (+ current-value (* i (remainder n 10)))))))
  (remainder (isbn-iter isbn 9 0) 11))



;;##################
;;### Testreihen ###
;;##################
;; Nicht Teil der Lösung (benutzen eqv? und set!)
;; Dienen nur zur Überprüfung

;; Definitionen
(define curr-test-name "")
(define curr-test-i 0)

(define (start-test name)
  (set! curr-test-name name)
  (set! curr-test-i 0)
  (newline) (display "Testreihe: ") (display name) (newline))

(define (test desc value expected)
  (set! curr-test-i (+ curr-test-i 1))
  (cond ((eqv? value expected) (display "      "))
        (else (display " FAIL ")))
  (display "#") (display curr-test-i) (display ": ") (display desc) (display " [") (display expected) (display "|") (display value) (display "]") (newline))

;; Tests
(start-test "Die Tests selbst!")
(test "erfüllt" #t #t)
(test "nicht erfüllt" #f #t)

(start-test "Distanz")
(test ""   (distance 0 0 (sqrt 8) (sqrt 8)) 4.)

(start-test "Kreis in Kreis?")
(test "gleicher Pos, kleiner" (contains? 0 0 1     0 0 0.5) #t)
(test "gleiche Pos, größer"   (contains? 5 5 1     5 5 2)   #f)
(test "innerhalb"             (contains? -1 -1 4   1 1 1)   #t)
(test "außerhalb"             (contains? -2 -2 4   3 3 4)   #f)
(test "überlappend"           (contains? 0 0 2     1 1 2)   #f)
(test "innen berührend"       (contains? 0 0 2     1 0 1)   #t)
(test "identisch"             (contains? 5 5 2     5 5 2)   #t)

(start-test "Punkt in Kreis?")
(test "innerhalb" (is-in? 0 0     -1 -1 2) #t)
(test "außerhalb" (is-in? -2 -2   4 4 3)   #f)
(test "auf Rand"  (is-in? 0 1     0 0 1)   #t)

(start-test "Summe von 0 bis 6")
(test "rekursiv" (sum-rec 6)    21)
(test "iterativ" (sum-iter 6 0) 21)

(start-test "Quadrat-Quersumme")
(test "5"    (quadrat-quersumme 5)    25)
(test "13"   (quadrat-quersumme 13)   10)
(test "4204" (quadrat-quersumme 4204) 36)

(start-test "ISBN")
(test "344615497" (isbn-test 344615497)  3)
(test "026201153" (isbn-test 026201153)  0)
(test "392511825" (isbn-test 392511825) 10)
