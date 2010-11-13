#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Übung 01
;; 22.10.10 - 05.11.10
;; Constantin Schomburg

;;#################
;;### Aufgabe 2 ###
;;#################

;; Teil 2.1
(- (/ (+ 9 6)
      (* (- 3 1)
         5))
   (* (- 7/8 2)
      4))


;; Teil 2.2
(define (g u v w)
  (+ (/ (- v
           (* 7 u))
        (- u w))
     (/ (+ u v)
        (- (* w 6)
           v))))


;; Teil 2.3
(define (max x y)
  (cond ((> x y) x)
        ((<= x y) y)))

;;#################
;;### Aufgabe 3 ###
;;#################
;;   Die Clauses werden bereits ausgewertet, bevor die Kondition überhaupt
;; überprüft wird, also ergibt sich im Beispiel eine Endlosschleife, da selbst
;; bei n=0 die Funktion für n-1 weiter rekursiv aufgerufen wird.
;;   If setzt sich als special-form darüber hinweg und wertet erst aus, wenn
;; die Kondition die entsprechende Clause auch wirklich einleitet.

(define (my-if kondition then-clause else-clause)
  (cond (kondition then-clause)
        (else else-clause)))


;;##################
;;### Testreihen ###
;;##################

(require "tests.rkt")

(start-test "Maximum")
(test "1 und 5"  (max 1 5)  5)
(test "7 und -2" (max 7 -2) 7)