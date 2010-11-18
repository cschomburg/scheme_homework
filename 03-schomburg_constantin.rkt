#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Übung 03
;; 12.11.10 - 19.11.10
;; Constantin Schomburg



;; Hilfsfunktionen aus der Vorlesung
(define (cputime f . args)
  (define-values (result cpu real gc) (time-apply f args))
  (* 1.0 cpu))

(define (factor o n f . args)
  (define-values (result cpu real gc) (time-apply f args))
  (* 1.0 (/ cpu (o n))))



;;#################
;;### Aufgabe 1 ###
;;#################

(define (aufgabe1 n)
  (define (iter n sum)
    (cond ((<= n 0) sum)
          ((or (= (remainder n 5) 0)
               (= (remainder n 3) 0))
           (iter (- n 1) (+ sum n)))
          (else (iter (- n 1) sum))))
  (iter (- n 1) 0))

(aufgabe1 1000) ;; 233168



;;#################
;;### Aufgabe 2 ###
;;#################

;; factorial aus Vorlesung
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; Compute-e mit Verwendung von factorial
(define (compute-e1 n)
  (define (iter m sum)
    (if (> m n) sum
        (iter (+ m 1)
              (+ sum (/ 1 (factorial m))))))
  (iter 0 0.))

;; Compute-e ohne Verwendung von factorial
(define (compute-e2 n)
  (define (iter m value term)
    (if (> m n) (+ value term)
        (iter (+ m 1)
              (+ value term)
              (/ term m))))
  (iter 1 0. 1))



;;#################
;;### Aufgabe 3 ###
;;#################

;; Rekursiv
(define (f-rec n)
  (if (< n 3) 1
      (+ (- (* 3 (f-rec (- n 1)))
            (* 2 (f-rec (- n 2))))
         (* 3 (f-rec (- n 3))))))

;; Iterativ
(define (f-iter n)
  (define (iter m f-1 f-2 f-3)
    (if (> m n) f-1
        (iter (+ m 1)
              (+ (- (* 3 f-1)
                    (* 2 f-2))
                 (* 3 f-3))
              f-1
              f-2)))
  (if (< n 3) 1
      (iter 3 1 1 1)))

;(f-iter 100) ;; 1159988228848170573365003508337887644164465

;; Der rekursive Prozess hat eine exponentielle Laufzeit von O(1.84^n), da
;; er beim Berechnen immer auf die Werte von rekursiven Prozeduren zurückgreifen muss.
;; Im Gegensatz läuft der iterative Prozess schneller, da er auf vorher berechnete
;; Eregebnisse zurückgreift. Ich schätze, er läuft mit O(n), die realen Werte deuten
;; aber eher auf O(n^2) hin - vielleicht wegen der großen Zahlen?

;; Proportionalitätsfaktoren an meinem Desktop-Computer:
;; rekursiv: ~ 9e-05 +- 3e-05
;; iterativ: ~ 0.04  +- 0.03

;; O-Funktionen
(define (f-rec-o n) (expt 1.84 n))
(define (f-iter-o n) (expt n 2))

;(factor f-iter-o 10000 f-iter 10000)
;(factor f-iter-o 20000 f-iter 20000)
;(factor f-iter-o 50000 f-iter 50000)
;(factor f-iter-o 100000 f-iter 100000)



;;#################
;;### Aufgabe 4 ###
;;#################



;;##################
;;### Testreihen ###
;;##################

(require "tests.rkt")

(start-test "Aufgabe 1")
(test "10" (aufgabe1 10) 23)

(start-test "E mit factorial")
(test "0"  (compute-e1 0)  1.0)
(test "27" (compute-e1 27) 2.7182818284590455)

(start-test "E ohne factorial")
(test "0"  (compute-e2 0)  1.0)
(test "27" (compute-e2 27) 2.7182818284590455)

(start-test "Aufgabe 3")
(test "rekursiv" (f-rec  10) 4504)
(test "iterativ" (f-iter 10) 4504)