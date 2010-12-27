#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Aufbaukurs 01a
;; 20.12.2010
;; Constantin Schomburg
;; Gebrauchte Zeit: 21/45 Minuten

(define ... 'TODO)
(define (assert-equal expected value)
  (cond ((not (equal? expected value)) (display "Fehler: ") (display expected)
                                       (display " erwartet, ") (display value)
                                       (display " erhalten.") (newline))))




(define example-tree '(a (b (c) (d) (e)) (f (g) (h (i)))))

;; Konstruktor

(define (make-tree value children) (cons value children))

;; Aufgabe 1

(define (first-k k list)
  (if (= k 0)
      '()
      (cons (car list) (first-k (- k 1) (cdr list)))))

(assert-equal '() (first-k 0 '(1 2 3)))
(assert-equal '(1 2) (first-k 2 '(1 2 3)))




;; Aufgabe 2

(define (without-first-k k list)
  (if (= k 0)
      list
      (without-first-k (- k 1) (cdr list))))

(assert-equal '(1 2 3) (without-first-k 0 '(1 2 3)))
(assert-equal '(3) (without-first-k 2 '(1 2 3)))
(assert-equal '() (without-first-k 3 '(1 2 3)))




;; Aufgabe 3

(define (decode-rec sequence)
  (cond ((null? sequence) '())
         (else (cons (make-tree (caar sequence)
                          (decode-rec (first-k (cdar sequence) (cdr sequence))))
                     (decode-rec (without-first-k (+ (cdar sequence) 1) sequence))))))

(assert-equal '() (decode-rec '()))
(assert-equal '((x)) (decode-rec '((x . 0))))
(assert-equal '((x (y)) (z)) (decode-rec '((x . 1) (y . 0) (z . 0))))
(assert-equal '((x (y (z)))) (decode-rec '((x . 2) (y . 1) (z . 0))))





(define (decode sequence) (car (decode-rec sequence)))
(assert-equal example-tree (decode '((a . 8) (b . 3) (c . 0) (d . 0) (e . 0) (f . 3) (g . 0) (h . 1) (i . 0))))