#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Aufbaukurs 01b
;; 20.12.2010
;; Constantin Schomburg
;; Gebrauchte Zeit: 16/45 Minuten

(define ... 'TODO)
(define (assert-equal expected value)
  (cond ((not (equal? expected value)) (display "Fehler: ") (display expected)
                                       (display " erwartet, ") (display value)
                                       (display " erhalten.") (newline))))




(define example-tree '(a (b (c) (d) (e)) (f (g) (h (i)))))

;; Selektoren

(define (root-value node) (car node))
(define (root-children node) (cdr node))


;; Aufgabe 1

(define (describe tree)
  (cons (car tree) (- (length tree) 1)))

(assert-equal '(a . 0) (describe '(a)))
(assert-equal '(a . 2) (describe '(a (d (h)) (f))))




;; Aufgabe 2

(define (concat-lists lists) 
  (cond ((null? lists) '())
        ((list? (car lists)) (append (concat-lists (car lists)) (concat-lists (cdr lists))))
        (else (cons (car lists) (concat-lists (cdr lists))))))

(assert-equal '() (concat-lists '()))
(assert-equal '(a b c) (concat-lists '((a) () (b c))))
(assert-equal '((a . 1) (b . 2) (c . 3)) (concat-lists '(((a . 1)) () ((b . 2) (c . 3)))))




;; Aufgabe 3

(define (encode tree)
  (concat-lists (cons (describe tree)
                      (map (lambda (value)
                             (encode value))
                           (cdr tree)))))

(assert-equal '((a . 0)) (encode '(a)))
(assert-equal '((a . 2) (b . 3) (c . 0) (d . 0) (e . 0) (f . 2) (g . 0) (h . 1) (i . 0)) (encode example-tree))