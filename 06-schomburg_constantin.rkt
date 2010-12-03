#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Übung 06
;; 03.12.10 - 10.12.10
;; Constantin Schomburg

;; Ausgaben-Formatierung
(define-namespace-anchor a)(define ns (namespace-anchor->namespace a))(define (fix-length str limit expand)(let ((l (string-length str)))(cond ((> l limit) str) ((= l limit) str)(else (fix-length (string-append str expand) limit expand)))))(define (header s)(printf "~%~a~%~a~%" s (fix-length "" (string-length s) "=")))(define (<< text)(printf "  ~a  =>  " text))(define (<<! expression)(<< expression)(display (eval expression ns))(newline))(define (<<? expression expected)(let ((value (eval expression ns)))(<< expression)(display value)(printf "  [~a]~%" (if (equal? value expected) "PASS" "FAIL"))))



;;#################
;;### Aufgabe 1 ###
;;#################
(header "Aufgabe 1")

;; Hamming-Distanz (rekursiv)
(define (hamming-rec lst1 lst2)
  (cond ((null? lst1) 0)
        (else (+ (abs (- (car lst1) (car lst2)))
                 (hamming-rec (cdr lst1) (cdr lst2))))))

;; Hamming-Distanz (iterativ)
(define (hamming-iter lst1 lst2)
  (define (iter lst1 lst2 count)
    (cond ((null? lst1) count)
          (else (iter (cdr lst1)
                      (cdr lst2)
                      (+ (abs (- (car lst1) (car lst2)))
                         count)))))
  (iter lst1 lst2 0))

(<<? '(hamming-rec (list 1 0 1 1 0 1 0 1) (list 0 1 1 1 0 1 0 0)) 3)
(<<? '(hamming-iter (list 1 0 1 1 0 1 0 1) (list 0 1 1 1 0 1 0 0)) 3)



;;#################
;;### Aufgabe 2 ###
;;#################
(header "Aufgabe 2")

;; Gleicht eine Liste aus (entklammert)
(define (evenup lst)
  (cond ((null? lst) '())
        ((pair? (car lst)) (append (evenup (car lst))
                                   (evenup (cdr lst))))
        (else (cons (car lst) (evenup (cdr lst))))))

(<<? '(evenup (list 1 2 (list 3 (list 4 5 6)) 7 (list 8 9 (list 10 11) 12)))
     '(1 2 3 4 5 6 7 8 9 10 11 12))



;;#################
;;### Aufgabe 3 ###
;;#################
(header "Aufgabe 3")

;; Nächste Zahl in der Collatz-Folge
(define (collatz n)
  (if (even? n) (/ n 2)
      (+ (* n 3) 1)))

(<<? '(collatz 6) 3)
(<<? '(collatz 7) 22)

;; Reihe einer Collatz-Folge von n (iterativ)
(define (collatz-length n)
  (define (iter n l)
    (if (= n 1) l
        (iter (collatz n) (+ l 1))))
  (iter n 1))

(<<? '(collatz-length 6) 9)

;; Längste Collatz-Folge kleiner gleich n (iterativ)
(define (collatz-longest)
  (define (iter n curr-max)
    (if (> n 10000) curr-max
        (iter (+ n 1) (max curr-max (collatz-length n)))))
  (iter 1 0))

(<< "(collatz-longest)") (collatz-longest)



;;#################
;;### Aufgabe 4 ###
;;#################
(header "Aufgabe 4")

;; Reduziert eine Liste mithilfe einer Funktion auf einen Wert
(define (reduce f initial-value list)
  (if (null? list) initial-value
      (reduce f
              (f (car list) initial-value)
              (cdr list))))

(<<? '(reduce + 0 '(4 2 5 7 3 -1 7)) 27)

(define (partition threshold list)
  (cons (reduce (lambda (value new-list)
                  (cond ((> value threshold) (cons value new-list))))
                '()
                list)
        (reduce (lambda (value new-list)
                  (cond ((<= value threshold) (cons value new-list))))
                '()
                list)))

(<<! '(partition 3 '(0 3 5 7 2 1 6 4)))