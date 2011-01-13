#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Übung 09
;; 07.01.11 - 14.01.11
;; Constantin Schomburg

(require scheme/mpair)

;; Ausgaben-Formatierung
(define-namespace-anchor nsa)(define ns (namespace-anchor->namespace nsa))(define (fix-length str limit expand)(let ((l (string-length str)))(cond ((> l limit) str) ((= l limit) str)(else (fix-length (string-append str expand) limit expand)))))(define (header s)(printf "~%~a~%~a~%" s (fix-length "" (string-length s) "=")))(define (<< text)(printf "  ~a  =>  " text))(define (<<! expression)(<< expression)(display (eval expression ns))(newline))(define (<<? expression expected)(let ((value (eval expression ns)))(<< expression)(display value)(printf "  [~a]~%" (if (equal? value expected) "PASS" "FAIL"))))



;;#################
;;### Aufgabe 1 ###
;;#################
(header "Aufgabe 1")

(define (fun-with-set-1 l)
  (define (help l r)
    (if (null? l) r
        (let ((temp (mcdr l)))
          (set-mcdr! l r)
          (help temp l))))
  (help l '()))

(define (fun-with-set-2 l)
  (define (help r)
    (if (null? (mcdr r)) (set-mcdr! r l)
        (help (mcdr r))))
  (help l))


;;### 1a) ###

(define a (mlist 2 3 5 7))
(define b (fun-with-set-1 a))
(<<! 'a)
(<<! 'b)

;; fun-with-set1 gibt die Liste in umgekehrter Reihenfolge zurück
;; Kästchennotation [ # = '() ]
;;   a vorher:  [2  ]--[3  ]--[5  ]--[7  #]
;;   a nachher: [2 #]
;;   b:         [7  ]--[5  ]--[3  ]--[2  #]


;;### 1b) ###

(define (hello l)
  (if (null? l) (display "Hello!")
      (hello (mcdr l))))

;; fun-with-set-2 fügt eine Liste an ihr eigenes Ende an (zyklische Liste)
;; hello iteriert durch die Liste und gibt am Ende (= null) "Hello!" aus.
;; In diesem Zyklus gibt es kein Ende, also läuft die Funktion in einer Endlosschleife

(define c (mlist 6 2 9 3 5))
(fun-with-set-2 c)
;(hello c) ;; Endlosschleife


;;### 1c) ###

(define (my-mlist? lst)
  (define (iter a b isSecond)
    (if (not (mpair? a)) #f
        (let ((a-step (mcdr a)))
          (cond ((null? a-step) #t)
                (isSecond (iter a-step (mcdr b) #f))
                ((eq? a-step b) #f)
                (else (iter a-step b #t))))))
  (iter lst lst #f))

(<<? '(my-mlist? a)     (mlist? a))
(<<? '(my-mlist? b)     (mlist? b))
(<<? '(my-mlist? c)     (mlist? c))



;;#################
;;### Aufgabe 2 ###
;;#################
(header "Aufgabe 2")

(define memory #f)

(define (f x)
  (if memory 0
      (begin (set! memory #t) x)))

(<<? '(+ (f 0) (f 1)) 0)
(set! memory #f) ;; Gedächtnis-Reset, damit es nicht vom vorherigen Aufruf abhängt
(<<? '(+ (f 1) (f 0)) 1)



;;#################
;;### Aufgabe 3 ###
;;#################
(header "Aufgabe 3")

(define (make-monitored f)
  (let ((calls 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) calls)
            ((eq? arg 'reset!) (let ((old calls))
                                 (set! calls 0)
                                 old))
            (else (set! calls (+ calls 1))
                  (f arg))))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(set! fibonacci (make-monitored fibonacci))

(<<? '(fibonacci 10)               55)
(<<? '(fibonacci 'reset!)          177)
(<<? '(fibonacci 20)               6765)
(<<? '(fibonacci 'how-many-calls?) 21891)



;;#################
;;### Aufgabe 4 ###
;;#################
;; siehe 09-schomburg_constantin-zeichnung.png