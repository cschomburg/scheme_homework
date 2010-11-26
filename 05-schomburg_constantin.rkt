#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Übung 05
;; 26.11.10 - 03.12.10
;; Constantin Schomburg


;; Ausgaben-Formatierung
(define (aufgabe n)
  (printf "~%Aufgabe ~a~%=========~%" n))
(define (output-expect name value expected)
  (printf " (~a) => ~a [~a]~%" name value (if (equal? value expected) "PASS" "FAIL")))
(define (output name value)
  (printf " (~a) => ~a~%" name value))
(define (output-title name)
  (printf " (~a) => " name))



;;#################
;;### Aufgabe 1 ###
;;#################
(aufgabe 1)

;;### Aufgabenteil 1a ###

;(cons (cons (cons 7 (cons 9 1)) (cons (cons 3 (cons 5 0)) (cons 6 (cons 8 2)))) 4)
;; [  4]
;;  |
;; [   ]--[   ]--[6 ]--[8 2]
;;  |      |
;;  |     [3  ]--[5 0]
;;  |
;; [7  ]--[9 1]

;(cons 7 (cons (cons 0 (cons (cons 5 9) (cons 4 2))) (cons (cons 6 1) (cons 3 8))))
;; [7 ]--[  ]--[  ]--[3 8]
;;        |     |
;;        |    [6 1]
;;        |
;;       [0  ]--[   ]--[4 2]
;;               |
;;              [5 9]


;;### Aufgabenteil 1b ###

(output-title "cons-Bau 1")
(cons 7 (cons (cons 0 (cons (cons 5 9) (cons 4 2))) (cons (cons (cons 6 1) 7) (cons 3 8))))

(output-title "cons-Bau 2")
(cons (cons (cons (cons 6 4) (cons 9 2)) (cons (cons (cons 5 1) 7) (cons 3 8))) (cons '() 0))


;;### Aufgabenteil 1c ###
;; PLATZHALTER Was gilt als Liste?



;;#################
;;### Aufgabe 2 ###
;;#################
(aufgabe 2)

;;### Aufgabenteil 2a ###

(define alphabet '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                   #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(define (alphabet->integer ch)
  (define (iter i alph-part)
    (cond ((null? alph-part) (error "Fehler!"))
          ((eq? ch (car alph-part)) i)
          (else (iter (+ i 1) (cdr alph-part)))))
  (iter 0 alphabet))

(output-expect "alphabet->integer #\\A" (alphabet->integer #\A) 0)
(output-expect "alphabet->integer #\\Z" (alphabet->integer #\Z) 25)
;(output-title "alphabet->integer #\\c") (alphabet->integer #\c) ;; Fehler!


;;### Aufgabenteil 2b ###

(define (integer->alphabet int)
  (define (iter i alph-part)
    (cond ((null? alph-part) (error "Fehler!"))
          ((eq? int i) (car alph-part))
          (else (iter (+ i 1) (cdr alph-part)))))
  (iter 0 alphabet))

(output-expect "integer->alphabet 3" (integer->alphabet 3) #\D)
(output-expect "integer->alphabet 10" (integer->alphabet 10) #\K)
;(output-title "integer->alphabet 27") (integer->alphabet 27) ;; Fehler!


;;### Aufgabenteil 2c ###

;; aus Vorlesung
(define (string->list str)
  (define (iter lst index)
    (if (< index 0) lst
        (iter (cons (string-ref str index) lst) (- index 1))))
  (iter '() (- (string-length str) 1)))

;; PLATZHALTER: Ohne map schreiben!
(define (string->intlist char->integer)
  (lambda (str)
    (map char->integer (string->list str))))

(define string->alphabet (string->intlist alphabet->integer))

(output-expect "string->alphabet \"HALLO\"" (string->alphabet "HALLO") '(7 0 11 11 14))
(output-expect "string->alphabet \"ZEBRA\"" (string->alphabet "ZEBRA") '(25 4 1 17 0))


;;### Aufgabenteil 2d ###

;; aus Vorlesung
(define (list->string lst)
  (define (iter rest str)
    (if (null? rest) str
        (iter (cdr rest) (string-append str (string (car rest))))))
  (iter lst ""))

;; PLATZHALTER: Ohne map schreiben!
(define (intlist->string integer->char)
  (lambda (lst)
    (list->string (map integer->char lst))))

(define alphabet->string (intlist->string integer->alphabet))

(output-expect "alphabet->string '(7 0 11 11 14)" (alphabet->string '(7 0 11 11 14)) "HALLO")
(output-expect "alphabet->string '(25 4 1 17 0)" (alphabet->string '(25 4 1 17 0)) "ZEBRA")
(output-expect "string->alphabet->string \"HALLO\"" (alphabet->string (string->alphabet "HALLO")) "HALLO")



;;#################
;;### Aufgabe 3 ###
;;#################
(aufgabe 3)

;;### Aufgabenteil 3b ###

(define (caesar-shift shift)
  (lambda (str)
    (alphabet->string (map (lambda (int)
                             (remainder (+ shift int) (length alphabet)))
                           (string->alphabet str)))))

(define my-caesar-shift (caesar-shift 3))
(define my-caesar-unshift (caesar-shift -3))

(output-expect "my-caesar-shift \"HALLO\"" (my-caesar-shift "HALLO") "KDOOR")
(output-expect "my-caesar-shift \"ZEBRA\"" (my-caesar-shift "ZEBRA") "CHEUD")
(output-expect "my-caesar-unshift \"KDOOR\"" (my-caesar-unshift "KDOOR") "HALLO")