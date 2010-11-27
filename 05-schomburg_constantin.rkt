#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Übung 05
;; 26.11.10 - 03.12.10
;; Constantin Schomburg

;; Ausgaben-Formatierung
(define-namespace-anchor a)(define ns (namespace-anchor->namespace a))(define (fix-length str limit expand)(let ((l (string-length str)))(cond ((> l limit) str) ((= l limit) str)(else (fix-length (string-append str expand) limit expand)))))(define (header s)(printf "~%~a~%~a~%" s (fix-length "" (string-length s) "=")))(define (<< text)(printf "  ~a  =>  " text))(define (<<! expression)(<< expression)(display (eval expression ns))(newline))(define (<<? expression expected)(let ((value (eval expression ns)))(<< expression)(display value)(printf "  [~a]~%" (if (equal? value expected) "PASS" "FAIL"))))



;;#################
;;### Aufgabe 1 ###
;;#################
(header "Aufgabe 1")

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

(<< "cons-Konstruktion 1")
(cons 7 (cons (cons 0 (cons (cons 5 9) (cons 4 2))) (cons (cons (cons 6 1) 7) (cons 3 8))))

(<< "cons-Konstruktion 2")
(cons (cons (cons (cons 6 4) (cons 9 2)) (cons (cons (cons 5 1) 7) (cons 3 8))) (cons '() 0))


;;### Aufgabenteil 1c ###
;; Listen sind gültig, wenn das Decrement Register des letzten (im DR verschachtelten) Paares eine Null-Liste enthält
;; 1) Ja
;; 2) Nein
;; 3) Ja
;; 4) Nein
;; 5) Ja



;;#################
;;### Aufgabe 2 ###
;;#################
(header "Aufgabe 2a/b (int<->char)")

;;### Aufgabenteil 2a ###

(define alphabet '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                   #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

;; Wandelt einen Wert in der Alphabet-Liste in dessen Stelle um
(define (alphabet->integer ch)
  (define (iter i lst)
    (cond ((null? lst) (error "Fehler!"))
          ((eq? ch (car lst)) i)
          (else (iter (+ i 1) (cdr lst)))))
  (iter 0 alphabet))

(<<? '(alphabet->integer #\A) 0)
(<<? '(alphabet->integer #\Z) 25)
;(<<! '(alphabet->integer #\c)) ;; Fehler


;;### Aufgabenteil 2b ###

;; Wandelt eine Stelle in der Alphabet-Liste in deren Wert um
(define (integer->alphabet int)
  (define (iter i lst)
    (cond ((null? lst) (error "Fehler!"))
          ((eq? int i) (car lst))
          (else (iter (+ i 1) (cdr lst)))))
  (iter 0 alphabet))

(<<? '(integer->alphabet 3) #\D)
(<<? '(integer->alphabet 10) #\K)
;(<<! '(integer->alphabet 27)) ;; Fehler!


;;### Aufgabenteil 2c ###
(header " Aufgabe 2c/d (string<->list)")

;; aus Vorlesung
(define (string->list str)
  (define (iter lst index)
    (if (< index 0) lst
        (iter (cons (string-ref str index) lst) (- index 1))))
  (iter '() (- (string-length str) 1)))

;; Factory: Funktion, die Strings in Integer-Listen konvertiert
(define (string->intlist char->integer)
  (lambda (str)
    (map char->integer (string->list str))))

(define string->alphabet (string->intlist alphabet->integer))

(<<? '(string->alphabet "HALLO") '(7 0 11 11 14))
(<<? '(string->alphabet "ZEBRA") '(25 4 1 17 0))


;;### Aufgabenteil 2d ###

;; aus Vorlesung
(define (list->string lst)
  (define (iter rest str)
    (if (null? rest) str
        (iter (cdr rest) (string-append str (string (car rest))))))
  (iter lst ""))

;; Factory: Funktion, die Integer-Listen in Strings konvertiert
(define (intlist->string integer->char)
  (lambda (lst)
    (list->string (map integer->char lst))))

(define alphabet->string (intlist->string integer->alphabet))

(<<? '(alphabet->string '(7 0 11 11 14)) "HALLO")
(<<? '(alphabet->string '(25 4 1 17 0)) "ZEBRA")
(<<? '(alphabet->string (string->alphabet "HALLO")) "HALLO")



;;#################
;;### Aufgabe 3 ###
;;#################

;;### Aufgabenteil 3a ###
(header "Aufgabe 3a (rekursiv)")

;; Factory: Caesar-Shift mit bestimmter Verschiebung (rekursiv)
(define (caesar-shift shift)
  (define (shift-list lst)
    (cond ((null? lst) lst)
          (else (cons (remainder (+ shift (car lst)) (length alphabet))
                      (shift-list (cdr lst))))))
  (lambda (str)
    (alphabet->string (shift-list (string->alphabet str)))))

(define my-caesar-shift (caesar-shift 3))
(define my-caesar-unshift (caesar-shift -3))

(<<? '(my-caesar-shift "HALLO") "KDOOR")
(<<? '(my-caesar-shift "ZEBRA") "CHEUD")
(<<? '(my-caesar-unshift "KDOOR") "HALLO")


;;### Aufgabenteil 3b ###
(header "Aufgabe 3b (map)")

;; Factory: Caesar-Shift mit bestimmter Verschiebung (map)
(define (caesar-shift-map shift)
  (lambda (str)
    (alphabet->string (map (lambda (int) (remainder (+ shift int) (length alphabet)))
                           (string->alphabet str)))))

(define my-caesar-shift-map (caesar-shift 3))
(define my-caesar-unshift-map (caesar-shift -3))

(<<? '(my-caesar-shift-map "HALLO") "KDOOR")
(<<? '(my-caesar-shift-map "ZEBRA") "CHEUD")
(<<? '(my-caesar-unshift-map "KDOOR") "HALLO")



;;#################
;;### Aufgabe 4 ###
;;#################
(header "Aufgabe 4a")

;; Expandiert eine Symbol-Liste (rekursiv)
(define (expand sym-list)
  (define (helper old-list count)
    (cond ((null? old-list) '())
          (else (let ((value (car old-list))
                      (rest-list (cdr old-list)))
                  (cond ((number? value) (helper rest-list value))
                        ((> count 0) (cons value (helper old-list (- count 1))))
                        (else (helper rest-list 1)))))))
  (helper sym-list 1))

(<<? '(expand '(a b c))            '(a b c))
(<<? '(expand '(a 2 b 3 c))        '(a b b c c c))
(<<? '(expand '(a 2 b 3 c a b c))  '(a b b c c c a b c))
(<<? '(expand '(10 a))             '(a a a a a a a a a a))


;;### Aufgabenteil 4b ###
(header "Aufgabe 4b")

;; Expandiert eine Symbol-Liste (rekursiv)
(define (compress sym-list)
  (define (helper old-list count)
    (cond ((null? old-list) '())
          (else (let ((value (car old-list))
                      (rest-list (cdr old-list)))
                  (cond ((and (not (null? rest-list))
                              (eq? value (car rest-list)))
                         (helper rest-list (+ count 1)))
                        ((> count 1) (cons count (cons value (helper rest-list 1))))
                        (else (cons value (helper rest-list 1))))))))
  (helper sym-list 1))

(<<? '(compress '(a b c))                '(a b c))
(<<? '(compress '(a b b c c c))          '(a 2 b 3 c))
(<<? '(compress '(a b b c c c a b c))    '(a 2 b 3 c a b c))
(<<? '(compress '(a a a a a a a a a a))  '(10 a))