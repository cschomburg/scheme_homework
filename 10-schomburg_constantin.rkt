#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Übung 10
;; 14.01.11 - 21.01.11
;; Constantin Schomburg

(define ... 'TODO)
(require (file "OO-pack.rkt"))
(require scheme/mpair)

;; Ausgaben-Formatierung
(define-namespace-anchor nsa)(define ns (namespace-anchor->namespace nsa))(define (fix-length str limit expand)(let ((l (string-length str)))(cond ((> l limit) str) ((= l limit) str)(else (fix-length (string-append str expand) limit expand)))))(define (header s)(printf "~%~a~%~a~%" s (fix-length "" (string-length s) "=")))(define (<< text)(printf "  ~a  =>  " text))(define (<<! expression)(<< expression)(display (eval expression ns))(newline))(define (<<? expression expected)(let ((value (eval expression ns)))(<< expression)(display value)(printf "  [~a]~%" (if (equal? value expected) "PASS" "FAIL"))))



;;#################
;;### Aufgabe 1 ###
;;#################
;; siehe Zeichnung

(define (twice f) (f) (f))

(define (create-counter) 
  (let ((val 0))
    (lambda () (set! val (+ val 1)) val)))

(define c (create-counter)) 
;(twice c)

;; Die Funktion greift auf die Variable "val" zu, die zu einem höheren Gültigkeitsbereich gehört, aber nicht zum Global Environment.



;;#################
;;### Aufgabe 2 ###
;;#################

(define (mlist-set! lst index value)
  (if (mpair? lst)
      (cond ((= index 0) (set-mcar! lst value))
            (else (mlist-set! (mcdr lst) (- index 1) value)))))

(header "Aufgabe 2")
(define a (mlist 'a 'b 'c))
(<< "a vorher") (display a) (display "\n")
(mlist-set! a 2 'z)
(<< "nach (mlist-set! a 2 'z)") (display a) (display "\n")



;;###############
;;### Gegeben ###
;;###############
(header "Aufgabe 3a")

(class (BasicMachine)
  (attribute _program)
  (attribute _counter 0)
  (attribute _registers (mlist 0 0 0 0))
  
  (method (init program argument)
          (set! _program program)
          (send self set-register! 0 argument))
  
  (method (register index) (mlist-ref _registers index))
  (method (set-register! index value) (mlist-set! _registers index value))
  
  (method (run)
          ;(send self inspect)
          (if (and (>= _counter 0) (< _counter (length _program)))
              (let ((fetched-instruction (list-ref _program _counter)))
                (set! _counter (+ _counter 1))
                (send self call fetched-instruction)
                (send self run))
              (send self register 0)))
  
  (method (call instruction)
          (apply send-msg self instruction))
  
  (method (sto r1 v1) (send self set-register! r1 v1))
  (method (mov r1 r2) (send self set-register! r1 (send self register r2)))
  (method (add r1 r2) (send self set-register! r1 (+ (send self register r1)
                                                     (send self register r2))))
  (method (jmp v1) (set! _counter v1))
)

(define (run-program machine-class program arg)
  (send (create-instance machine-class program arg) run))

(define quadruple '((mov 1 0) (add 0 1) (add 0 1) (add 0 1)))
(<<? '(run-program BasicMachine quadruple 25) 100)



;;#################
;;### Aufgabe 3 ###
;;#################

(class (ExtendedMachine BasicMachine)
  (method (mul r1 r2) (send self set-register! r1 (* (send self register r1)
                                                     (send self register r2))))
  (method (swap r1 r2) (let ((tmp (send self register r1)))
                         (send self mov r1 r2)
                         (send self sto r2 tmp)))
  (method (jz v1 r1) (if (= 0 (send self register r1))
                         (send self jmp v1))))

(define square-plus-one '((mul 0 0) (sto 1 1) (add 0 1)))
(<<? '(run-program ExtendedMachine square-plus-one 8) 65)

;;### Aufgabe 3b ###
(header "Aufgabe 3b")

;; Register 0: Zwischenergebnis
;; Register 1: Counter
;; Register 2: -1
(define fact '((mov 1 0)  ;; 0 - Counter auf n initialisieren
               (sto 0 1)  ;; 1 - Zwischenergebnis auf 1 initialisieren
               (sto 2 -1) ;; 2 - Register 2 auf -1 setzen
               (jz 7 1)   ;; 3 - Wenn Counter 0, dann springe zum Ende
               (mul 0 1)  ;; 4 - Multipliziere Counter auf das Zwischenergebnis
               (add 1 2)  ;; 5 - Vermindere Counter um 1
               (jmp 3)))  ;; 6 - Springe zurück zur Abbruchbedingung

(<<? '(run-program ExtendedMachine fact 5) 120)
(<<? '(run-program ExtendedMachine fact 1) 1)
(<<? '(run-program ExtendedMachine fact 0) 1)
