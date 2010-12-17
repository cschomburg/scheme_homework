#lang scheme

;; Programmieren (Scheme) - WS 2010/11 - Übung 08 (Weihnachtsaufgabe)
;; 17.12.10 - 07.01.10
;; Constantin Schomburg

;; Ausgaben-Formatierung
(define-namespace-anchor a)(define ns (namespace-anchor->namespace a))(define cached-tests '())(define (run-tests)(define (run-test lst)(cond ((not (null? lst))(eval (car lst))(run-test (cdr lst)))))(define (iter lst)(cond ((not (null? lst))(header (caar lst))(run-test (cdar lst))(iter (cdr lst)))))(iter cached-tests))(define (test title . tests)(set! cached-tests (append cached-tests (list (cons title tests)))))(define (fix-length str limit expand)(let ((l (string-length str)))(cond ((> l limit) str) ((= l limit) str)(else (fix-length (string-append str expand) limit expand)))))(define (header s)(printf "~%~a~%~a~%" s (fix-length "" (string-length s) "=")))(define (<< text)(printf "  ~a  =>  " text))(define (<<! expression)(<< expression)(display (eval expression ns))(newline))(define (<<? expression expected)(let ((value (eval expression ns)))(<< expression)(display value)(printf "  [~a]~%" (if (equal? value expected) "PASS" "FAIL"))))


(require (lib "graphics.ss" "graphics"))

(open-graphics)
(define pi 3.141592653589793238462)

(define TODO '...)

;; GEGEBEN
(define (make-production left right) (cons left right))
(define (get-leftpart production) (car production))
(define (get-rightpart production) (cdr production))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Aufgabe (Suchen von Anwendbaren Produktionen)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;1.a)

(define (random-select lst)
  (define (iter lst count)
    (if (= count 0)
        (car lst)
        (iter (cdr lst) (- count 1))))
  (iter lst (random (length lst))))

; TESTEN
(test "Aufgabe 1a"
      '(<<! '(random-select '(1 2 3 4 5 6))) ; -> eine zufaellige Zahl zwischen 1 und 6
      '(<<! '(random-select '(A B)))) ; -> zufaellig das Symbol A oder B

;1.b)

(define (find-rightparts symbol productions)
  (define (iter productions return-list)
    (cond ((null? productions) return-list)
          ((eq? (get-leftpart (car productions)) symbol)
           (iter (cdr productions)
                 (cons (get-rightpart (car productions))
                       return-list)))
          (else (iter (cdr productions) return-list))))
  (iter productions '()))

; TESTEN
(test "Aufgabe 1b"
      '(<<? '(find-rightparts 'A (list (make-production 'A '[B C])
                                       (make-production 'D '[E F])))
            '((B C)))
      '(<<? '(find-rightparts 'D (list (make-production 'A '[B C])
                                       (make-production 'D '[E F])))
            '((E F)))
      '(<<? '(find-rightparts 'E (list (make-production 'A '[B C])
                                       (make-production 'D '[E F])))
            '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Aufgabe (Erstellen von Tabellen)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;2.a)

(define (delete-table symbol)
  '())

(define (identity-table symbol)
  (list symbol))

; TESTEN
(test "Aufgabe 2a"
      '(<<? '(delete-table 'A)      '())
      '(<<? '(identity-table 'A)    '(A)))

;2.b)

(define (make-table base-table . productions)
  (lambda (symbol)
    (let ((rightparts (find-rightparts symbol productions)))
      (if (null? rightparts)
          (base-table symbol)
          (random-select rightparts)))))

(define table1
  (make-table identity-table
   (make-production 'A '[B C])
   (make-production 'D '[E F])))

(define table2
 (make-table identity-table
  (make-production 'F '[X ( + F) (- F)])))

; TESTEN
(test "Aufgabe 2b"
      '(<<? '(table1 'A)    '(B C))
      '(<<? '(table1 'D)    '(E F))
      '(<<? '(table1 'E)    '(E)))

;2.c)

(define (append-table base-table symbols . rightpart)
  (lambda (symbol)
    (define (iter symbols)
      (cond ((null? symbols) (base-table symbol))
            ((eq? (car symbols) symbol) rightpart)
            (else (iter (cdr symbols)))))
    (iter symbols)))

; TESTEN
(test "Aufgabe 2c"
      '(<<? '((append-table table1 '(E F) 'X 'Y) 'E)    '(X Y))
      '(<<? '((append-table table1 '(E F) 'X 'Y) 'F)    '(X Y))
      '(<<? '((append-table table1 '(E F) 'X 'Y) 'A)    '(B C))
      '(<<? '((append-table table1 '(E F) 'X 'Y) 'G)    '(G)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Aufgabe (Anwendung eines Termersetzungsschritts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply word table)
  (define (helper word new-word)
    (cond ((null? word) new-word)
          ((symbol? (car word)) (helper (cdr word)
                                        (append new-word (table (car word)))))
          (else (helper (cdr word)
                        (append new-word (list (helper (car word) '())))))))
  (helper word '()))

; TESTEN
(test "Aufgabe 3"
      '(define word1 '(F))
      '(define word2 (apply word1 table2))
      '(define word3 (apply word2 table2))
      
      '(<<? 'word2    '(X (+ F) (- F)))
      '(<<? 'word3    '(X (+ X (+ F) (- F)) (- X (+ F) (- F)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Aufgabe (Iteratives Auswerten eines Lindenmayer-Systems)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lsystem axiom tables n)
  (if (= n 0)
      axiom
      (lsystem (apply axiom (random-select tables)) tables (- n 1))))
      

; TESTEN
(test "Aufgabe 4"
      '(<<? '(lsystem '(F) (list table2) 0)    '(F))
      '(<<? '(lsystem '(F) (list table2) 1)    '(X (+ F) (- F)))
      '(<<? '(lsystem '(F) (list table2) 2)    '(X (+ X (+ F) (- F)) (- X (+ F) (- F))))
      '(<<? '(lsystem '(F) (list table2) 3)    '(X (+ X (+ X (+ F) (- F)) (- X (+ F) (- F))) (- X (+ X (+ F) (- F)) (- X (+ F) (- F)))))
      
      '(define lsystem3 (lsystem '(F) (list table2) 15)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Aufgabe (Konstruktoren und Selektoren fuer Turtle-Grafik)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;5.a)

(define (make-state viewport posn scale-x scale-y angle)
  (lambda (msg)
    (cond ((eq? msg 'viewport) viewport)
          ((eq? msg 'posn) posn)
          ((eq? msg 'scale-x) scale-x)
          ((eq? msg 'scale-y) scale-y)
          ((eq? msg 'angle) angle))))

(define (get-viewport state) (state 'viewport))
(define (get-posn state) (state 'posn))
(define (get-scale-x state) (state 'scale-x))
(define (get-scale-y state) (state 'scale-y))
(define (get-angle state) (state 'angle))

(define state0 (make-state "Viewport" (make-posn 100 200) 1.5 2 45))

; TESTEN
(test "Aufgabe 5a"
      '(<<? '(get-viewport state0)         "Viewport")
      '(<<? '(posn-x (get-posn state0))    100)
      '(<<? '(posn-y (get-posn state0))    200)
      '(<<? '(get-scale-x state0)          1.5)
      '(<<? '(get-scale-y state0)          2)
      '(<<? '(get-angle state0)            45))

;5.b)

(define (scale factor)
  (lambda (state)
    (make-state (get-viewport state)
                (get-posn state)
                (* (get-scale-x state) factor)
                (* (get-scale-y state) factor)
                (get-angle state))))

(define (translate dx dy)
  (lambda (state)
    (make-state (get-viewport state)
                (make-posn (+ (posn-x (get-posn state)) dx)
                           (+ (posn-y (get-posn state)) dy))
                (get-scale-x state)
                (get-scale-y state)
                (get-angle state))))

(define (rotate angle)
  (lambda (state)
    (make-state (get-viewport state)
                (get-posn state)
                (get-scale-x state)
                (get-scale-y state)
                (+ (get-angle state) angle))))

; TESTEN
(test "Aufgabe 5b"
      '(<<? '(get-scale-x ((scale 2) state0))                  3.0)
      '(<<? '(get-scale-y ((scale 2) state0))                  4)
      '(<<? '(posn-x (get-posn ((translate 50 75) state0)))    150)
      '(<<? '(posn-y (get-posn ((translate 50 75) state0)))    275)
      '(<<? '(get-angle ((rotate 90) state0))                  135))

;5.c)

;; Wandelt Koordinaten relativ zu einem Zustand in eine absolute Zielposition,
;; abhängig von der Position, der Richtung und der Skalierung des Zustands
;;   @param state <state> der Ausgangszustand
;;   @param dx <number> die X-Koordinate relativ zum Zustand
;;   @param dy <number> die Y-Koordinate relativ zum Zustand
;;   @return <posn> die absolute Zielposition
(define (apply-state state dx dy) 
  (let ((posn (get-posn state)))
    (make-posn
     (+ (posn-x posn) (* (get-scale-x state) dy (sin (/ (* (get-angle state) pi) 180))) (* (get-scale-x state) dx (cos (/ (* (get-angle state) pi) 180))))
     (+ (posn-y posn) (- (* (get-scale-x state) dy (cos (/ (* (get-angle state) pi) 180)))) (* (get-scale-x state) dx (sin (/ (* (get-angle state) pi) 180)))))))

;; Erzeugt eine Funktion, die einen neuen Zustand relativ verschoben zu einem alten erzeugt,
;; abhängig von den vorher festgelegten relativen Koordinaten
;;   @param dx <number> die X-Koordinate relativ zum alten Zustand
;;   @param dy <number> die Y-Koordinate relativ zum alten Zustand
;;   @return <procedure(state) => state> die Verschiebungsfunktion
(define (translate-local-state dx dy) 
  (lambda (state)
    (make-state
     (get-viewport state) 
     (apply-state state dx dy)
     (get-scale-x state) (get-scale-y state) (get-angle state))))

;; Step ist eine Ausprägung von translate-local-state,
;; die einen Zustand relativ um 1 in Y-Richtung verschiebt
(define step (translate-local-state 0.0 1.0))

;; Zeichnet von einem Zustand ausgehend eine Linie in 1 relative Y-Richtung und gibt den alten Zustand zurück
(define line
  (lambda (state)
    ((draw-line (get-viewport state)) (get-posn state) (apply-state state 0.0 1.0))
    state))

;; Liefert eine Funktion zurück, die die gleiche Linie wie in 'line' zeichnet,
;; allerdings mit einer vorher festgelegten Farbe
(define (colored-line color)
  (lambda (state)
    ((draw-line (get-viewport state)) (get-posn state) (apply-state state 0.0 1.0) color)
    state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Aufgabe (Zeichnen von Turtle-Grafik)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-word word state)
  (if (null? word)
      state
      (let ((element (car word))
            (rest-word (cdr word)))
        (cond ((procedure? element) (draw-word rest-word (element state)))
              ((list? element)
               (draw-word element state)
               (draw-word rest-word state))
              (else (error "Unknown element: " element))))))

; TESTEN
(test "Aufgabe 6"
      '(<<! '(draw-word (list line step (rotate 240) line step (rotate 240) line)
                        (make-state (open-viewport "Test" 600 600) (make-posn 50 475) 500 500 90)))
      '(<<! '(draw-word (list line step 
                              (list (scale 0.5) (rotate -60) line step (rotate -60) line step) 
                              line step (rotate -120) line step 
                              (list (scale 0.5) (rotate -60) line step (rotate -60) line step) 
                              line step (rotate -120) line step 
                              (list (scale 0.5) (rotate -60) line step (rotate -60) line step)
                              line step)
                        (make-state (open-viewport "Test" 600 600) (make-posn 150 500) 200 200 60))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Zeichnen eines L-Systems)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (turtle-table factor angle)
    (make-table delete-table
     (make-production '+ (list (rotate angle)))
     (make-production '- (list (rotate (- angle))))
     (make-production '* (list (scale factor)))
     (make-production '/ (list (scale (/ 1 angle))))))

(define (run-lsystem start productions n start-size draw-table width height x y start-angle)
  (draw-word 
   (apply (lsystem start productions n) draw-table)
   (make-state (open-viewport "L-System" width height) 
               (make-posn x y) start-size start-size start-angle)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beispiele
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Kochkurve
(define (kochkurve n)
  (run-lsystem '(F + + F + + F)
               (list (make-table identity-table
                                 (make-production 'F '[F - F + + F - F])))
               n (* (expt 0.333 n) 420)
               (append-table (turtle-table 1.0 60.0) '(F) line step)
               600 600 180 500 0))

; Drachenkurve
(define (drachenkurve n)
  (run-lsystem '(R)
               (list (make-table identity-table
                                 (make-production 'R '[- R + + L])
                                 (make-production 'L '[R - - L +])))
               n (* (expt 0.705 n) 350)
               (append-table (turtle-table 1.0 45.0) '(L R) line step)
               600 600 350 450 0))

; Sierpinski Dreieck
(define (sierpinski n)
  (run-lsystem '(F x F - - F F - - F F)
               (list (make-table identity-table
                                 (make-production 'F '[F F])
                                 (make-production 'x '[- - F x F + + F x F + + F x F - -])))
               n (/ 250 (expt 2 n))
               (append-table (turtle-table 1.0 60.0) '(F) line step)
               600 600 50 500 90))


(define (pflanzen n)
  ; Einfacher Pflanzenwachstum [S. 94]
  (cond ((= n 1)
         (run-lsystem
          '(F)
          (list (make-table identity-table
                            (make-production 'F '[F F (- F) (+ F (- - F) F)])))
          6 2.3 
          (append-table (turtle-table 1.0 25.0) '(F) line step)
          600 600 300 500 0))
        
        ((= n 2)
         (run-lsystem
          '(F)
          (list (make-table identity-table
                            (make-production 'F '[F F (+ F + F) (- - F)])))
          7 1.5 
          (append-table (turtle-table 1.0 20.0) '(F) line step)
          600 600 300 500 0))
        
        ((= n 3)
         (run-lsystem
          '(F)
          (list (make-table identity-table
                            (make-production 'F '[F (- - F) F (+ + F - (F - F)) (- F - (F - F))])))
          5 3.5
          (append-table (turtle-table 1.0 18.0) '(F) line step)
          600 600 300 500 0))
        
        ((= n 4)
         (run-lsystem
          '(F)
          (list (make-table identity-table
                            (make-production 'F '[F (+ + (F) F) F (- F F)])))
          7 1.3
          (append-table (turtle-table 1.0 18.0) '(F) line step)
          600 600 300 500 0))
        
        ; Zusaetzlicher Stammwachstum ueber Nichtterminal (F -> F F) [S. 95]
        ((= n 5)
         (run-lsystem
          '(A)
          (list (make-table identity-table
                            (make-production 'A '[F (- - A) F (+ + + A) (F + (A))])
                            (make-production 'F '[F F])))
          7 1.2 
          (append-table (turtle-table 1.0 12.5) '(A F) line step)
          600 600 300 500 0))
        
        ((= n 5)
         (run-lsystem
          '(A)
          (list (make-table identity-table
                            (make-production 'A '[(- F (- A) + + A) (+ + F A) B])
                            (make-production 'B '[F A])
                            (make-production 'F '[F F])))
          7 3
          (append-table (turtle-table 1.0 20.0) '(A B F) line step)
          600 600 260 500 0))
        
        ((= n 6)
         (run-lsystem
          '(A)
          (list (make-table identity-table
                            (make-production 'A '[(A) F (- B) (+ B) (+ F + A)])
                            (make-production 'B '[F (+ A) (- B B) F (- F - B)])
                            (make-production 'F '[F F])))
          7 1.7
          (append-table (turtle-table 1.0 15.0) '(A B F) line step)
          600 600 260 500 0))
        
        ((= n 7)
         (run-lsystem
          '(A)
          (list (make-table identity-table
                            (make-production 'A '[(A) F (- A (- A)) (+ A) (+ F (+ A))])
                            (make-production 'F '[F F])))
          6 3.5
          (append-table (turtle-table 1.0 12.5) '(A F) line step)
          600 600 260 500 0))
        
        ; Zufaellige Wachstumsprozesse
        ; Pflanzenwachstum [S. 96-97] (zufaelliger Wachstum durch 2 Tabellen)
        ((= n 8)
         (run-lsystem
          '(S)
          (list (make-table identity-table
                            (make-production 'S '[S (- A) S (+ B) (- S)])
                            (make-production 'A '[S (- S - (F A A))])
                            (make-production 'B '[S (+ S + (F B B))])
                            (make-production 'F '[F F]))
                (make-table identity-table
                            (make-production 'S '[S (+ B) S (- A) (+ S)])
                            (make-production 'A '[(- S - (A A))])
                            (make-production 'B '[(+ S + (B B))])
                            (make-production 'F '[F F])))
          7 1.5
          (append-table (turtle-table 1.0 12.5) '(S F) line step)
          600 600 300 500 0))
        
        ; Pflanzenwachstum [S. 93] (zufaelliger Wachstum durch 2 Tabellen)
        ((= n 9)
         (run-lsystem
          '(F)
          (list (make-table identity-table
                            (make-production 'F '[F F (- F F) (+ F (- F))]))
                (make-table identity-table
                            (make-production 'F  '[F F (+ F F) (- F (+ F))])))
          6 2.3 
          (append-table (turtle-table 1.0 20.0) '(F) line step)
          600 600 300 500 0))
         
        ; Pflanzenwachstum (zufaelliger Wachstum durch 3 Produktionen fuer das Symbol F)
        ((= n 10)
         (run-lsystem
          '(F)
          (list (make-table identity-table
                            (make-production 'F '[F (+ F) F (- F) F])
                            (make-production 'F '[F (+ F) F])
                            (make-production 'F '[F (- F) F])))
          6 2.3 
          (append-table (turtle-table 1.0 30.0) '(F) line step)
          600 600 300 500 0))))

; Barnsley Farn [S. 101]
(define (barnsley)
  (run-lsystem
   '(A)
   (list (make-table identity-table
                     (make-production 'A  '[F (s0.35 r+20 B) F (s0.35 r-20 A) s0.85 r+1 A])
                     (make-production 'B  '[F (s0.35 r-20 A) F (s0.35 r+20 B) s0.85 r-1 B])))
   9 40
   (append-table 
    (make-table delete-table
                (make-production 'r+1 (list (rotate (* 1 2.5))))
                (make-production 'r-1 (list (rotate (* -1 2.5))))
                (make-production 'r+20 (list (rotate (* 20 2.5))))
                (make-production 'r-20 (list (rotate (* -20 2.5))))
                (make-production 's0.35 (list (scale 0.35)))
                (make-production 's0.85 (list (scale 0.85))))
    '(F A B) line step)
   600 600 300 500 0))
         

; ; Komplexere Darstellung durch erweiterte Zeicheroutinen
(define (komplex)
  (define shrink 0.9)
  (define trunk-width 0.06)
  
  (define trunk
    (lambda (state)
      ((draw-solid-polygon (get-viewport state)) 
       (list (apply-state state (- trunk-width) 0.0) 
             (apply-state state trunk-width 0.0) (apply-state state (* shrink trunk-width) 1.0) (apply-state state (* shrink (- trunk-width)) 1.0)) (make-posn 0 0) "brown")
      state))
  
  (define leaf
    (lambda (state)
      ((draw-solid-polygon (get-viewport state)) 
       (list (get-posn state) (apply-state state 0.2 0.4) (apply-state state 0.00 1.0) (apply-state state -0.2 0.4)) (make-posn 0 0) "green")
      state))
  
  ; Barnsley Farn [S. 101]
  (run-lsystem
   '(B)
   (list (make-table identity-table 
                     (make-production 'B '[S S S S S S (- B) (+ B)])
                     (make-production 'B '[S S S S (- B) (+ B)])
                     (make-production 'B '[S S (- B) (+ B)])
                     (make-production 'B '[S B])
                     (make-production 'S '[S])
                     (make-production 'S '[T])
                     (make-production 'T '[T])
                     (make-production 'S '[S (- B)])
                     (make-production 'S '[S (+ B)])
                     ))
   8 60
   (make-table delete-table
               (make-production '+ (list (rotate 20.5)))
               (make-production '- (list (rotate -20.5)))
               (make-production 'S (list trunk (scale shrink) step))
               (make-production 'T (list trunk (scale shrink) step))
               (make-production 'B (list leaf)))
   600 600 300 550 0))

(header "Mögliche Optionen:")
(display " * (run-tests) : Durchlaufe Test-Ausgaben\n\n")
(display " - (kochkurve 3..5)\n")
(display " - (drachenkurve 5..14)\n")
(display " - (sierpinski 2..6)\n")
(display " - (pflanzen 1..10)\n")
(display " - (barnsley)")
(display " - (komplex)")