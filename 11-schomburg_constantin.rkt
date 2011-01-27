#lang scheme

(require "OO-pack.rkt")

;; Programmieren (Scheme) - WS 2010/11 - Übung 11
;; 21.01.11 - 28.01.11
;; Constantin Schomburg

;; Ausgaben-Formatierung
(define-namespace-anchor nsa)(define ns (namespace-anchor->namespace nsa))(define (fix-length str limit expand)(let ((l (string-length str)))(cond ((> l limit) str) ((= l limit) str)(else (fix-length (string-append str expand) limit expand)))))(define (header s)(printf "~%~a~%~a~%" s (fix-length "" (string-length s) "=")))(define (<< text)(printf "  ~a  =>  " text))(define (<<! expression)(<< expression)(display (eval expression ns))(newline))(define (<<? expression expected)(let ((value (eval expression ns)))(<< expression)(display value)(printf "  [~a]~%" (if (equal? value expected) "PASS" "FAIL"))))

;;#################
;;### Aufgabe 1 ###
;;#################
(header "Aufgabe 1")


;;### Aufgabe 1 a ###

(class (3DObject)
  (attribute _x)
  (attribute _y)
  (attribute _z)
  
  (method (init)
          (set! _x 0)
          (set! _y 0)
          (set! _z 0))
  
  (method (x) _x)
  (method (y) _y)
  (method (z) _z)

  (method (move-to x y z)
          (set! _x x)
          (set! _y y)
          (set! _z z))
  
  (method (volume) (error "Abstract class: Implement volume!"))
)


;;### Aufgabe 1b ###

(class (Sphere 3DObject)
  (attribute _radius)
  
  (method (init x y z radius)
          (send super init)
          (send self move-to x y z)
          (set! _radius radius))
  
  (method (volume)
          (* 4/3 3.14159 _radius _radius _radius))
)

(define (create-Sphere x y z radius)
  (create-instance Sphere x y z radius))


(define s (create-Sphere 2 3 4 5))
(<<? '(send s volume) 523.5983333333332)


;;### Aufgabe 1c / 1d ###

(class (Box 3DObject)
  (attribute _x2)
  (attribute _y2)
  (attribute _z2)
  
  (method (init x1 y1 z1 x2 y2 z2)
          (send super init)
          (send super move-to x1 y1 z1)
          (set! _x2 x2)
          (set! _y2 y2)
          (set! _z2 z2))
  
  (method (volume)
          (* (abs (- _x2 (send self x)))
             (abs (- _y2 (send self y)))
             (abs (- _z2 (send self z)))))
  
  (method (move-to x y z)
          (let ((x-diff (- _x2 (send self x)))
                (y-diff (- _y2 (send self y)))
                (z-diff (- _z2 (send self z))))
            (send super move-to x y z)
            (set! _x2 (+ x x-diff))
            (set! _y2 (+ y y-diff))
            (set! _z2 (+ z z-diff))))
)

(define (create-Box x1 y1 z1 x2 y2 z2)
  (create-instance Box x1 y1 z1 x2 y2 z2))

(define b (create-Box 1 2 3 4 5 6))
(<<? '(send b volume) 27)
(send b move-to 5 4 7)
(<<? '(send b volume) 27)



;;#################
;;### Aufgabe 2 ###
;;#################


;;### Aufgabe 2a ###
;; Abstrakte Klasse, da folgende Methoden nicht implementiert sind:
;;   (accepted?) => bool
;;   (handle input-var) => bool

(class (AbstractMachine)
  (attribute _input)
  
  (method (input) _input)
  
  (method (run input)
    (set! _input input)
    (send self loop-over-input))
  
  (method (loop-over-input)
    (cond ((null? _input) (send self accepted?))
          ((send self handle (car _input)) 
              (set! _input (cdr _input)) (loop-over-input))
          (else #f))))


;;### Aufgabe 2b ###

(class (AbstractStateMachine AbstractMachine)
  (attribute _state)
  (attribute _end-states)
  
  (method (init)
          (set! _end-states '()))

  (method (state) _state)
  (method (set-state! state) (set! _state state))

  (method (add-accept-state state)
          (set! _end-states (cons state _end-states)))
  (method (accepted?)
          (if (member _state _end-states) #t #f))
  
  (method (run input)
          (send self set-state! 'start)
          (send super run input))
)

(header "Aufgabe 2b")
(define asm (create-instance AbstractStateMachine))
(send asm add-accept-state 'end)
(<<? '(send asm accepted?) #f)
(send asm set-state! 'end)
(<<? '(send asm state) 'end)
(<<? '(send asm accepted?) #t)


;; ### Aufgabe 2c ###

(class (FiniteStateMachine AbstractStateMachine)
  (attribute _transitions)
  
  (method (init)
          (send super init)
          (set! _transitions '()))
  
  (method (add-transition from-state with-character to-state)
          (set! _transitions (cons (list from-state
                                         with-character
                                         to-state)
                                   _transitions)))
  
  (method (handle character)
          (let ((state (send self state)))
            (define (iter transitions)
              (if (null? transitions) #f
                  (let ((transition (car transitions)))
                    (cond ((and (eq? (car transition) state)
                             (eq? (cadr transition) character))
                           (send self set-state! (caddr transition))
                           #t)
                          (else (iter (cdr transitions)))))))
            (iter _transitions)))
)
(define (create-FiniteStateMachine) (create-instance FiniteStateMachine))

(define machine-1 (create-FiniteStateMachine))
(send machine-1 add-accept-state 'ok)
(send machine-1 add-transition 'start 1 'start)
(send machine-1 add-transition 'start 2 'warte-auf-3)
(send machine-1 add-transition 'start 3 'ok)
(send machine-1 add-transition 'warte-auf-3 2 'warte-auf-3)
(send machine-1 add-transition 'warte-auf-3 3 'ok)
(send machine-1 add-transition 'ok 3 'ok)

(header "Aufgabe 2c")
(<<? '(send machine-1 run '())              #f)
(<<? '(send machine-1 run '(1 2))           #f)
(<<? '(send machine-1 run '(1 2 3))         #t)
(<<? '(send machine-1 run '(1 2 3 1 3))     #f)
(<<? '(send machine-1 run '(3))             #t)
(<<? '(send machine-1 run '(2 3))           #t)
(<<? '(send machine-1 run '(1 3))           #t)
(<<? '(send machine-1 run '(1 1 2 2 3 3 3)) #t)


;;### Aufgabe 2d ###

(define machine-2 (create-FiniteStateMachine))
(send machine-2 add-accept-state 'char-a)
(send machine-2 add-accept-state 'char-b)
(send machine-2 add-transition 'start  'a 'char-a)
(send machine-2 add-transition 'start  'b 'char-b)
(send machine-2 add-transition 'char-a 'b 'char-b)
(send machine-2 add-transition 'char-b 'a 'char-a)

(header "Aufgabe 2d")
(<<? '(send machine-2 run '())          #f)
(<<? '(send machine-2 run '(a))         #t)
(<<? '(send machine-2 run '(b a b a b)) #t)
(<<? '(send machine-2 run '(a b c))     #f)
(<<? '(send machine-2 run '(a b a a))   #f)