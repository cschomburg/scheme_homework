#lang scheme

;; Richtigen Namespace für eval bekommen
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define cached-tests '())

;; Führe alle Tests aus
(define (run-tests)
  (define (run-test lst)
    (cond ((not (null? lst))
           (eval (car lst))
           (run-test (cdr lst)))))
  (define (iter lst)
    (cond ((not (null? lst))
           (header (caar lst))
           (run-test (cdar lst))
           (iter (cdr lst)))))
  (iter cached-tests))

;; Definiere eine Testreihe
(define (test title . tests)
  (set! cached-tests (append cached-tests (list (cons title tests)))))

;; Erweitere String mit einem Zeichen auf eine bestimmte Länge
(define (fix-length str limit expand)
  (let ((l (string-length str)))
    (cond ((> l limit) str) ((= l limit) str)
          (else (fix-length (string-append str expand) limit expand)))))

;; Gib eine Überschrift aus
(define (header s)
  (printf "~%~a~%~a~%" s (fix-length "" (string-length s) "=")))

;; Simplen Text ausgeben, die Werte folgen danach
(define (<< text)
  (printf "  ~a  =>  " text))

;; Gibt einen Ausdruck und dessen Wert aus
(define (<<! expression)
  (<< expression)
  (display (eval expression ns))
  (newline))

;; Gibt einen Ausdruck, dessen Wert und ob er mit der Erwartung übereinstimmt aus
  (define (<<? expression expected)
  (let ((value (eval expression ns)))
    (<< expression)
    (display value)
    (printf "  [~a]~%" (if (equal? value expected) "PASS" "FAIL"))))

(test "Aufgabe 1"
      '(<<! '(+ 3 5))
      '(<<? '(* 1 2) 2))

(test "Aufgabe 2"
      '(<<! '(+ 3 5))
      '(<<? '(* 1 2) 2))