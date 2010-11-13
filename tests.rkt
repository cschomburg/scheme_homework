#lang scheme

(provide start-test test)

(define curr-test-name "")
(define curr-test-i 0)

(define (start-test name)
  (set! curr-test-name name)
  (set! curr-test-i 0)
  (newline) (display "Testreihe: ") (display name) (newline))

(define (test desc value expected)
  (set! curr-test-i (+ curr-test-i 1))
  (define result "      ")
  (cond ((not (eqv? value expected)) (set! result " FAIL ")))
  (printf "~a#~s: ~a [~s|~s]~%" result curr-test-i desc expected value))