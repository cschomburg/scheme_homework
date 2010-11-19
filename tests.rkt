#lang scheme

(provide test)

(define (fix-length str limit)
  (let ((l (string-length str)))
    (cond ((> l limit) str)
          ((= l limit) str)
          (else (fix-length (string-append str " ") limit)))))

(define (test verbose test-func)
  (let ((row-name "Undefined"))
    (test-func (lambda (name)
                 (set! row-name name)
                 (cond (verbose (printf "~%Test row: ~a~%" name))))
               (lambda (desc value expected)
                 (let* ((result (eqv? value expected)))
                   (cond ((or verbose (not result))
                          (printf " ~a ~a [~s|~s]~%"
                                  (if result "    " "FAIL")
                                  (fix-length desc 25)
                                  expected
                                  value))))))))