#lang racket
(require "cps-learn0.rkt")
(require test-engine/racket-tests)
(check-expect (eval-top #t) #t)

(check-random (eval-top (random 100)) (random 100))

(check-expect (eval-top '((lambda x x) 1)) 1)

(check-expect (eval-top '((lambda x (+ x 3)) 1)) 4)

(check-expect (eval-top '(if (= 1 2) 7 3)) 3)

(check-expect (with-output-to-string
                (lambda ()
                  (eval-top '((lambda x (show x)) 10))))
              "10\n")

;; redirect eval's output to a string
(define (eval-top-wrapped exp)
  (with-output-to-string
    (lambda ()
      (eval-top exp))))

(check-expect
 (eval-top-wrapped
  '(begin
     (show 11)
     (show 12)))
 "11\n12\n")

(check-expect
 (eval-top
  '(begin
     1))
 1)
(test)
