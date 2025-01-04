#lang racket
(require "cps-learn4.rkt")
(require test-engine/racket-tests)

;; (println (cps-trans #t))
;; todo: find some test frame work that easier to review transformation and update the expected result

(check-expect (eval-after-trans #t) #t)

(check-expect (eval-after-trans #t) #t)

(check-random (eval-after-trans (random 100)) (random 100))

(check-expect (eval-after-trans '((lambda x x) 1)) 1)

(check-expect (eval-after-trans '((lambda x (+ x 3)) 1)) 4)

(check-expect (eval-after-trans '(if (= 1 2) 7 3)) 3)

(check-expect
 (eval-after-trans '(+ (call/cc (lambda return (+ 9999 (return (* 3 3))))) (* 4 4)))
 25)

(check-expect
 (eval-after-trans '(+ (call/cc (lambda k 1)) 1))
 2)

(check-expect (with-output-to-string
                (lambda ()
                  (eval-after-trans '((lambda x (show x)) 10))))
              "10\n")

;; redirect eval's output to a string
(define (eval-after-trans-wrapped exp)
  (with-output-to-string
    (lambda ()
      (eval-after-trans exp))))

(check-expect
 (eval-after-trans-wrapped
  '(begin
     (show 11)
     (show 12)))
 "11\n12\n")

(check-expect
 (eval-after-trans
  '(begin
     1))
 1)

(test)
