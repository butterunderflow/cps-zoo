#lang racket

(define (operator? op)
  (member op '(+ - * / = show)))

(define (cps-trans-list exs)
  (match exs
    ['() (lambda (k) (k `(void)))]
    [`(,ex) (cps-trans ex)]
    [`(,head ,@res)
     (lambda (k)
       ((cps-trans head)
        (lambda (ex)
          `(begin
             ,ex
             ,((cps-trans-list res) k)))))]))

;; cps conversion procedure. 
;; A invariant that hold during the conversion:
;; every converted expression accepts a continuation as its parameter,
;; and this continuation parameter will fill the evaluated value into
;; the correct context (at the runtime). 
;; Did some reduction at the conversion time, e.g. make cps-trans return a
;; functional object, which accept a `k` that can fill any expression
;; into some context.
;; Q: will there some duplicate computations exist?
;; A: I think it's not possible if we can gurantee these condition:
;;    1. the k is always used and only used once (if-else is the only exception, but it should be safe)
;;    2. the parameter of first stage continuation is always used and only used once
(define (cps-trans ex)
  (match ex
    [b #:when (boolean? b) (lambda (k) (k b))]
    [n #:when (number? n) (lambda (k) (k n))]
    [x #:when (symbol? x) (lambda (k) (k x))]
    [`(lambda ,x ,body)
     (lambda (k) (k `(lambda ,x
                       (lambda __k
                         ,((cps-trans body) (lambda (ex) `(__k ,ex)))))))]
    [`(if ,cd ,ex0 ,ex1)
     (lambda (k)
       ((cps-trans cd) (lambda (ex)
                         `(if ,ex
                              ,((cps-trans ex0) k)
                              ,((cps-trans ex1) k)))))]
    [`(call/cc ,ex)
     (lambda (k)
       ((cps-trans ex)
        (lambda (f) #| a expression evaluates to a function |#
          ;; Q: __v is still possible to collide with others?
          ;; A: No, because we have never capture any parameter with a new `__v`
          `((,f (lambda __v (lambda __k1
                              ,(k '__v)))) (lambda __v ,(k '__v))))))]
    [`(reset ,ex)
     (lambda (k) (k ((cps-trans ex) (lambda (v) v))))]
    [`(shift ,ex)
     (lambda (k)
       ((cps-trans ex)
        (lambda (ex_f)
          `(;; continuation-composing style:
            ;; when this functional argument is applied,
            ;; we compose k1 and k while incur a nested call (`(k1 (k v))`)
            (,ex_f (lambda __v (lambda __k1 (__k1 ,(k '__v)))))
            ;; continuation at `shift` site is shifted, so we pass an `identity` function here
            (lambda v v)))))]
    [`(begin ,@exs)
     (cps-trans-list exs)]
    [`(,op ,arg0 ,arg1) #:when (operator? op)
                        (lambda (k)
                          ((cps-trans arg0)
                           (lambda (ex0)
                             ((cps-trans arg1)
                              (lambda (ex1)
                                (k `(,op ,ex0 ,ex1)))))))]
    ;; TODO: make operator application consistent with learn0.rkt
    [`(,op ,ex) #:when (operator? op)
                (lambda (k)
                  ((cps-trans ex) (lambda (ex0) (k `(,op ,ex0)))))]
    [`(,op ,ex)
     (lambda (k)
       ((cps-trans op) (lambda (op_ex)
                         ((cps-trans ex) (lambda (ex1)
                                           `((,op_ex ,ex1) (lambda __v ,(k '__v))))))))]))
(require "cps-learn0.rkt")

(define (eval-after-trans ex)
  (eval-top ((cps-trans ex) (lambda (v) v))))

(provide cps-trans)
(provide eval-after-trans)

