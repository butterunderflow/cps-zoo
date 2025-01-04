#lang racket

(define (operator? op)
  (member op '(+ - * / = show)))

(define (cps-trans-list exs)
  (match exs
    ['() `(lambda k (k (void)))]
    [`(,ex) `(lambda k (,(cps-trans ex) k))]
    [`(,head ,@res)
     `(lambda k
        (,(cps-trans head)
         (lambda v
           (,(cps-trans-list res) k))))]))

;; cps conversion procedure. 
;; A invariant that hold during the conversion:
;; every converted expression accepts a continuation as its parameter,
;; and this continuation parameter will fill the evaluated value into
;; the correct context (at the runtime). 
(define (cps-trans ex)
  (match ex
    [b #:when (boolean? b) `(lambda k (k ,b))]
    [n #:when (number? n) `(lambda k (k ,n))]
    [(? symbol?) `(lambda k (k ,ex))]
    [`(lambda ,x ,body)
     `(lambda k (k (lambda ,x ,(cps-trans body))))]
    [`(if ,cd ,ex0 ,ex1)
     `(lambda k
        (,(cps-trans cd) (lambda v
                           (if v
                               (,(cps-trans ex0) k)
                               (,(cps-trans ex1) k)))))]
    ;; TODO: shift/reset
    [`(call/cc ,ex)
     `(lambda k
        (,(cps-trans ex)
         (lambda f
           ((f (lambda v (lambda _k1
                           ;; _k1 contains the computation from the continuation parameter's call site
                           ;; to call/cc's call site, when the continuation parameter is invoked,
                           ;; _k1 should be ignored. 
                           (k v)))) k))))]
    [`(begin ,@exs)
     (cps-trans-list exs)]
    [`(,op ,arg0 ,arg1) #:when (operator? op)
                        `(lambda k
                           (,(cps-trans arg0)
                            (lambda v0
                              (,(cps-trans arg1)
                               (lambda v1
                                 (k (,op v0 v1)))))))]
    ;; TODO: make operator application consistent with learn0.rkt
    [`(,op ,ex) #:when (operator? op)
                `(lambda k
                   (,(cps-trans ex) (lambda v0 (k (,op v0)))))]
    [`(,op ,ex)
     `(lambda k
        (,(cps-trans op) (lambda op_v
                           (,(cps-trans ex) (lambda vv
                                              ((op_v vv) k))))))]))
(require "cps-learn0.rkt")

(define (eval-after-trans ex)
  (eval-top `(,(cps-trans ex) (lambda v v))))

(provide cps-trans)
(provide eval-after-trans)

