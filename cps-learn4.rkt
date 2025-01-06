#lang racket

(define (operator? op)
  (member op '(+ - * / = show)))

(define (cps-trans-list exs)
  (match exs
    ['() `(lambda __k (__k (void)))]
    [`(,ex) `(lambda __k (,(cps-trans ex) __k))]
    [`(,head ,@res)
     `(lambda __k
        (,(cps-trans head)
         (lambda v
           (,(cps-trans-list res) __k))))]))

;; cps conversion procedure. 
;; A invariant that hold during the conversion:
;; every converted expression accepts a continuation as its parameter,
;; and this continuation parameter will fill the evaluated value into
;; the correct context (at the runtime). 
(define (cps-trans ex)
  (match ex
    [b #:when (boolean? b) `(lambda __k (__k ,b))]
    [n #:when (number? n) `(lambda __k (__k ,n))]
    [(? symbol?) `(lambda __k (__k ,ex))]
    [`(lambda ,x ,body)
     `(lambda __k (__k (lambda ,x ,(cps-trans body))))]
    [`(if ,cd ,ex0 ,ex1)
     `(lambda __k
        (,(cps-trans cd) (lambda v
                           (if v
                               (,(cps-trans ex0) __k)
                               (,(cps-trans ex1) __k)))))]
    [`(call/cc ,k0 ,ex)
     `(lambda __k
        (,(cps-trans `(lambda ,k0 ,ex))
         (lambda __f
           ((__f (lambda __v (lambda __k1
                               ;; __k1 contains the computation from the continuation parameter's call site
                               ;; to call/cc's call site, when the continuation parameter is invoked,
                               ;; __k1 should be ignored. 
                               (__k __v)))) __k))))]
    [`(reset ,ex)
     ;; continuation-composing style
     `(lambda __k (__k (,(cps-trans ex) (lambda __v __v))))]
    [`(shift ,k0 ,ex)
     `(lambda __k
        (,(cps-trans `(lambda ,k0 ,ex))
         (lambda __f
           (;; continuation-composing style:
            ;; when this functional argument is applied,
            ;; we compose k1 and k while incur a nested call (`(k1 (k v))`)
            (__f (lambda __v (lambda __k1 (__k1 (__k __v)))))
            ;; continuation at `shift` site is shifted, so we pass an `identity` function here
            (lambda __v __v)))))]

    [`(begin ,@exs)
     (cps-trans-list exs)]
    [`(,op ,arg0 ,arg1) #:when (operator? op)
                        `(lambda __k
                           (,(cps-trans arg0)
                            (lambda __v0
                              (,(cps-trans arg1)
                               (lambda __v1
                                 (__k (,op __v0 __v1)))))))]
    ;; TODO: make operator application consistent with learn0.rkt
    [`(,op ,ex) #:when (operator? op)
                `(lambda __k
                   (,(cps-trans ex) (lambda v0 (__k (,op v0)))))]
    [`(,op ,ex)
     `(lambda __k
        (,(cps-trans op) (lambda op_v
                           (,(cps-trans ex) (lambda vv
                                              ((op_v vv) __k))))))]))
(require "cps-learn0.rkt")

(define (eval-after-trans ex)
  (eval-top `(,(cps-trans ex) (lambda v v))))

(provide cps-trans)
(provide eval-after-trans)

