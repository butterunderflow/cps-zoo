#lang racket

(define (get x env)
  (cdr (assoc x env)))

(define (operator? op)
  (member op '(+ - * / =)))

(define (eval-op op x y)
  (match op
    ['+ (+ x y)]
    ['- (- x y)]
    ['* (* x y)]
    ['/ (/ x y)]
    ['= (= x y)]))

(define (eval-list eval-f lst env cont)
  (match lst
    ['() (cont void)]
    [`(,ex) (eval-f ex env cont)]
    [`(,head ,@res)
     (let ([new-cont
            (lambda (v)
              (eval-list eval-f res env cont))])
       (eval-f head env new-cont))]))

;; TODO: can we partial evaluate this function to make a cps transformation?
(define (eval1 ex env cont)
  ;; cps interpreter
  (match ex
    [b #:when (boolean? b) (cont b)]
    [n #:when (number? n) (cont n)]
    [x #:when (symbol? x) (cont (get x env))]
    [`(lambda ,sym ,body)
     (cont (lambda (x k)
             (eval1 body (cons (cons sym x) env) k)))]
    [`(,op ,arg0 ,arg1) #:when (operator? op)
                        (let ([new-cont0
                               (lambda (v0)
                                 (let ([new-cont1
                                        (lambda (v1)
                                          (cont (eval-op op v0 v1)))])
                                   (eval1 arg1 env new-cont1)))])
                          (eval1 arg0 env new-cont0))]
    [`(if ,ex0 ,ex1 ,ex2)
     (let ([new-cont (lambda (v)
                       (if v
                           (eval1 ex1 env cont)
                           (eval1 ex2 env cont)))])
     (eval1 ex0 env new-cont))]
    [`(begin ,@exs1)
     (eval-list eval1 exs1 env cont)]
    [`(,op ,arg)
     (let ([new-cont0
            (lambda (v0)
              (let ([new-cont1
                     (lambda (v1)
                       (v0 v1 cont))])
                (eval1 arg env new-cont1)))])
       (eval1 op env new-cont0))]))

(define default-env
  (list `(show . ,(lambda (x cont) (cont (println x))))))

(define init-cont
  (lambda (x) x))

(define (eval-top ex)
  (eval1 ex default-env init-cont))

(provide eval-top)
