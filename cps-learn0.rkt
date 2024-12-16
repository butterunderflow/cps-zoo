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

(define (eval-list eval-f lst env)
  (match lst
    ['() (void)]
    [`(,ex) (eval-f ex env)]
    [`(,head ,@res)
     (eval-f head env)
     (eval-list eval-f res env)]))

(define (eval0 ex env)
  ;; basic interpreter
  (match ex
    [b #:when (boolean? b) b]
    [n #:when (number? n) n]
    [x #:when (symbol? x) (get x env)]
    [`(lambda ,sym ,body)
     (lambda (x) (eval0 body (cons (cons sym x) env)))]
    [`(,op ,arg0 ,arg1) #:when (operator? op)
                        (let ((arg0 (eval0 arg0 env))
                              (arg1 (eval0 arg1 env)))
                          (eval-op op arg0 arg1))]
    [`(if ,cond ,ex0 ,ex1)
     (if (eval0 cond env)
         (eval0 ex0 env)
         (eval0 ex1 env))]
    [`(begin ,@exs1)
     (eval-list eval0 exs1 env)]
    [`(,op ,arg)
     (let ((op (eval0 op env))
           (arg (eval0 arg env)))
       (op arg))]))

(define default-env
  (list `(show . ,println)))

(define (eval-top ex)
  (eval0 ex default-env))

(provide eval-top)
