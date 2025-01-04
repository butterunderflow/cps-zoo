#lang racket

(define (get x env)
  (let ([value (assoc x env)])
    (if value
        (cdr value)
        (error (string-append "variable " (symbol->string x) " not found")))))

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

(define (eval2 ex env cont)
  ;; cps interpreter with call/cc operator
  ;; call/cc stands for call with current continuation,
  ;; when call a function with current continuation, interpreter will pass the continuation
  ;; as argument to the function, and when call this continuation inside the function, 
  ;; the control will enter this continuation and never come back again. 
  (match ex
    [b #:when (boolean? b) (cont b)]
    [n #:when (number? n) (cont n)]
    [x #:when (symbol? x) (cont (get x env))]
    [`(lambda ,sym ,body)
     (cont (lambda (x k)
             (eval2 body (cons (cons sym x) env) k)))]
    [`(,op ,arg0 ,arg1) #:when (operator? op)
                        (let ([new-cont0
                               (lambda (v0)
                                 (let ([new-cont1
                                        (lambda (v1)
                                          (cont (eval-op op v0 v1)))])
                                   (eval2 arg1 env new-cont1)))])
                          (eval2 arg0 env new-cont0))]
    [`(if ,ex0 ,ex1 ,ex2)
     (let ([new-cont (lambda (v)
                       (if v
                           (eval2 ex1 env cont)
                           (eval2 ex2 env cont)))])
     (eval2 ex0 env new-cont))]
    [`(begin ,@exs1)
     (eval-list eval2 exs1 env cont)]
    [`(call/cc ,ex)
     (let ([new-cont (lambda (f)
                       (let ([contv (lambda (v _k)
                                      ;; _k is ignored, when call this continuation,
                                      ;; the control will never come back to caller of call/cc
                                      (cont v))])
                         (f contv cont)))])
       (eval2 ex env new-cont))]
    [`(reset ,ex)
     (cont (eval2 ex env init-cont))]
    [`(shift ,ex)
     (let ([new-cont (lambda (f)
                       (let ([contv (lambda (v k1)
                                      ;; with continuation composing style, after calling the shifted
                                      ;; continuation parameter, we can back to the call site
                                      (k1 (cont v)))])
                         ;; continuation `cont` is shifted here
                         ;;           |
                         ;;           v
                         (f contv init-cont)))])
       (eval2 ex env new-cont))]
    [`(,op ,arg)
     (let ([new-cont0
            (lambda (v0)
              (let ([new-cont1 (lambda (v1) (v0 v1 cont))])
                (eval2 arg env new-cont1)))])
       (eval2 op env new-cont0))]))

(define default-env
  (list `(show . ,(lambda (x cont) (cont (println x))))))

(define init-cont
  (lambda (x) x))

(define (eval-top ex)
  (eval2 ex default-env init-cont))

(provide eval-top)
