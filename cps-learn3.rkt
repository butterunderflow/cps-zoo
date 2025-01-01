#lang racket

(define (get x env)
  (let ((value (assoc x env)))
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

(define (eval-list eval-f lst env cont mcont)
  (match lst
    ['() (cont void)]
    [`(,ex) (eval-f ex env cont mcont)]
    [`(,head ,@res)
     (let ((new-cont
            (lambda (v mk)
              (eval-list eval-f res env cont mk))))
       (eval-f head env new-cont mcont))]))

(define (eval3 ex env cont mcont)
  ;; cps interpreter with call/cc operator
  ;; call/cc stands for call with current continuation,
  ;; when call a function with current continuation, interpreter will pass the continuation
  ;; as argument to the function, and when call this continuation inside the function, 
  ;; the control will enter this continuation and never come back again. 
  (match ex
    [b #:when (boolean? b) (cont b mcont)]
    [n #:when (number? n) (cont n mcont)]
    [x #:when (symbol? x) (cont (get x env) mcont)]
    [`(lambda ,sym ,body)
     (cont (lambda (x k mk)
             (eval3 body (cons (cons sym x) env) k mk)) mcont)]
    [`(,op ,arg0 ,arg1) #:when (operator? op)
                        (let ((new-cont0
                               (lambda (v0 mk0)
                                 (let ((new-cont1
                                        (lambda (v1 mk1)
                                          (cont (eval-op op v0 v1) mk1))))
                                   (eval3 arg1 env new-cont1 mk0)))))
                          (eval3 arg0 env new-cont0 mcont))]
    [`(if ,ex0 ,ex1 ,ex2)
     (let ((new-cont (lambda (v mk)
                       (if v
                           (eval3 ex1 env cont mk)
                           (eval3 ex2 env cont mk)))))
       (eval3 ex0 env new-cont mcont))]
    [`(begin ,@exs1)
     (eval-list eval3 exs1 env cont mcont)]
    ;; we reset parameter `cont` twice, is this right?
    [`(reset ,ex)
     (eval3 ex env init-cont (lambda (v) (cont v mcont)))]
    [`(shift ,ex)
     (let ((new-cont (lambda (f mk)
                       (let ((contv (lambda (v k mk1)
                                      ;; The shifted continuation.
                                      ;; Parameter `k` contains the computation targeting last **shift** rather than
                                      ;; **reset**, because the continuaition betweeen last **shift** and last **reset**
                                      ;; was shifted into shift's parameter. Hence if we shift again inside the shift's
                                      ;; body(the `f`'s body), the shifted computation will only contains computation
                                      ;; till last shift. 
                                      (let ([new-mk1 (lambda (v) (k v mk1))])
                                        (cont v new-mk1)))))
                         ;;               ^
                         ;;               |
                         ;;           -----
                         ;;           |
                         ;;    shifted continuation
                         ;;           |
                         (f contv init-cont mcont)))))
       (eval3 ex env new-cont mcont))]
    [`(shift, ex)
     ;; shift implementation use direct style
     (let ((new-cont (lambda (f mk)
                       (let ((contv (lambda (v k mk1)
                                      (let ((res (cont v init-mcont)))
                                        (k res mk1)))))
                         (f contv init-cont mk)))))
       (eval3 ex env new-cont mcont))]
    [`(prompt ,ex)
     ;; exactly same as reset
     (eval3 ex env init-cont (lambda (v) (cont v mcont)))]
    [`(control ,ex)
     ;; a wrong implementation of control with same semantic as shift
     (let ((new-cont (lambda (f mk)
                       (let ((contv (lambda (v k mk1)
                                      ;; try to append k after cont to make a `new-cont1`, but it's wrong!
                                      (let ([new-cont1
                                             (lambda (v mk2)
                                               (cont v (lambda (v) (k v mk2))))])
                                        (new-cont1 v mk1)))))
                         (f contv init-cont mk)))))
       (eval3 ex env new-cont mcont))]
    [`(,op ,arg)
     (let ((new-cont0
            (lambda (v0 mk0)
              (let ((new-cont1
                     (lambda (v1 mk1)
                       (v0 v1 cont mk1))))
                (eval3 arg env new-cont1 mk0)))))
       (eval3 op env new-cont0 mcont))]))

(define default-env
  (list `(show . ,(lambda (x cont mcont) (cont (println x) mcont)))))

(define init-cont
  (lambda (v mk) (mk v))) 

(define init-mcont
  (lambda (v) v))

(define (eval-top ex)
  (eval3 ex default-env init-cont init-mcont))

(provide eval-top)
