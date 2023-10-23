; vanilla interpreter
#lang racket
(require rackunit)
(define value-of
  (lambda (expr env)
    (match expr
      (`(const ,expr) expr)
      (`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env)))
      (`(sub1 ,x) (sub1 (value-of x env)))
      (`(zero ,x) (zero? (value-of x env)))
      (`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env)))
      (`(catch ,body) (let/cc k
                        (value-of body (lambda (y) (if (zero? y) k (env (sub1 y)))))))
      (`(pitch ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env)))
      (`(let ,e ,body) (let ((a (value-of e env)))
                         (value-of body (lambda (y) (if (zero? y) a (env (sub1 y)))))))
      (`(var ,y) (env y))
      (`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y)))))))
      (`(app ,rator ,rand) ((value-of rator env) (value-of rand env))))))
 
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

