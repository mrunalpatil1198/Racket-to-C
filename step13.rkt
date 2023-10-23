;Transformed environment constructors into 'envr' define-union, updated 'apply-env' to use 'union-case,' and adjusted constructor invocations. Removed backquotes and commas from match patterns for clarity.
#lang racket
(require "parenthec.rkt")

(define value-of-cps
  (lambda (exp env k)
    (union-case exp expr
      ((const cexp) (apply-k k cexp))
      
      ((mult x1 x2) (value-of-cps x1 env (make-mult x2 env k)))
      
      ((sub1 x) (value-of-cps x env
                               (make-sub1 k)))
      
      ((zero x) (value-of-cps x env
                                (make-zero k)))
      
      ((if test conseq alt) (value-of-cps test env (make-if conseq alt env k)))

      ((catch body) (value-of-cps body (envr_extend-env env k) k))
     
      ((pitch k-exp v-exp)
       (value-of-cps k-exp env
                      (make-pitch v-exp env)))
      
      ((let e body)
       (value-of-cps e env
                     (make-let body env k)))
      
      ((var y) (apply-env env y k))
      
      ((lambda body)
       (apply-k k (clos_make-closure body env)))
      
      ((app rator rand)
       (value-of-cps rator env
                     (make-rand rand env k))))))
      
; ============= constructors =============
(define make-mult
  (lambda (x2^ env^ k^)
      `(make-mult ,x2^ ,env^ ,k^)))

(define make-mult-x1
  (lambda (x1^ k^)
    `(make-mult-x1 ,x1^ ,k^)))

(define make-sub1
  (lambda (k^)
    `(make-sub1 ,k^)))

(define make-zero
  (lambda (k^)
    `(make-zero ,k^)))

(define make-if
  (lambda (conseq^ alt^ env^ k^)
    `(make-if ,conseq^ ,alt^ ,env^ ,k^) ))

(define make-pitch
  (lambda (v-exp^ env^)
    `(make-pitch ,v-exp^ ,env^)))

(define make-let
  (lambda (body^ env^ k^)
    `(make-let ,body^ ,env^ ,k^)))

(define make-rand
  (lambda (rand^ env^ k^)
   `(make-rand ,rand^ ,env^ ,k^)))

(define make-apply-closure
  (lambda (v-rator^ k^)
    `(make-apply-closure ,v-rator^ ,k^)))

(define extend-env
  (lambda (env-cps^ a^)
    `(extend-env ,env-cps^ ,a^)))

; ============= unions =============

(define-union clos
  (make-closure body env))

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (catch body)
  (pitch kexp vexp)
  (let exp body)
  (lambda body)
  (app rator rand))

(define-union envr
  (empty-env)
  (extend-env env-cps a))

; ============= apply-helpers =============

(define apply-env
  (lambda (env-cps y k^)
    (union-case env-cps envr
      ((empty-env)(error "empty-env unbound variable"))
      ((extend-env env-cps^ a^)
       (if (zero? y)
          (apply-k k^ a^)
          (apply-env env-cps^ (sub1 y) k^))))))

(define apply-k
  (lambda (k^ v)
    (match k^
      (`(make-mult ,x2^ ,env^ ,k^) (value-of-cps x2^ env^
                      (make-mult-x1 v k^)))
      (`(make-mult-x1 ,x1^ ,k^) (apply-k k^ (* x1^ v)))
      (`(make-sub1 ,k^) (apply-k k^ (sub1 v)))
      (`(make-zero ,k^) (apply-k k^ (zero? v)))
      (`(make-if ,conseq^ ,alt^ ,env^ ,k^)  (if v
          (value-of-cps conseq^ env^ k^)
          (value-of-cps alt^ env^ k^)))
      (`(make-pitch ,v-exp^ ,env^) (value-of-cps v-exp^ env^ v))
      (`(make-let ,body^ ,env^ ,k^) (value-of-cps body^
                    (envr_extend-env env^ v) k^))
      (`(make-rand ,rand^ ,env^ ,k^)  (value-of-cps rand^ env^
                    (make-apply-closure v k^)))
      (`(make-apply-closure ,v-rator^ ,k^)
    (apply-closure v-rator^ v k^))
      (`(empty-k) v)
      )))

(define apply-closure
  (lambda (closure a k)
    (union-case closure clos
      ((make-closure body env)
       (value-of-cps body (envr_extend-env env a) k)))))

; ============= empty =============
 
(define empty-k
  (lambda ()
    `(empty-k)))

; ============= main =============

(define main
  (lambda ()
    (value-of-cps
     (expr_let
      (expr_lambda
       (expr_lambda
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult
          (expr_var 0)
          (expr_app
           (expr_app (expr_var 1) (expr_var 1))
           (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_catch
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_pitch
          (expr_var 0)
          (expr_app
           (expr_app (expr_var 1) (expr_var 1))
           (expr_const 4)))))
       (expr_const 5)))
     (envr_empty-env)
     (empty-k))))

(main)

#|
(require test-engine/racket-tests)

(check-expect (value-of-cps '(const 5) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)
(check-expect (value-of-cps '(zero (const 5)) (empty-env) (empty-k)) #f)
(check-expect (value-of-cps '(sub1 (const 5)) (empty-env) (empty-k)) 4)
(check-expect (value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)
(check-expect (value-of-cps '(zero (sub1 (const 6))) (empty-env) (empty-k)) #f)
(check-expect (value-of-cps '(if (zero (const 5)) (const 3) (mult (const 2) (const 2))) (empty-env) (empty-k)) 4)
(check-expect (value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) 4)
(check-expect (value-of-cps '(app (lambda (const 5)) (const 6)) (empty-env) (empty-k)) 5) 
(check-expect (value-of-cps '(app (lambda (var 0)) (const 5)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)
(check-expect (value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(app (lambda (if (zero (var 0)) (const 4) (const 5))) (const 3)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) 4)
(check-expect (value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)
(check-expect (value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)
(check-expect (value-of-cps '(catch (const 5)) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(catch (pitch (var 0) (const 5))) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(catch (pitch (var 0) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
(check-expect (value-of-cps '(catch (pitch (app (lambda (var 0)) (var 0)) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
(check-expect (value-of-cps '(catch (sub1 (pitch (var 0) (const 5)))) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(catch (pitch (pitch (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(catch (pitch (const 5) (pitch (var 0) (const 5)))) (empty-env) (empty-k)) 5)
(check-expect (value-of-cps '(mult (const 3) (catch (pitch (const 5) (pitch (var 0) (const 5))))) (empty-env) (empty-k)) 15)
(check-expect (value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
                            (empty-env)
                            (empty-k))
              4)
(check-expect (value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                            (empty-env)
                            (empty-k))
              4)
(check-expect (value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                                  (lambda
                                      (lambda 
                                          (if (zero (var 0))  
                                              (const 1)
                                              (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                            (empty-env)
                            (empty-k))
              1)

(test)
|#


