;Revised (define name (lambda () ...)) statements to utilize define-label. Established the program counter at the beginning of the program with define-program-counter.
#lang racket
(require "parenthec.rkt")

(define-registers *exp* *env* *k* *a-env-y* *a-k* *v* *closure*)
(define-program-counter *pc*)

(define-label value-of-cps ;*exp* *env* *k*
  (union-case *exp* expr
              ((const cexp) (begin (set! *a-k* *k*)
                                   (set! *v* cexp)
                                   (apply-k)))
      
              ((mult x1 x2) (begin (set! *exp* x1)
                                   (set! *k* (kt_make-mult x2 *env* *k*))
                                   (value-of-cps)))
      
              ((sub1 x) (begin (set! *exp* x)
                               (set! *k* (kt_make-sub1 *k*))
                               (value-of-cps)))
                       
              ((zero x) (begin (set! *exp* x)
                               (set! *k* (kt_make-zero *k*))
                               (value-of-cps)))
      
              ((if test conseq alt) (begin (set! *exp* test)
                                           (set! *k* (kt_make-if conseq alt *env* *k*))
                                           (value-of-cps)))

              ((catch body) (begin (set! *exp* body)
                                   (set! *env* (envr_extend-env *env* *k*)) 
                                   (value-of-cps)))
     
              ((pitch k-exp v-exp) (begin (set! *exp* k-exp)
                                          (set! *k*  (kt_make-pitch v-exp *env*))
                                          (value-of-cps)))
      
              ((let e body) (begin (set! *exp* e)
                                   (set! *k*  (kt_make-let body *env* *k*))
                                   (value-of-cps)))
      
              ((var y) (begin (set! *a-env-y* y)
                              (apply-env)))
      
              ((lambda body) (begin (set! *a-k* *k*)
                                    (set! *v* (clos_make-closure body *env*))
                                    (apply-k)))
      
              ((app rator rand) (begin (set! *exp* rator)
                                       (set! *k* (kt_make-rand rand *env* *k*))
                                       (value-of-cps)))
              ))
      
; ============= constructors =============

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

(define-union kt
  (make-mult x2^ env^ k^)
  (make-mult-x1 x1^ k^)
  (make-zero k^)
  (make-sub1 k^) 
  (make-rand rand^ env^ k^)         
  (make-apply-closure v-rator^ k^) 
  (make-if conseq^ alt^ env^ k^)                
  (make-let body^ env^ k^) 
  (make-pitch v-exp^ env^) 
  (empty-k))

; ============= apply-helpers =============

(define-label apply-env ;*a-env-y* 
  (union-case *env* envr
              ((empty-env)(error "empty-env unbound variable"))
              ((extend-env env-cps a)
               (if (zero? *a-env-y*)
                   (begin (set! *a-k* *k*)
                          (set! *v* a)
                          (apply-k))
                   (begin (set! *env* env-cps)
                          (set! *a-env-y* (sub1 *a-env-y*))
                          (apply-env))))))

(define-label apply-k ;*a-k* *v*
  (union-case *a-k* kt
              ((make-mult x2^ env^ k^) (begin (set! *exp* x2^)
                                              (set! *env* env^)
                                              (set! *k* (kt_make-mult-x1 *v* k^))
                                              (value-of-cps)))
      
              ((make-mult-x1 x1^ k^) (begin (set! *a-k* k^)
                                            (set! *v* (* x1^ *v*))
                                            (apply-k)))
       
              ((make-sub1 k^) (begin (set! *a-k* k^)
                                     (set! *v* (sub1 *v*))
                                     (apply-k)))
                             
              ((make-zero k^) (begin (set! *a-k* k^)
                                     (set! *v* (zero? *v*))
                                     (apply-k)))
       
              ((make-if conseq^ alt^ env^ k^) (begin (set! *env* env^)
                                                     (set! *k* k^)
                                                     (set! *exp* (if *v* conseq^ alt^))
                                                     (value-of-cps)))
      
              ((make-pitch v-exp^ env^) (begin (set! *exp* v-exp^)
                                               (set! *env* env^)
                                               (set! *k* *v*)
                                               (value-of-cps)))
      
              ((make-let body^ env^ k^) (begin (set! *exp* body^)
                                               (set! *env* (envr_extend-env env^ *v*))
                                               (set! *k* k^)
                                               (value-of-cps)))
      
              ((make-rand rand^ env^ k^)  (begin (set! *exp* rand^)
                                                 (set! *env* env^)
                                                 (set! *k* (kt_make-apply-closure *v* k^))
                                                 (value-of-cps)))
      
              ((make-apply-closure v-rator^ k^) (begin (set! *closure* v-rator^)
                                                       (set! *k* k^)
                                                       (apply-closure)))
      
              ((empty-k) *v*)
              ))

(define-label apply-closure ;*closure*
  (union-case *closure* clos
              ((make-closure body env) (begin (set! *exp* body)
                                              (set! *env* (envr_extend-env env *v*))
                                              (value-of-cps)))))

; ============= empty =============

; ============= main =============

(define-label main
  (begin
    (set! *k* (kt_empty-k))
    (set! *env* (envr_empty-env))
    (set! *exp* (expr_let
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
                  (expr_const 5))))
    (value-of-cps)))

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



