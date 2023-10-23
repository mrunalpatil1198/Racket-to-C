;Renamed formal parameters with asterisks and converted serious function calls to A-normal form style by introducing 'let*' above the calls. Maintained the exact names of formal parameters for actual parameters in the calls.
#lang racket
(require "parenthec.rkt")

(define value-of-cps
  (lambda (*exp* *env* *k*)
    (union-case *exp* expr
      ((const cexp) (let* ((*a-k* *k*)
                           (*v* cexp))
                      (apply-k *a-k* *v*)))
      
      ((mult x1 x2) (let* ((*exp* x1)
                           (*env* *env*)
                           (*k* (kt_make-mult x2 *env* *k*)))
                      (value-of-cps *exp* *env* *k*)))
      
      ((sub1 x) (let* ((*exp* x)
                       (*env* *env*)
                       (*k* (kt_make-sub1 *k*)))
                  (value-of-cps *exp* *env* *k*)))
                       
      ((zero x) (let* ((*exp* x)
                       (*env* *env*)
                       (*k* (kt_make-zero *k*)))
                  (value-of-cps *exp* *env* *k*)))
      
      ((if test conseq alt) (let* ((*exp* test)
                                   (*env* *env*)
                                   (*k* (kt_make-if conseq alt *env* *k*)))
                              (value-of-cps *exp* *env* *k*)))

      ((catch body) (let* ((*exp* body)
                           (*env* (envr_extend-env *env* *k*))
                           (*k* *k*))
                      (value-of-cps *exp* *env* *k*)))
     
      ((pitch k-exp v-exp) (let* ((*exp* k-exp)
                                  (*env* *env*)
                                  (*k*  (kt_make-pitch v-exp *env*)))
                              (value-of-cps *exp* *env* *k*)))
      
      ((let e body) (let* ((*exp* e)
                           (*env* *env*)
                           (*k*  (kt_make-let body *env* *k*)))
                      (value-of-cps *exp* *env* *k*)))
      
      ((var y) (let* ((*a-env-cps* *env*)
                      (*a-env-y* y)
                      (*a-env-k* *k*))
                 (apply-env *a-env-cps* *a-env-y* *a-env-k*)))
      
      ((lambda body) (let* ((*a-k* *k*)
                            (*v* (clos_make-closure body *env*)))
                       (apply-k *a-k* *v*)))
      
      ((app rator rand) (let* ((*exp* rator)
                                (*env* *env*)
                                 (*k* (kt_make-rand rand *env* *k*)))
                          (value-of-cps *exp* *env* *k*)))
      )))
      
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

(define apply-env
  (lambda (*a-env-cps* *a-env-y* *a-env-k*)
    (union-case *a-env-cps* envr
      ((empty-env)(error "empty-env unbound variable"))
      ((extend-env env-cps a)
       (if (zero? *a-env-y*)
          (let* ((*a-k* *a-env-k*)
                 (*v* a))
            (apply-k *a-k* *v*))
          (let* ((*a-env-cps* env-cps)
                            (*a-env-y* (sub1 *a-env-y*))
                            (*a-env-k* *a-env-k*))
                       (apply-env *a-env-cps* *a-env-y* *a-env-k*)))))))

(define apply-k
  (lambda (*a-k* *v*)
    (union-case *a-k* kt
      ((make-mult x2^ env^ k^) (let* ((*exp* x2^)
                                      (*env* env^)
                                      (*k* (kt_make-mult-x1 *v* k^)))
                                 (value-of-cps *exp* *env* *k*)))
      
      ((make-mult-x1 x1^ k^) (let* ((*a-k* k^)
                                    (*v* (* x1^ *v*)))
                               (apply-k *a-k* *v*)))
       
      ((make-sub1 k^) (let* ((*a-k* k^)
                             (*v* (sub1 *v*)))
                        (apply-k *a-k* *v*)))
                             
       ((make-zero k^) (let* ((*a-k* k^)
                                       (*v* (zero? *v*)))
                                  (apply-k *a-k* *v*)))
       
      ((make-if conseq^ alt^ env^ k^) (let* ((*env* env^)
                                             (*k* k^)
                                             (*exp* (if *v* conseq^ alt^)))
                                        (value-of-cps *exp* *env* *k*)))
      
      ((make-pitch v-exp^ env^) (let* ((*exp* v-exp^)
                                       (*env* env^)
                                       (*k* *v*))
                                  (value-of-cps *exp* *env* *k*)))
      
      ((make-let body^ env^ k^) (let* ((*exp* body^)
                                       (*env* (envr_extend-env env^ *v*))
                                       (*k* k^))
                                  (value-of-cps *exp* *env* *k*)))
      
      ((make-rand rand^ env^ k^)  (let* ((*exp* rand^)
                                         (*env* env^)
                                         (*k* (kt_make-apply-closure *v* k^)))
                                    (value-of-cps *exp* *env* *k*)))
      
      ((make-apply-closure v-rator^ k^) (let* ((*closure* v-rator^)
                                               (*arg* *v*)
                                               (*clos-k* k^))
                                          (apply-closure *closure* *arg* *clos-k*)))
      
      ((empty-k) *v*)
      )))

(define apply-closure
  (lambda (*closure* *arg* *clos-k*)
    (union-case *closure* clos
      ((make-closure body env)
       (let* ((*exp* body)
              (*env* (envr_extend-env env *arg*))
              (*k* *clos-k*))
         (value-of-cps *exp* *env* *k*))))))

; ============= empty =============

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
     (kt_empty-k))))

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


