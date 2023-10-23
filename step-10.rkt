;removing else clause from apply-k

#lang racket
(define value-of-cps
  (lambda (expr env k)
    (match expr
      (`(const ,expr) (apply-k k expr))
      
      (`(mult ,x1 ,x2) (value-of-cps x1 env (make-mult x2 env k)))
      
      (`(sub1 ,x) (value-of-cps x env
                               (make-sub1 k)))
      
      (`(zero ,x) (value-of-cps x env
                                (make-zero k)))
      
      (`(if ,test ,conseq ,alt) (value-of-cps test env (make-if conseq alt env k)))

      (`(catch ,body) (value-of-cps body (extend-env env k) k))
     
      (`(pitch ,k-exp ,v-exp)
       (value-of-cps k-exp env
                      (make-pitch v-exp env)))
      
      (`(let ,e ,body)
       (value-of-cps e env
                     (make-let body env k)))
      
      (`(var ,y) (apply-env env y k))
      
      (`(lambda ,body)
       (apply-k k (make-closure body env)))
      
      (`(app ,rator ,rand)
       (value-of-cps rator env
                     (make-rand rand env k))))))


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
                    (extend-env env^ v) k^))
      (`(make-rand ,rand^ ,env^ ,k^)  (value-of-cps rand^ env^
                    (make-apply-closure v k^)))
      (`(make-apply-closure ,v-rator^ ,k^)
    (apply-closure v-rator^ v k^))
      (`(empty-k) v)
      )))
      

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

(define apply-closure
  (lambda (clos a k)
    (match clos
      (`(make-closure ,body ,env)
       (value-of-cps body (extend-env env a) k)))))

(define make-closure
  (lambda (body env-cps)
    `(make-closure ,body ,env-cps)))

(define apply-env
  (lambda (env-cps y k^)
    (match env-cps
      (`(empty-env)(error "empty-env unbound variable"))
      (`(extend-env ,env-cps^ ,a^)
       (if (zero? y)
          (apply-k k^ a^)
          (apply-env env-cps^ (sub1 y) k^))))))

(define empty-env
  (lambda ()
    `(empty-env)))
 
(define empty-k
  (lambda ()
    `(empty-k)))

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
