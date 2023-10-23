;defining make-closure function to create closures instead of higher order functions. 
;replacing higher-order closure representation with tagged lists
 
#lang racket
(define value-of-cps
  (lambda (expr env k)
    (match expr
      (`(const ,expr) (apply-k k expr))
      (`(mult ,x1 ,x2) (value-of-cps x1 env
                                  (lambda (v)
                                    (value-of-cps x2 env
                                               (lambda (w)
                                               (apply-k k (* v w)))))))
      (`(sub1 ,x) (value-of-cps x env
                                (lambda (v)
                                       (apply-k k (sub1 v)))))
      (`(zero ,x) (value-of-cps x env
                                (lambda (v)
                                  (apply-k k (zero? v)))))
      
     (`(if ,test ,conseq ,alt) (value-of-cps test env (lambda (x)
                                                         (if x
                                                             (value-of-cps conseq env k)
                                                             (value-of-cps alt env k)))))

      (`(catch ,body) (value-of-cps body (extend-env env k) k))
     
      (`(pitch ,k-exp ,v-exp)
       (value-of-cps k-exp env
                      (lambda (v)
                        (value-of-cps v-exp env v))))
      
      (`(let ,e ,body)
       (value-of-cps e env
                     (lambda (v)
                       (value-of-cps body
                                     (extend-env env v) k))))
      
      (`(var ,y) (apply-env env y k))
      
      (`(lambda ,body)
       (apply-k k (make-closure body env)))
      
      (`(app ,rator ,rand)
       (value-of-cps rator env
                     (lambda (v)
                       (value-of-cps rand env
                                     (lambda (w)
                                       (apply-closure v w k)))))))))

(define extend-env
  (lambda (env-cps^ a^)
    `(extend-env ,env-cps^ ,a^)))

(define apply-closure
  (lambda (rator a k)
    (match rator
      (`(make-closure ,body ,env)
       (value-of-cps body (extend-env env a) k)))))

(define make-closure
  (lambda (body env-cps)
    `(make-closure ,body ,env-cps)))

(define apply-k
  (lambda (k y)
    (k y)))

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
    (lambda (v)
      v)))

(value-of-cps '(const 5) (empty-env) (empty-k)) ; 5)
(value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) ; 25)
(value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) ; 3)
(value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) ; 4)
(value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) ; 6)
(value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) ; 5)
(value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) ; 4)
(value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) ; 5)
(value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) ; 25)
(value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) ; 5)
(value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) ; 3)
(value-of-cps '(catch (pitch (pitch (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) ; 5)

(value-of-cps '(catch (pitch (const 5) (pitch (var 0) (const 5)))) (empty-env) (empty-k)) ; 5)

(value-of-cps '(mult (const 3) (catch (pitch (const 5) (pitch (var 0) (const 5))))) (empty-env) (empty-k)) ; 15)
(value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
              (empty-env)
              (empty-k)) ;4)
(value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
              (empty-env)
              (empty-k)) ;4)
(value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                    (lambda
                        (lambda 
                            (if (zero (var 0))  
                                (const 1)
                                (app (app (var 1) (var 1)) (sub1 (var 0)))))))
              (empty-env)
              (empty-k)) ; 1)