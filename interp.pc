;#lang racket
;(require "parenthec.rkt")

(define-registers k v exp env y cl a^)
(define-program-counter pc)

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (capture body)
  (return kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))

(define-label value-of-cps
    (union-case exp expr 
      [(const cexp) (begin (set! v cexp)
                     (set! pc apply-k))]
      [(var expr) (begin (set! y expr)
                   (set! pc apply-env))]
      [(mult x1 x2) (begin (set! k (kt_mult-cps-outer-k x2 env k ))
                           (set! exp x1)
                           (set! pc value-of-cps))]
      [(sub1 x)   (begin (set! k (kt_sub1-cps-k k))
                         (set! exp x)
                         (set! pc value-of-cps))]
      [(zero x)  (begin (set! k (kt_zero-cps-k k))
                        (set! exp x)
                        (set! pc value-of-cps))]
      [(if test conseq alt) (begin (set! k (kt_if-cps-k conseq alt env k))
                                   (set! exp test)
                                   (set! pc value-of-cps))]                       
      [(capture  body) (begin (set! env (envr_extend-env env k))
                              (set! exp body)
                              (set! pc value-of-cps))]     
      [(return k-exp v-exp) (begin (set! k (kt_return-cps-k v-exp env k))
                                   (set! exp k-exp)
                                   (set! pc value-of-cps))]
      [(let e body) (begin (set! k (kt_let-cps-k body env k))
                           (set! exp e)
                           (set! pc value-of-cps))]
      [(lambda body)  (begin (set! v (closure_closure body env))
                          (set! pc apply-k))]
      [(app rator rand)  (begin (set! k (kt_app-cps-outer-k rand env k))
                                (set! exp rator)
                                (set! pc value-of-cps))]))
  

(define-label apply-env
   (union-case env envr
      [(empty-env) (error 'value-of-cps "unbound identifier")]
      [(extend-env env^ m^) (if (zero? y)  (begin (set! v m^) (set! pc apply-k)) (begin (set! env env^) (set! y (sub1 y)) (set! pc apply-env)))]
   ))

(define-label apply-k
    (union-case k kt
      ((empty-k jumpout) (dismount-trampoline jumpout))
      ((mult-cps-outer-k x^ env^ k^) (begin (set! k (kt_mult-cps-inner-k v k^)) (set! env env^) (set! exp x^)  
                                      (set! pc value-of-cps)))
      ((mult-cps-inner-k v^ k^) (begin (set! k k^)
                                       (set! v (* v^ v))
                                       (set! pc apply-k)))
      ((sub1-cps-k k^) (begin (set! k k^)
                              (set! v (sub1 v))
                              (set! pc apply-k)))
      ((zero-cps-k k^) (begin (set! k k^)
                              (set! v (zero? v))
                              (set! pc apply-k)))
      ((if-cps-k conseq^ alt^ env^ k^) (if v (begin (set! k k^) (set! env env^) (set! exp conseq^)  (set! pc value-of-cps))
                                           (begin (set! k k^) (set! env env^) (set! exp alt^)   (set! pc value-of-cps))))
      ((return-cps-k v-exp^ env^ k^) (begin (set! k v)
                                            (set! env env^)
                                            (set! exp v-exp^)
                                            (set! pc value-of-cps)))
      ((let-cps-k body^ env^ k^) (begin (set! k k^)
                                         (set! env (envr_extend-env env^ v ))
                                         (set! exp body^)
                                         (set! pc value-of-cps)))
      ((app-cps-outer-k rand^ env^ k^) (begin (set! k (kt_app-cps-inner-k v k^))
                                               (set! env env^)
                                               (set! exp rand^)
                                               (set! pc value-of-cps)))
      ((app-cps-inner-k v^ k^) (begin (set! k k^)
                                      (set! a^ v)
                                      (set! cl v^)  
                                      (set! pc apply-closure)))
      ))

(define-union envr
    (extend-env env^ a^)
    (empty-env)
  )

(define-union closure
   (closure body env))

(define-label apply-closure
    (union-case cl closure
      [(closure body env^) (begin (set! env (envr_extend-env env^ a^))
                                 (set! exp body)
                           (set! pc value-of-cps))]))

(define-union kt
  (mult-cps-outer-k x^ env^ k^)
  (mult-cps-inner-k v^ k^)
  (sub1-cps-k k^)
  (zero-cps-k k^)
  (if-cps-k conseq^ alt^ env^ k^)
  (return-cps-k v-exp^ env^ k^)
  (let-cps-k body^ env^ k^)
  (app-cps-outer-k rand^ env^ k^)
  (app-cps-inner-k v^ k^)
  (empty-k jumpout))
  
  


(define-label main 
    (begin
      (set! exp (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_capture
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_return (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5))))
      (set! env (envr_empty-env))
      (set! pc value-of-cps)
      (mount-trampoline kt_empty-k k pc)
      (printf "Fact 5: ~s\n" v)))