#lang racket
;Minh Nguyen

(provide env
         env?
         empty-env
         empty-env?
         env-syms
         env-vals
         env-previous
         env-lookup
         )

; The empty environment is null.
(define empty-env null)
(define empty-env? null?)

(struct env (syms vals previous) #:transparent)



(define env-a
  (env '(x y) '(1 2) empty-env))

(define env-b
  (env '(x z) '(5 7) env-a))

;(env-syms env-b)
;(env-vals env-b)
;(env-previous env-b)

(define (env-lookup environment symbol)
  (cond [(empty-env? environment) (error 'env-lookup "No binding for ~s" symbol)]
        [(empty? (env-syms environment)) (env-lookup (env-previous environment) symbol)]
        [(equal? symbol (first (env-syms environment))) (first (env-vals environment))]
        [else (env-lookup (env (rest (env-syms environment)) (rest (env-vals environment)) (env-previous environment)) symbol)]))




