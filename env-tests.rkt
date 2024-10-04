#lang racket
;Minh Nguyen

(require rackunit)
(provide env-tests)
(require "env.rkt")

; Define an environment for testing.
(define test-env
  (env '(x y)
       '(1 2)
       empty-env))

(define env-a
  (env '(x y) '(1 2) empty-env))

(define env-b
  (env '(x z) '(5 7) env-a))

(define env-tests
  (test-suite
   "Environment tests"
   (test-exn "Empty environment has no previous"
             exn:fail?
             (λ () (env-previous empty-env)))
   (test-exn "Looking up a symbol in an empty environment throws an error"
             exn:fail?
             (λ () (env-lookup empty-env 'x)))
   (test-equal? "Looking up a symbol that’s bound in an environment returns its value"
                (env-lookup test-env 'x)
                1)
   (test-equal? "Looking up a symbol in the environment’s previous environment returns the correct value."
                (env-lookup env-b 'y)
                2)
   (test-equal? "Looking up symbol x in an environment"
                (env-lookup env-b 'x)
                5)
   (test-equal? "Looking up symbol x in previous environment."
                (env-lookup env-a 'x)
                1)))

;(run-tests env-tests)
