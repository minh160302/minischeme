#lang racket
;Minh Nguyen

(require rackunit)
(require "env.rkt" "parse.rkt" "interp.rkt")

(provide interp-tests)

(define test-env
  (env '(x y) (map box '(10 23)) init-env))

(define interp-tests
  (test-suite
   "Interpreter tests"
   ;;; lit-exp

   ;;; var-exp

   ;;; app-exp
   ;; add
   (test-eqv? "+ operator for nothing"
              (eval-exp (app-exp (var-exp '+)
                                 '()) test-env)
              0)
   (test-eqv? "+ operator for constants"
              (eval-exp (app-exp (var-exp '+)
                                 (list (lit-exp 1) (lit-exp 2))) test-env)
              3)
   (test-eqv? "+ operator for multiple numbers"
              (eval-exp (app-exp (var-exp '+)
                                 (list (lit-exp 1) (lit-exp 2) (lit-exp 3) (lit-exp 4))) test-env)
              10)
   (test-eqv? "+ operator for numbers and env variables"
              (eval-exp (app-exp (var-exp '+)
                                 (list (lit-exp 4) (lit-exp 2) (var-exp 'x))) test-env)
              16)
   ;; subtract
   (test-eqv? "- operator in test-env"
              (eval-exp (app-exp (var-exp '-)
                                 (list (var-exp 'y) (var-exp 'x))) test-env)
              13)
   (test-exn "- for nothing"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp '-)
                                      '()) test-env)))
   (test-eqv? "- operator for multiple numbers"
              (eval-exp (app-exp (var-exp '-)
                                 (list (lit-exp 1) (lit-exp 2) (lit-exp 3) (lit-exp 4))) test-env)
              -8)
   ;; multiply
   (test-eqv? "* operator in init-env"
              (eval-exp (app-exp (var-exp '*)
                                 (list (var-exp 'y) (var-exp 'x))) init-env)
              966)
   (test-eqv? "* operator for nothing"
              (eval-exp (app-exp (var-exp '*)
                                 '()) test-env)
              1)
   (test-eqv? "* operator for multiple numbers"
              (eval-exp (app-exp (var-exp '*)
                                 (list (lit-exp 1) (lit-exp 2) (lit-exp 3) (lit-exp 4))) test-env)
              24)
   ;; divide
   (test-eqv? "/ operator in test-env"
              (eval-exp (app-exp (var-exp '/)
                                 (list (var-exp 'y) (var-exp 'x))) test-env)
              (/ 23 10))
   (test-exn "/ for nothing"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp '/)
                                      '()) test-env)))
   (test-eqv? "/ operator for multiple numbers"
              (eval-exp (app-exp (var-exp '/)
                                 (list (lit-exp 10) (lit-exp 2) (lit-exp 1) (lit-exp 4) (lit-exp 5))) test-env)
              (/ 1 4))
   ;; add1
   (test-eqv? "add1 for number"
              (eval-exp (app-exp (var-exp 'add1)
                                 (list (lit-exp 10))) test-env)
              11)
   (test-exn "add1 for nothing"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'add1)
                                      '()) test-env)))
   (test-exn "add1 for multiple number"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'add1)
                                      (list (lit-exp 10) (lit-exp 9) (lit-exp 'x))) test-env)))
   (test-eqv? "add1 for env variable"
              (eval-exp (app-exp (var-exp 'add1)
                                 (list (var-exp 'y))) test-env)
              24)
   ;; sub1
   (test-eqv? "sub1 for number"
              (eval-exp (app-exp (var-exp 'sub1)
                                 (list (lit-exp 10))) test-env)
              9)
   (test-exn "sub1 for nothing"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'sub1)
                                      '()) test-env)))
   (test-exn "sub1 for multiple number"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'sub1)
                                      (list (lit-exp 10) (lit-exp 9) (lit-exp 'x))) test-env)))
   (test-eqv? "sub1 for env variable"
              (eval-exp (app-exp (var-exp 'sub1)
                                 (list (var-exp 'y))) test-env)
              22)
   ;;; negate
   (test-eqv? "negate for number"
              (eval-exp (app-exp (var-exp 'negate)
                                 (list (lit-exp 9))) test-env)
              -9)
   (test-exn "negate for nothing"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'negate)
                                      '()) test-env)))
   (test-exn "negate for multiple number"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'negate)
                                      (list (lit-exp 10) (lit-exp 9) (lit-exp 'x))) test-env)))
   (test-eqv? "negate for env variable"
              (eval-exp (app-exp (var-exp 'negate)
                                 (list (var-exp 'y))) test-env)
              -23)
   ;;; list
   (test-equal? "list for number and env variable"
                (eval-exp (app-exp (var-exp 'list)
                                   (list (lit-exp 9) (var-exp 'x))) test-env)
                '(9 10))
   (test-equal? "Empty list"
                (eval-exp (app-exp (var-exp 'list)
                                   '()) test-env)
                '())
   (test-equal? "nested lists"
                (eval-exp (app-exp (var-exp 'list)
                                   (list (app-exp (var-exp 'list) (list (lit-exp 8) (lit-exp 9))) (lit-exp 10))) test-env)
                '((8 9) 10))
   ;;; cons
   (test-equal? "cons for number and env variable"
                (eval-exp (app-exp (var-exp 'cons)
                                   (list (lit-exp 9) (var-exp 'x))) test-env)
                (cons 9 10))
   (test-exn "cons for 1 number"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'cons)
                                      (list (lit-exp 9))) test-env)))
   (test-exn "cons for 3 or more numbers"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'cons)
                                      (list (lit-exp 9) (lit-exp 16) (var-exp 'x))) test-env)))
   (test-equal? "make a list with cons"
                (eval-exp (app-exp (var-exp 'cons)
                                   (list (lit-exp 1) (app-exp (var-exp 'cons)
                                                              (list (lit-exp 2) (app-exp (var-exp 'cons)
                                                                                         (list (lit-exp 3) (var-exp 'null))))))) test-env)
                '(1 2 3))
   ;;; car
   (test-equal? "car on a pair of for number and env variable"
                (eval-exp (app-exp (var-exp 'car)
                                   (list (app-exp (var-exp 'cons) (list (var-exp 'x) (lit-exp 9))))) test-env)
                10)
   (test-equal? "car on a list"
                (eval-exp (app-exp (var-exp 'car)
                                   (list (app-exp (var-exp 'list) (list (lit-exp 8) (lit-exp 9) (lit-exp 10))))) test-env)
                8)
   (test-equal? "car on a nested lists"
                (eval-exp (app-exp (var-exp 'car)
                                   (list (app-exp (var-exp 'list) (list (app-exp (var-exp 'list) (list (lit-exp 8) (var-exp 'y))) (lit-exp 9))))) test-env)
                '(8 23))
   (test-equal? "car on a pair"
                (eval-exp (app-exp (var-exp 'car)
                                   (list (app-exp (var-exp 'cons) (list (lit-exp 1) (lit-exp 2))))) test-env)
                1)
   (test-exn "car takes 2 arguments"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'car) (list (lit-exp 1) (lit-exp 2))) test-env)))
   (test-exn "car takes an empty list"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'car) (list (var-exp 'null))) test-env)))
   ;;; cdr
   (test-equal? "cdr on a list of for number and env variable"
                (eval-exp (app-exp (var-exp 'cdr)
                                   (list (app-exp (var-exp 'list) (list (lit-exp 1) (lit-exp 2) (var-exp 'x))))) test-env)
                '(2 10))
   (test-equal? "cdr on a list of 1 element"
                (eval-exp (app-exp (var-exp 'cdr)
                                   (list (app-exp (var-exp 'list) (list (lit-exp 1))))) test-env)
                '())
   (test-equal? "cdr on a nested lists"
                (eval-exp (app-exp (var-exp 'cdr)
                                   (list (app-exp (var-exp 'list) (list (lit-exp 1) (app-exp (var-exp 'list) (list (lit-exp 2) (lit-exp 3))))))) test-env)
                '((2 3)))
   (test-equal? "cdr on a pair"
                (eval-exp (app-exp (var-exp 'cdr)
                                   (list (app-exp (var-exp 'cons) (list (lit-exp 1) (lit-exp 2))))) test-env)
                2)
   (test-exn "cdr takes 2 arguments"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'cdr) (list (lit-exp 1) (lit-exp 2))) test-env)))
   (test-exn "cdr takes an empty list"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'cdr) (list (var-exp 'null))) test-env)))
   ;;; eqv?
   (test-equal? "False output of eqv?"
                (eval-exp (app-exp (var-exp 'eqv?) (list (lit-exp 1) (lit-exp 2))) test-env)
                'False)
   (test-equal? "True output of eqv?"
                (eval-exp (app-exp (var-exp 'eqv?) (list (lit-exp 2) (lit-exp 2))) test-env)
                'True)
   (test-exn "eqv? takes only 1 argument"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'eqv?) (list (lit-exp 2))) test-env)))
   (test-exn "eqv? takes more than 2 arguments"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'eqv?) (list (lit-exp 2) (lit-exp 3) (lit-exp 4))) test-env)))
   ;;; lt?
   (test-equal? "False output of lt?"
                (eval-exp (app-exp (var-exp 'lt?) (list (lit-exp 2) (lit-exp 1))) test-env)
                'False)   
   ;;; gt?
   (test-equal? "True output of gt?"
                (eval-exp (app-exp (var-exp 'gt?) (list (lit-exp 2) (lit-exp 1))) test-env)
                'True)
   ;;; leq?
   (test-equal? "True output of leq?"
                (eval-exp (app-exp (var-exp 'leq?) (list (lit-exp 2) (lit-exp 2))) test-env)
                'True)
   ;;; geq?
   (test-equal? "True output of leq?"
                (eval-exp (app-exp (var-exp 'geq?) (list (lit-exp 1) (lit-exp 2))) test-env)
                'False)
   ;;; null?
   (test-equal? "True output of null?"
                (eval-exp (parse '(null? null)) test-env)
                'True)
   (test-equal? "False output of null?"
                (eval-exp (parse '(null? 23)) test-env)
                'False)
   (test-exn "null? takes more than 1 argument"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'null?) (list (lit-exp 1) (lit-exp 2))) test-env)))
   ;;; list?
   (test-equal? "True output of list?"
                (eval-exp (app-exp (var-exp 'list?) (list (app-exp (var-exp 'list) (list (lit-exp 1) (lit-exp 2) (lit-exp 3))))) test-env)
                'True)
   (test-equal? "False output of list?"
                (eval-exp (parse '(list? 23)) test-env)
                'False)
   (test-exn "list? takes more than 1 argument"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'list?) (list (lit-exp 1) (lit-exp 2))) test-env)))
   ;;; number?
   (test-equal? "True output of number?"
                (eval-exp (parse '(number? 23)) test-env)                
                'True)
   (test-equal? "False output of number?"
                (eval-exp (app-exp (var-exp 'number?) (list (app-exp (var-exp 'list) (list (lit-exp 1) (lit-exp 2) (lit-exp 3))))) test-env)
                'False)
   (test-exn "number? takes more than 1 argument"
             exn:fail?
             (λ () (eval-exp (app-exp (var-exp 'number?) (list (lit-exp 1) (lit-exp 2))) test-env)))
   ;;; ite-exp
   (test-equal? "Conditional statement execute then branch"
                (eval-exp (ite-exp (app-exp (var-exp 'number?) (list (lit-exp 2)))
                                   (lit-exp 100)
                                   (lit-exp 1)) test-env)
                100)
   (test-equal? "Conditional statement execute else branch"
                (eval-exp (ite-exp (app-exp (var-exp 'gt?) (list (var-exp 'x) (var-exp 'y)))
                                   (app-exp (var-exp 'add1) (list (var-exp 'x)))
                                   (app-exp (var-exp 'add1) (list (var-exp 'y)))) test-env)
                24)
   ;;; let-exp
   (test-equal? "Single let expression"
                (eval-exp (let-exp '(a b) (list (lit-exp 1) (lit-exp 5))
                                   (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b)))) test-env)
                6)
   (test-equal? "Nested let expression"
                (eval-exp (let-exp '(a b) (list (app-exp (var-exp '*) (list (lit-exp 2) (lit-exp 3))) (lit-exp 24))
                                   (let-exp '(c) (list (app-exp (var-exp '-) (list (var-exp 'b) (var-exp 'a))))
                                            (app-exp (var-exp '*) (list (var-exp 'c) (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b))))))) test-env)
                540)
   ;;; lambda-exp
   (test-equal? "Single lambda expression"
                (eval-exp (app-exp (lambda-exp '(x) (var-exp 'x)) (list (lit-exp 1))) test-env)
                1)
   (test-equal? "Lambda has 2 parameters"
                (eval-exp (app-exp (lambda-exp '(x y) (app-exp (var-exp '*) (list (var-exp 'x) (var-exp 'y))))
                                   (list (lit-exp 2) (lit-exp 4))) test-env)
                8)
   (test-equal? "Lambda inside a let"
                (eval-exp (let-exp '(sqr) (list (lambda-exp '(x) (app-exp (var-exp '*) (list (var-exp 'x) (var-exp 'x)))))
                                   (app-exp (var-exp 'sqr) (list (lit-exp 64)))) test-env)
                4096)
   (test-equal? "Nested lambda and let"
                (eval-exp (let-exp
                           '(sqr)
                           (list (lambda-exp '(x) (app-exp (var-exp '*) (list (var-exp 'x) (var-exp 'x)))))
                           (let-exp '(cube) (list (lambda-exp '(x) (app-exp (var-exp '*)
                                                                            (list (var-exp 'x) (app-exp (var-exp 'sqr) (list (var-exp 'x)))))))
                                    (app-exp (var-exp 'cube) (list (lit-exp 3))))) test-env)
                27)
   (test-exn "Invalid lambda"
             exn:fail?
             (λ () (eval-exp (app-exp (lambda-exp '(x y) (var-exp 'x)) (list (lit-exp 1))) test-env)))
   ;;; set-exp combined with begin-exp
   (test-equal? "Combination 1"
                (eval-exp (let-exp '(x y)
                                   (list (lit-exp 1) (lit-exp 2))
                                   (begin-exp (list (set-exp 'x (lit-exp 23)) (app-exp (var-exp '+) (list (var-exp 'x) (var-exp 'y))))))
                          test-env)
                25)
   (test-equal? "Combination 2"
                (eval-exp (let-exp '(x)
                                   (list (lit-exp 0))
                                   (begin-exp (list (set-exp 'x (lit-exp 23)) (app-exp (var-exp '+) (list (var-exp 'x) (lit-exp 5))))))
                          test-env)
                28)
   ;;; letrec
   (test-equal? "letrec 1"
                (eval-exp (let-exp
                           '(length)
                           (list (lit-exp 0))
                           (let-exp
                            '(g2946849)
                            (list
                             (lambda-exp
                              '(lst)
                              (ite-exp
                               (app-exp (var-exp 'null?) (list (var-exp 'lst)))
                               (lit-exp 0)
                               (app-exp (var-exp 'add1) (list (app-exp (var-exp 'length) (list (app-exp (var-exp 'cdr) (list (var-exp 'lst))))))))))
                            (begin-exp (list (set-exp 'length (var-exp 'g2946849)) (app-exp (var-exp 'length) (list (app-exp (var-exp 'list) (list (lit-exp 10) (lit-exp 20) (lit-exp 30))))))))) test-env)
                3)

   (test-equal? "letrec 2"
                (eval-exp (let-exp
                           '(fac)
                           (list (lit-exp 0))
                           (let-exp
                            '(g2978155)
                            (list
                             (lambda-exp
                              '(x)
                              (ite-exp
                               (app-exp (var-exp 'eqv?) (list (var-exp 'x) (lit-exp 0)))
                               (lit-exp 1)
                               (app-exp (var-exp '*) (list (var-exp 'x) (app-exp (var-exp 'fac) (list (app-exp (var-exp 'sub1) (list (var-exp 'x))))))))))
                            (begin-exp (list (set-exp 'fac (var-exp 'g2978155)) (app-exp (var-exp 'fac) (list (lit-exp 4))))))) test-env)
                24)

   (test-equal? "letrec 3"
                (eval-exp (let-exp
                           '(fac)
                           (list (lit-exp 0))
                           (let-exp
                            '(g2995372)
                            (list
                             (lambda-exp
                              '(x)
                              (ite-exp
                               (app-exp (var-exp 'eqv?) (list (var-exp 'x) (lit-exp 0)))
                               (lit-exp 1)
                               (app-exp (var-exp '*) (list (var-exp 'x) (app-exp (var-exp 'fac) (list (app-exp (var-exp 'sub1) (list (var-exp 'x))))))))))
                            (begin-exp (list (set-exp 'fac (var-exp 'g2995372)) (app-exp (var-exp 'fac) (list (lit-exp 10))))))) test-env)
                3628800)


   (test-equal? "letrec 4"
                (eval-exp (let-exp
                           '(even? odd?)
                           (list (lit-exp 0) (lit-exp 0))
                           (let-exp
                            '(g3016283 g3016284)
                            (list
                             (lambda-exp
                              '(n)
                              (ite-exp (app-exp (var-exp 'eqv?) (list (lit-exp 0) (var-exp 'n))) (var-exp 'True) (app-exp (var-exp 'odd?) (list (app-exp (var-exp 'sub1) (list (var-exp 'n)))))))
                             (lambda-exp
                              '(n)
                              (ite-exp (app-exp (var-exp 'eqv?) (list (lit-exp 0) (var-exp 'n))) (var-exp 'False) (app-exp (var-exp 'even?) (list (app-exp (var-exp 'sub1) (list (var-exp 'n))))))))
                            (begin-exp (list (set-exp 'even? (var-exp 'g3016283)) (set-exp 'odd? (var-exp 'g3016284)) (app-exp (var-exp 'even?) (list (lit-exp 5))))))) test-env)
                'False)
   ))































