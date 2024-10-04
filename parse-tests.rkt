#lang racket
;Minh Nguyen

(require rackunit)
(require "parse.rkt")

(provide parse-tests)

(define parse-tests
  (test-suite
   "Parse tests"
   ;;; lit-exp
   (test-pred "Literal"
              lit-exp?
              (parse 5))
   (test-equal? "Getter"
                (lit-exp-num (parse 15))
                15)
   ;;; var-exp
   (test-pred "Test var-exp"
              var-exp?
              (parse 'sym))
   (test-equal? "Symbol"              
                (var-exp-symbol (parse 'sym))
                'sym)
   (test-exn "More than 1 input"
             exn:fail?
             (λ () (parse 'sym 5)))
   ;;; app-exp
   (test-exn "Empty input"
             exn:fail?
             (λ () (parse '())))
   (test-equal? "Only procedure"
                (parse '(foo))
                (app-exp (var-exp 'foo) '()))
   (test-pred "Procedure of app-exp is var-exp"
              var-exp?
              (app-exp-proc (parse '(foo 1 2 3))))
   (test-pred "Arguments of app-exp is a list"
              list?
              (app-exp-args (parse '(foo 1 2 3))))
   (test-equal? "Procedure with arguments"
                (parse '(foo x y z))
                (app-exp (var-exp 'foo) (list (var-exp 'x) (var-exp 'y) (var-exp 'z))))
   (test-equal? "Procedure with constant"
                (parse '(barz 1))
                (app-exp (var-exp 'barz) (list (lit-exp 1))))
   ;;; ite-exp
   (test-equal? "Conditional statement with eqv?"
                (parse '(if (eqv? 1 2) 5 6))
                (ite-exp (app-exp (var-exp 'eqv?) (list (lit-exp 1) (lit-exp 2))) (lit-exp 5) (lit-exp 6)))
   (test-equal? "Conditional statement with symbol"
                (parse '(if (number? y) x 6))
                (ite-exp (app-exp (var-exp 'number?) (list (var-exp 'y))) (var-exp 'x) (lit-exp 6)))
   (test-exn "Conditional statement doesn't have enough elements"
             exn:fail?
             (λ () (parse '(if (eqv? 1 2) 5))))
   ;;; let-exp
   (test-equal? "Let expressions with numbers"
                (parse '(let ([a 1] [b 5])(+ a b)))
                (let-exp '(a b) (list (lit-exp 1) (lit-exp 5)) (app-exp (var-exp '+) (list (var-exp 'a) (var-exp 'b)))))
   (test-equal? "Let expressions with all symbols"
                (parse '(let ([a x]  [b y])  a))
                (let-exp '(a b) (list (var-exp 'x) (var-exp 'y)) (var-exp 'a)))
   (test-exn "Let expressions statement doesn't have enough elements"
             exn:fail?
             (λ () (parse '(let ([a x]  [b y]) ))))
   ;;; lambda-exp
   (test-equal? "Lambda expression with 1 params"
                (parse '(lambda (x y) (+ x y)))
                (lambda-exp '(x y) (app-exp (var-exp '+) (list (var-exp 'x) (var-exp 'y)))))
   (test-equal? "Lambda expression inside a let expression"
                (parse '(let ([x 10])
                          ((lambda () (+ x 5)))))
                (let-exp '(x) (list (lit-exp 10))
                         (app-exp (lambda-exp '() (app-exp (var-exp '+) (list (var-exp 'x) (lit-exp 5)))) '())))
   (test-exn "Lambda expressions statement doesn't have enough elements"
             exn:fail?
             (λ () (parse '(lambda (+ x y)))))
   ;;; set-exp
   (test-equal? "Simple set expression"
                (parse '(set! + -))
                (set-exp '+ (var-exp '-)))
   (test-exn "Invalid set expression"
             exn:fail?
             (λ () (parse '(parse '(set! x 10 9)))))
   (test-exn "Empty set expression"
             exn:fail?
             (λ () (parse '(parse '(set! x)))))
   ;;; begin-exp
   (test-equal? "Begin without any expression"
                (parse '(begin))
                (begin-exp '()))
   (test-equal? "Begin with multiple expression"
                (parse '(begin (set! x 23)
                               (+ x y)))
                (begin-exp (list (set-exp 'x (lit-exp 23)) (app-exp (var-exp '+) (list (var-exp 'x) (var-exp 'y))))))
   ;;; letrec
   (test-true "Parsing a letrec should return a let-exp"
              (let-exp? (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 4)))))
   (test-true "That let-exp should have a let-exp as the body"
              (let-exp? (let-exp-proc (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 4))))))
   (test-true "The inner let-exp should have a begin-exp as the body"
              (begin-exp? (let-exp-proc (let-exp-proc (parse '(letrec ([fac (lambda (x) (if (eqv? x 0) 1 (* x (fac (sub1 x)))))]) (fac 4)))))))
   ))

























