#lang racket
;Minh Nguyen
(provide parse
         lit-exp
         lit-exp?
         lit-exp-num
         var-exp
         var-exp?
         var-exp-symbol
         app-exp
         app-exp?
         app-exp-proc
         app-exp-args
         ite-exp
         ite-exp?
         ite-exp-cond
         ite-exp-then
         ite-exp-else
         let-exp
         let-exp?
         let-exp-exps
         let-exp-vals
         let-exp-proc
         lambda-exp
         lambda-exp?
         lambda-exp-params
         lambda-exp-body
         set-exp
         set-exp?
         set-exp-sym
         set-exp-exp
         begin-exp
         begin-exp?
         begin-exp-exps
         )


;;; lit-exp
(struct lit-exp (num) #:transparent)

;;; var-exp
(struct var-exp (symbol) #:transparent)

;;; app-exp
(struct app-exp (proc args) #:transparent)

;;; ite-exp
(struct ite-exp (cond then else) #:transparent)

;;; let-exp
(struct let-exp (exps vals proc) #:transparent)

;;; lambda-exp
(struct lambda-exp (params body) #:transparent)

;;; set-exp
(struct set-exp (sym exp) #:transparent)

;;; begin-exp
(struct begin-exp (exps) #:transparent)



;;; parse
(define (parse input)
  (letrec ([parse-error (λ () (error 'parse "Invalid syntax ~s" input))])
    (cond [(number? input) (lit-exp input)]
          [(symbol? input) (var-exp input)]
          [(list? input)
           (cond [(empty? input) (parse-error)]
                 ;; if
                 [(equal? 'if (first input)) (cond [(equal? 4 (length input)) (ite-exp (parse (second input))
                                                                                       (parse (third input))
                                                                                       (parse (fourth input)))]
                                                   [else (parse-error)])]
                 ;; let
                 [(equal? 'let (first input)) (cond [(equal? 3 (length input)) (let-exp (map first (second input))
                                                                                        (map parse (map second (second input)))
                                                                                        (parse (third input)))]
                                                    [else (parse-error)])]
                 ;; letrec
                 [(equal? 'letrec (first input))
                  (cond [(equal? 3 (length input)) (let ([syms (map first (second input))]
                                                         [exps (map second (second input))]
                                                         [body (third input)])
                                                     (let-exp syms
                                                              (map (λ (s) (lit-exp 0)) syms)
                                                              (let ([new-syms (map (λ (s) (gensym)) syms)])
                                                                (let-exp new-syms
                                                                         (map parse exps)
                                                                         ;; begin
                                                                         (begin-exp (foldr (λ (s new-s acc) 
                                                                                             (cons (set-exp s (parse new-s)) acc))
                                                                                           (list (parse body))
                                                                                           syms 
                                                                                           new-syms))))))]
                        [else (parse-error)])]
                 ;; λ
                 [(equal? 'lambda (first input)) (cond [(equal? 3 (length input)) (lambda-exp (second input)
                                                                                              (parse (third input)))]
                                                       [else (parse-error)])]
                 ;; set!
                 [(equal? 'set! (first input)) (cond [(equal? 3 (length input)) (set-exp (second input) (parse (third input)))]
                                                     [else (parse-error)])]
                 ;; begin
                 [(equal? 'begin (first input)) (begin-exp (map parse (rest input)))]
                 ;; else
                 [else (app-exp (parse (first input)) (map parse (rest input)))])]
          [else (parse-error)])))










