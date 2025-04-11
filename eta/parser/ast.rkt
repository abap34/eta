#lang racket

(require racket/string)
(require rebellion/type/enum)
(require "tokenizer.rkt")

(provide Expr
         pretty-print
         Expr-head
         Expr-args
         expr-deep-equal?
         expr-equal?)

(define-enum-type ExprHead
  (Const Var App Lambda Quote Define If Begin Let LetRec LetStar Cond And Or Load))






(struct Expr (head args loc) #:transparent)

(define (expr-deep-equal? x y)
  (cond
    [(and (Expr? x) (Expr? y)) (expr-equal? x y)]
    [(and (list? x) (list? y)) (args-equal? x y)]
    [else (equal? x y)]))

(define (args-equal? a1 a2)
  (cond
    [(and (null? a1) (null? a2)) #t]
    [(or (null? a1) (null? a2)) #f]
    [else
     (and (expr-deep-equal? (car a1) (car a2))
          (args-equal? (cdr a1) (cdr a2)))]))

(define (expr-equal? e1 e2)
  (and (Expr? e1) (Expr? e2)
       (eq? (Expr-head e1) (Expr-head e2))
       (args-equal? (Expr-args e1) (Expr-args e2))
       (location-equal? (Expr-loc e1) (Expr-loc e2))))
