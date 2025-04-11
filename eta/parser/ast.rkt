#lang racket

(require racket/string)
(require rebellion/type/enum)
(require "tokenizer.rkt")

(provide Expr              ; Struct type
         make-expr         ; Constructor with type checking
         pretty-print
         Expr-head
         Expr-args
         Expr-loc
         ExprHead?         ; Type predicate for ExprHead
         ; Export enum values
         Const Var App Lambda Quote Define If Begin Let LetRec LetStar Cond And Or Load)

(define-enum-type ExprHead
  (Const Var App Lambda Quote Define If Begin Let LetRec LetStar Cond And Or Load))

(struct Expr (head args loc) #:transparent)

;  make-expr
;     Creates a new Expr with type checking for head.
;  Arguments:
;      head - The expression head (must be an ExprHead)
;      args - List of expression arguments
;      loc - Location information
;  Returns:
;      A new Expr instance
;  Raises:
;      exn:fail - If head is not an ExprHead
(define (make-expr head args loc)
  (unless (ExprHead? head)
    (error 'make-expr "Expected an ExprHead for 'head', got: ~a" head))
  (Expr head args loc))