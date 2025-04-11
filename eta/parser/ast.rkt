#lang racket

(require racket/string)
(require rebellion/type/enum)
(require "tokenizer.rkt")

(provide Expr              ; Struct type
         make-expr         ; Constructor with type checking
         pretty-print-Expr ; Pretty printer for Expr
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


(define (ExprHead->name head)
  (match head
    [Const "Const"]
    [Var "Var"]
    [App "App"]
    [Lambda "Lambda"]
    [Quote "Quote"]
    [Define "Define"]
    [If "If"]
    [Begin "Begin"]
    [Let "Let"]
    [LetRec "LetRec"]
    [LetStar "LetStar"]
    [Cond "Cond"]
    [And "And"]
    [Or "Or"]
    [Load "Load"])
    )
    

;  pretty-print-Expr
;     Formats an Expr instance for readable display.
;  Arguments:
;      expr - The expression to format (an Expr struct)
;  Returns:
;      A string representation of the expression
;  Example:
;      (pretty-print-Expr (make-expr Const (list 42) loc)) ; => "Const(42)"
(define (pretty-print-Expr expr)
  (match expr
    [(Expr head args loc)
     (format "~a(~a)" 
             (ExprHead->name head) 
             (string-join 
              (map (lambda (arg)
                     (if (Expr? arg)
                         (pretty-print-Expr arg)
                         (format "~a" arg)))
                   args)
              " "))]
    [other (format "~a" other)]))