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
         Expr?
         ExprHead?         ; Type predicate for ExprHead
         ExprHead->name
         ; Export enum values
         Const Var App Lambda Quote Define If Begin UnNamedLet NamedLet LetRec LetStar Cond CondClause And Or Load Set! Do DoLet DoFinal Nil Body 
         Bind Bindings Arg S-Expr
)


; Expr-head
;    Represents the head of an expression in the AST.
; Note:
;    Currently, there are a number of heads that have been added for now. 
;    Many of them may become obsolete as a result of desugar and other mechanisms. 
(define-enum-type ExprHead
  (Const  ; Constant value
   Nil    ; Nil value
   Var    ; Variable 
   App    ; Application
   Lambda ; Lambda function
   Arg    ; Argument of a function
   Quote  ; Quoted expression
   Define ; Define a variable or function 

   If     ; If expression           | MEMO: These heads may be integrated in the future 
   Cond   ; Conditional expression  |       by desugaring `Cond` to `If`
   CondClause ; Conditional clause
  
   Begin  ; Begin block (MEMO: This head may be deprecated in the future) 
   
   UnNamedLet  ; Unnamed let expression     |
   NamedLet    ; Named let expression       | EMO: These heads may be integrated in the future
   LetRec      ; Recursive let expression   |      by desugaring
   LetStar     ; Let star expression        | 

   And         ; Logical AND expression  
   Or          ; Logical OR expression
   Load        ; Load a module or file (MEMO: This is not a special form, but a function call. But in 
   Set!        ; Set! operator 
   Do          ; Do expression
     DoLet       ; Let expression inside a Do expression
     DoFinal     ; Final expression inside a Do expression
          
   Body        ; Body of a function or block
   Bind        ; Binding expression
     Bindings  ; list of bindings

   S-Expr      ; S-expression
  ))

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
    [(== Const) "Const"]
    [(== Var) "Var"]
    [(== App) "App"]
    [(== Lambda) "Lambda"]
    [(== Quote) "Quote"]
    [(== Define) "Define"]
    [(== If) "If"]
    [(== Begin) "Begin"]
    [(== UnNamedLet) "UnNamedLet"]
    [(== NamedLet) "NamedLet"]
    [(== LetRec) "LetRec"]
    [(== LetStar) "LetStar"]
    [(== Cond) "Cond"]
    [(== And) "And"]
    [(== Or) "Or"]
    [(== Load) "Load"]
    [(== Set!) "Set!"]
    [(== Do) "Do"]
    [(== DoLet) "DoLet"]
    [(== DoFinal) "DoFinal"]
    [(== Nil) "Nil"]
    [(== Body) "Body"]
    [(== Bind) "Bind"]
    [(== Bindings) "Bindings"]
    [(== Arg) "Arg"]
    [(== UnNamedLet) "UnNamedLet"]
    [(== NamedLet) "NamedLet"]
    [(== CondClause) "CondClause"]
    [(== S-Expr) "S-Expr"]
    [else (error 'ExprHead->name "Unknown ExprHead: ~a" head)]))
    

;  pretty-print-Expr
;     Formats an Expr instance for readable display with appropriate line breaks.
;  Arguments:
;      expr - The expression to format (Expr, list of Expr, or primitive value)
;      [depth] - Current nesting depth (optional, used for internal recursion)
;  Returns:
;      A string representation of the expression
;  Example:
;      (pretty-print-Expr (make-expr Const (list 'Num 42) loc)) ; => "Const(Num 42)"
(define (pretty-print-Expr expr [depth 0])
  (define (indent d) (make-string (* d 2) #\space))
  
  (cond
    ; Handle Expr instances
    [(Expr? expr)
     (let ([head (Expr-head expr)]
           [args (Expr-args expr)])
       (if (null? args)
           (format "~a()" (ExprHead->name head))
           (let* ([next-depth (add1 depth)]
                  [arg-strings (map (lambda (arg) (pretty-print-Expr arg next-depth)) args)]
                  [has-complex-args? (ormap (lambda (arg)
                                              (or (Expr? arg)
                                                  (and (list? arg) (not (null? arg)))))
                                            args)])
             (if has-complex-args?
                 (format "~a(\n~a~a\n~a)"
                         (ExprHead->name head)
                         (indent next-depth)
                         (string-join arg-strings (format "\n~a" (indent next-depth)))
                         (indent depth))
                 (format "~a(~a)"
                         (ExprHead->name head)
                         (string-join arg-strings " "))))))]
    
    ; Handle lists of expressions
    [(list? expr)
     (if (null? expr)
         "()"
         (let* ([next-depth (add1 depth)]
                [item-strings (map (lambda (item) (pretty-print-Expr item next-depth)) expr)])
           (if (ormap (lambda (item)
                         (or (Expr? item)
                             (and (list? item) (not (null? item)))))
                      expr)
               (format "(\n~a~a\n~a)"
                       (indent next-depth)
                       (string-join item-strings (format "\n~a" (indent next-depth)))
                       (indent depth))
               (format "(~a)" (string-join item-strings " ")))))]
    
    ; Handle primitive values
    [(number? expr) (number->string expr)]
    [(string? expr) (format "\"~a\"" expr)]
    [(boolean? expr) (if expr "#t" "#f")]
    [(symbol? expr) (symbol->string expr)]
    
    ; Default case
    [else (format "~a" expr)]))

