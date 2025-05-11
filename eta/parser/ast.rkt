#lang racket

(require "tokenizer.rkt")

(provide Expr             
         make-expr        
         pretty-print-Expr 
         Expr-head
         Expr-args
         Expr-loc
         Expr?
         ExprHead?     
         ExprHead->name

         make-const
         make-app
         make-var
         make-lambda
         make-define
         make-single-arg
         make-list-arg
         make-quote
         make-setbang
         make-unnamed-let
         make-named-let
         make-letstar
         make-letrec
         make-ifthen
         make-ifthenelse
         make-cond-noelse
         make-cond-clause
         make-cond-else
         make-and
         make-or
         make-begin
         make-do-let
         make-do-final
         make-do
         make-body
         make-bind
         make-bindings
         make-load
         make-sexpr 

         assert-expr
         assert-head
)

; ExprHead
;    Represents the head of an expression in the AST.
; Note:
;    - Some of these head may be deprecated in the future by macros in stdlib.
(define (ExprHead? head)
  (or (equal? head 'ConstHead)       ; Constant value
      (equal? head 'IdHead)          ; Variable
      (equal? head 'AppHead)         ; Application
      (equal? head 'LambdaHead)      ; Lambda function
      (equal? head 'QuoteHead)       ; Quoted expression
      (equal? head 'DefineHead)      ; Define a variable or function
      (equal? head 'IfHead)          ; If expression
      (equal? head 'CondHead)        ; Conditional expression
      (equal? head 'CondClauseHead)  ; Conditional clause
      (equal? head 'BeginHead)       ; Begin block
      (equal? head 'UnNamedLetHead)  ; Unnamed let expression
      (equal? head 'NamedLetHead)    ; Named let expression
      (equal? head 'LetRecHead)      ; Recursive let expression
      (equal? head 'LetStarHead)     ; Let star expression
      (equal? head 'AndHead)         ; AND expression
      (equal? head 'OrHead)          ; OR expression
      (equal? head 'LoadHead)        ; Load a file (MEMO: This is not a special form, but a function call. But in)
      (equal? head 'SetHead)         ; Breaking the value of a variable
      (equal? head 'DoHead)          ; Do expression
      (equal? head 'DoLetHead)       ; Let expression inside a Do expression
      (equal? head 'DoFinalHead)     ; Final expression inside a Do expression
      (equal? head 'BodyHead)        ; Body of a function or block
      (equal? head 'BindHead)        ; Binding expression
      (equal? head 'BindingsHead)    ; list of bindings
      (equal? head 'ArgHead)         ; Argument expression
      (equal? head 'S-ExprHead)))    ; S-expression

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

;  ExprHead->name
;     Converts an ExprHead symbol to its string representation.
;  Arguments:
;      head - The ExprHead symbol (e.g., 'ConstHead)
;  Returns:
;      The string name of the head (e.g., "Const")
;  Example:
;      (ExprHead->name 'ConstHead) ; => "Const"
(define (ExprHead->name head)
  (cond
    [(equal? head 'ConstHead)      "Const"]
    [(equal? head 'IdHead)         "Id"]
    [(equal? head 'AppHead)        "App"]
    [(equal? head 'LambdaHead)     "Lambda"]
    [(equal? head 'QuoteHead)      "Quote"]
    [(equal? head 'DefineHead)     "Define"]
    [(equal? head 'IfHead)         "If"]
    [(equal? head 'BeginHead)      "Begin"]
    [(equal? head 'UnNamedLetHead) "UnNamedLet"]
    [(equal? head 'NamedLetHead)   "NamedLet"]
    [(equal? head 'LetRecHead)     "LetRec"]
    [(equal? head 'LetStarHead)    "LetStar"]
    [(equal? head 'CondHead)       "Cond"]
    [(equal? head 'CondClauseHead) "CondClause"]
    [(equal? head 'AndHead)        "And"]
    [(equal? head 'OrHead)         "Or"]
    [(equal? head 'LoadHead)       "Load"]
    [(equal? head 'SetHead)        "Set!"]
    [(equal? head 'DoHead)         "Do"]
    [(equal? head 'DoLetHead)      "DoLet"]
    [(equal? head 'DoFinalHead)    "DoFinal"]
    [(equal? head 'BodyHead)       "Body"]
    [(equal? head 'BindHead)       "Bind"]
    [(equal? head 'BindingsHead)   "Bindings"]
    [(equal? head 'ArgHead)        "Arg"]
    [(equal? head 'S-ExprHead)     "S-Expr"]
    [else (error 'ExprHead->name   "Unknown ExprHead: ~a" head)]))

;  pretty-print-Expr
;     Formats an Expr instance for readable display with appropriate line breaks.
;  Arguments:
;      expr - The expression to format (Expr, list of Expr, or primitive value)
;      [depth] - Current nesting depth (optional, used for internal recursion)
;  Returns:
;      A string representation of the expression
;  Example:
;      (pretty-print-Expr (make-expr 'ConstHead (list 'Num 42) loc)) ; => "Const(Num 42)"
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


(define (assert-expr arg func-name context)
  (unless (Expr? arg)
    (error func-name (format "Expected an Expr for ~a, got: ~a" context arg))))

(define (assert-head head arg func-name context)
  (assert-expr arg func-name context)
  (unless (ExprHead? head)
    (error func-name (format "Expected an ExprHead for ~a, got: ~a" context head)))

  (unless (equal? head (Expr-head arg))
    (error func-name (format "Expected ~a for ~a, got: ~a" head context (Expr-head arg)))))


(define (assert-string arg func-name context)
  (unless (string? arg)
    (error func-name (format "Expected a string for ~a, got: ~a" context arg))))

(define (assert-list-of-exprs arg func-name context)
  (unless (list? arg)
    (error func-name (format "Expected a list of Expr for ~a, got: ~a" context arg)))
  (map (lambda (item)
         (assert-expr item func-name context)) arg))

;; -- ast builders 

;  make-const
;     Create a constant expression node
;  Arguments:
;     location - Source location
;     tag - The type of the constant (e.g., number, string, boolean). All tags are defined in `devdocs/ast.md`.
;     value - The constant value (e.g., 42, "hello", #t)
;  Returns:
;     An Expr with Const head
(define (make-const location tag value)
  (make-expr 'ConstHead (list tag value) location))  ; TODO: validate tag

; make-voidnode
;     Create a void expression node
;  Arguments:
;     location - Source location
;  Returns:
;     An Expr with Const head with VoidTag
(define (make-voidnode location)
  (make-expr 'ConstHead (list 'VoidConstNode '()) location)) ; TODO: validate tag

;  make-symbol
;     Create a symbol (variable) expression node
;  Arguments:
;     location - Source location
;     name - The symbol name (string)
;  Returns:
;     An Expr with Var head
(define (make-var location name)
  (assert-string name 'make-var "name")
  (unless (not (string=? name ""))
    (error 'make-var "Expected a non-empty string for variable name, got: ~a" name))

  ; MEMO: Overwrite keyword is allowed. So we don't need to check like:
  ; (make-parse-error (format "Cannot use keyword ~a as variable name" name) location)
  (make-expr 'IdHead (list name) location))


; make-app
;     Create an application expression node
;  Arguments:
;     location - Source location
;     op - The operator to apply (Expr with any head).   
;     args - The arguments to the operator (list of Expr)
;  Returns:
;     An Expr with App head
(define (make-app location op args)
  (assert-expr op 'make-app "operator")
  (assert-list-of-exprs args 'make-app "arguments")
  
  (make-expr 'AppHead (list op args) location))  

; make-lambda
;    Create a lambda expression node
;  Arguments:
;     location - Source location
;     args - The arguments to the lambda (Expr with Arg head)
;     body - The body of the lambda (Expr with Body head)
;  Returns:
;     An Expr with Lambda head
(define (make-lambda location args body)
  (assert-head 'ArgHead args 'make-lambda "arguments")
  (assert-head 'BodyHead body 'make-lambda "body")
  
  (make-expr 'LambdaHead (list args body) location))


; make-define
;     Create a define expression node
; Arguments:
;     location - Source location
;     ident - The identifier for the definition (Expr with Id head)
;     value - The value to assign (Expr with any head)
;  Returns:
;     An Expr with Define head
(define (make-define location ident value)
  (assert-head 'IdHead ident 'make-define "identifier")
  (assert-expr value 'make-define "value")

  (make-expr 'DefineHead (list ident value) location))

; make-single-arg
;     Create an argument expression node
;  Arguments:
;     location - Source location
;     name - The argument name (string)
;  Returns:
;     An Expr with Arg head
(define (make-single-arg location name)
      (assert-string name 'make-single-arg "name")

      (make-expr 'ArgHead 
          (list
            '() ; no required args
           name ; single variadic arg
          )     ;
      location))

; make-list-arg
;     Create a list of argument expression nodes
;  Arguments:
;     location - Source location
;     required-args - List of required argument names (strings)
;     variadic-arg  - The variadic argument name (string)
;  Returns:
;     An Expr with Arg head
(define (make-list-arg location required-args variadic-arg)
  (unless (and (list? required-args) (andmap string? required-args))
    (error (format "Expected a list of strings for required args, got: ~a" required-args) location))
  (unless (or (string? variadic-arg) 
              (eq? variadic-arg '()))
    (error (format "Expected a string or empty list for variadic arg, got: ~a" variadic-arg) location))
  (make-expr 'ArgHead (list required-args variadic-arg) location))


; make-quote
;     Create a quote expression node
;  Arguments:
;     location - Source location
;     value - The quoted value (Expr)
;  Returns:
;     An Expr with Quote head
(define (make-quote location value)
  (assert-expr value 'make-quote "value")

  (make-expr 'QuoteHead (list value) location))  ; Quote has one argument


; make-setbang
;     Create a set! expression node
;  Arguments:
;     location - Source location
;     ident - The variable ident to set (Expr with Id head)
;     value - The value to assign (Expr)
;  Returns:
;     An Expr with Set! head
(define (make-setbang location ident value)
  (assert-head 'IdHead ident 'make-setbang "identifier")
  (assert-expr value 'make-setbang "value")
  
  (make-expr 'SetHead (list ident value) location))


; make-unnamed-let
;     Create a let (**not named**) expression node
;  Arguments:
;     location - Source location
;     bindings - binings (Expr with Bindings head)
;     body - The body of the let (Expr with Body head)
;  Returns:
;     An Expr with UnNamedLet head
(define (make-unnamed-let location bindings body)
  (assert-head 'BindingsHead bindings 'make-unnamed-let "bindings")
  (assert-head 'BodyHead body 'make-unnamed-let "body")
  
  (make-expr 'UnNamedLetHead (list bindings body) location))  

; make-named-let
;     Create a named let expression node
;  Arguments:
;     location - Source location
;     name - The name of the let (string)
;     bindings - The bindings (Expr with Bindings head)
;     body - The body of the let (Expr with Body head)
;  Returns:
;     An Expr with NamedLet head
(define (make-named-let location name bindings body)
  (assert-head 'IdHead name 'make-named-let "name")
  (assert-head 'BindingsHead bindings 'make-named-let "bindings")
  (assert-head 'BodyHead body 'make-named-let "body")
  
  (make-expr 'NamedLetHead (list name bindings body) location))

; make-letstar
;     Create a let* expression node
;  Arguments:
;     location - Source location
;     bindings - The bindings (Expr with Bindings head)
;     body - The body of the let* (Expr with Body head)
;  Returns:
;     An Expr with LetStar head
(define (make-letstar location bindings body)
  (assert-head 'BindingsHead bindings 'make-letstar "bindings")
  (assert-head 'BodyHead body 'make-letstar "body")
  
  (make-expr 'LetStarHead (list bindings body) location))

; make-letrec
;     Create a letrec expression node
;  Arguments:
;     location - Source location
;     bindings - The bindings (Expr with Bindings head)
;     body - The body of the letrec (Expr with Body head)
;  Returns:
;     An Expr with LetRec head
(define (make-letrec location bindings body)
  (assert-head 'BindingsHead bindings 'make-letrec "bindings")
  (assert-head 'BodyHead body 'make-letrec "body")
  
  (make-expr 'LetRecHead (list bindings body) location)) 


; make-ifthen
;     Create an if expression node without else
;  Arguments:
;     location - Source location
;     test - The test expression (Expr)
;     then - The then expression (Expr)
;  Returns:
;     An Expr with If head
;  Note:
;     location of else expression is equivalent to the location of the whole if expression.
(define (make-ifthen location test then)
  (assert-expr test 'make-ifthen "test")
  (assert-expr then 'make-ifthen "then")
  
  (make-expr 'IfHead (list test then (make-voidnode location)) location))
  
; make-ifthenelse
;     Create an if expression node with else
;  Arguments:
;     location - Source location
;     test - The test expression (Expr)
;     then - The then expression (Expr)
;     else - The else expression (Expr)
;  Returns:
;     An Expr with If head
(define (make-ifthenelse location test then else)
  (assert-expr test 'make-ifthenelse "test")
  (assert-expr then 'make-ifthenelse "then")
  (assert-expr else 'make-ifthenelse "else")
  
  (make-expr 'IfHead (list test then else) location))

; make-cond-clause
;     Create a cond clause expression node
;  Arguments:
;     location - Source location
;     test - The test expression (Expr)
;     body - The body of the cond clause (list of Expr)
;  Returns:
;     An Expr with CondClause head
(define (make-cond-clause location test body)
  (assert-expr test 'make-cond-clause "test")
  (assert-list-of-exprs body 'make-cond-clause "body")
  (make-expr 'CondClauseHead (list test (make-begin location body)) location))

; make-cond-noelse
;     Create a cond expression node without else
;  Arguments:
;     location - Source location
;     clauses - The list of cond clauses (Expr with CondClause head)
;  Returns:
;     An Expr with Cond head
; Note:
;     - The else expression is set to a void node.
(define (make-cond-noelse location clauses)
  (unless (list? clauses)
    (error 'make-cond-noelse "Expected a list of cond clauses, got: ~a" clauses))
  (when (not (null? clauses))
    (map (lambda (clause) (assert-head 'CondClauseHead clause 'make-cond-noelse "clause")) clauses))
  
  (make-expr 'CondHead (list clauses (make-voidnode location)) location))

; make-cond-else
;     Create a cond expression node with else
;  Arguments:
;     location - Source location
;     clauses - The list of cond clauses (Expr with CondClause head)
;     else - The else expression (Expr)
;  Returns:
;     An Expr with Cond head
(define (make-cond-else location clauses else-exps)
  (unless (list? clauses)
    (error 'make-cond-else "Expected a list of cond clauses, got: ~a" clauses))
  (when (not (null? clauses))
    (for-each (lambda (clause) (assert-head 'CondClauseHead clause 'make-cond-else "clause")) clauses))
  
  (assert-expr else-exps 'make-cond-else "else")
  (make-expr 'CondHead (list clauses (make-begin (Expr-loc else-exps) else-exps)) location))


; make-and
;     Create an and expression node
;  Arguments:
;     location - Source location
;     args - The list of arguments (Expr)
;  Returns:
;     An Expr with And head
(define (make-and location args)
  (assert-list-of-exprs args 'make-and "arguments")
  
  (make-expr 'AndHead args location))

; make-or
;     Create an or expression node
;  Arguments:
;     location - Source location
;     args - The list of arguments (Expr)
;  Returns:
;     An Expr with Or head
(define (make-or location args)
  (assert-list-of-exprs args 'make-or "arguments")
  
  (make-expr 'OrHead args location))

; make-begin
;    Create a begin expression node
; Arguments:
;     location - Source location
;     args - The list of expressions to execute (Expr)
;  Returns:
;     An Expr with Begin head
(define (make-begin location args)
  (assert-list-of-exprs args 'make-begin "arguments")
  
  (make-expr 'BeginHead args location))

; make-do-let
;     Create a binding expression node for do
;  Arguments:
;     location - Source location
;     name - The name of the variable (string)
;     init - The initial value (Expr)
;     step - The step value (Expr)
;  Returns:
;     An Expr with DoLet head
(define (make-do-let location name init step)
  (assert-string name 'make-do-let "name")
  (assert-expr init 'make-do-let "initial value")
  (assert-expr step 'make-do-let "step value")
  
  (make-expr 'DoLetHead (list name init step) location))

; make-do-final
;     Create a final expression node for do
;  Arguments:
;     location - Source location
;     cond - The condition expression (Expr)
;     body - The body of the do (list of Expr)
;  Returns:
;     An Expr with DoFinal head
(define (make-do-final location cond body)
  (assert-expr cond 'make-do-final "condition")
  (assert-list-of-exprs body 'make-do-final "body")

  (make-expr 'DoFinalHead (list cond body) location))


; make-do
;     Create a do expression node
;  Arguments:
;     location - Source location
;     do-lets - The list of do-let bindings (Expr with DoLet head)
;     do-final - The final expression (Expr with DoFinal head)
;     body - The body of the do (Expr)
;  Returns:
;     An Expr with Do head
(define (make-do location do-lets do-final body)
  (unless (list? do-lets)
    (error 'make-do "Expected a list of do-let bindings, got: ~a" do-lets))
  (when (not (null? do-lets))
    (for-each (lambda (do-let) (assert-head 'DoLetHead do-let 'make-do "do-let binding")) do-lets))
  (assert-head 'DoFinalHead do-final 'make-do "do-final")
  (assert-expr body 'make-do "body")
  
  (make-expr 'DoHead (list do-lets do-final body) location))


; make-body
;     Create a body expression node
;  Arguments:
;     location - Source location
;     defines - The list of definitions (list of Expr with Define head)
;     body - The body of the expression (list of Expr)
;  Returns:
;     An Expr with Body head
(define (make-body location defines body)
  (unless (and (list? defines) 
               (andmap (lambda (define) 
                         (assert-head 'DefineHead define 'make-body "definition")) 
                       defines))
    (error 'make-body "Expected a list of definitions, got: ~a" defines))

  (assert-list-of-exprs body 'make-body "body")

  (make-expr 'BodyHead (list defines body) location))


; make-bind
;     Create a binding expression node
;  Arguments:
;     location - Source location
;     ident - The ident of the variable (Expr with Id head)
;     value - The value to bind (Expr)
;  Returns:
;     An Expr with Bind head
(define (make-bind location ident value)
  (assert-head 'IdHead ident 'make-bind "identifier")
  (assert-expr value 'make-bind "value")
  
  (make-expr 'BindHead (list ident value) location))  


; make-bindings
;     Create a bindings expression node
; Arguments:
;     location - Source location
;     bindings - The list of bindings (Expr with Bind head)
;  Returns:
;     An Expr with Bindings head
(define (make-bindings location bindings)
  (unless (list? bindings)
    (error 'make-bindings "Expected a list of bindings, got: ~a" bindings))
  (when (not (null? bindings))
    (for-each (lambda (bind) (assert-head 'BindHead bind 'make-bindings "binding")) bindings))
  
  (make-expr 'BindingsHead bindings location))


; make-load
;     Create a load expression node
;  Arguments:
;     location - Source location
;     filename - The name of the file to load (string)
;  Returns:
;     An Expr with Load head
(define (make-load location filename)
  (assert-string filename make-load "filename")
  
  (make-expr 'LoadHead (list filename) location))  


; make-sexpr
;     Create a s-expression node
;  Arguments:
;     location - Source location
;     args - The list of arguments (Expr)
;    tail - The tail of the s-expression (Expr)
;  Returns:
;     An Expr with S-Expr head
;  Example:
;     (a b c) -> (Expr 'S-ExprHead (list (list a b c) '()))
;     (a b c . d) -> (Expr 'S-ExprHead (list (list a b c) d))
(define (make-sexpr location args tail)
  (make-expr 'S-ExprHead (list args tail) location))