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
         make-nil
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
)

; ExprHead
;    Represents the head of an expression in the AST.
; Note:
;    - Some of these head may be deprecated in the future by macros in stdlib.
(define (ExprHead? head)
  (or (equal? head 'ConstHead)       ; Constant value
      (equal? head 'IdHead)         ; Variable
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
      (equal? head 'NilHead)         ; Nil value
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
    [(equal? head 'ConstHead) "Const"]
    [(equal? head 'IdHead) "Var"]
    [(equal? head 'AppHead) "App"]
    [(equal? head 'LambdaHead) "Lambda"]
    [(equal? head 'QuoteHead) "Quote"]
    [(equal? head 'DefineHead) "Define"]
    [(equal? head 'IfHead) "If"]
    [(equal? head 'BeginHead) "Begin"]
    [(equal? head 'UnNamedLetHead) "UnNamedLet"]
    [(equal? head 'NamedLetHead) "NamedLet"]
    [(equal? head 'LetRecHead) "LetRec"]
    [(equal? head 'LetStarHead) "LetStar"]
    [(equal? head 'CondHead) "Cond"]
    [(equal? head 'CondClauseHead) "CondClause"]
    [(equal? head 'AndHead) "And"]
    [(equal? head 'OrHead) "Or"]
    [(equal? head 'LoadHead) "Load"]
    [(equal? head 'SetHead) "Set!"]
    [(equal? head 'DoHead) "Do"]
    [(equal? head 'DoLetHead) "DoLet"]
    [(equal? head 'DoFinalHead) "DoFinal"]
    [(equal? head 'NilHead) "Nil"]
    [(equal? head 'BodyHead) "Body"]
    [(equal? head 'BindHead) "Bind"]
    [(equal? head 'BindingsHead) "Bindings"]
    [(equal? head 'ArgHead) "Arg"]
    [(equal? head 'S-ExprHead) "S-Expr"]
    [else (error 'ExprHead->name "Unknown ExprHead: ~a" head)]))

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


;; -- ast builders 

;  make-const
;     Create a constant expression node
;  Arguments:
;     location - Source location
;     tag - The type of the constant (e.g., number, string, boolean)
;     value - The constant value (e.g., 42, "hello", #t)
;  Returns:
;     An Expr with Const head
(define (make-const location tag value)
  (make-expr 'ConstHead (list tag value) location))  ; TODO: validate tag

;  make-symbol
;     Create a symbol (variable) expression node
;  Arguments:
;     location - Source location
;     name - The symbol name (string)
;  Returns:
;     An Expr with Var head
(define (make-var location name)
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
  (make-expr 'AppHead (list op args) location))  

; make-lambda
;    Create a lambda expression node
;  Arguments:
;     location - Source location
;     args - The arguments to the lambda (Expr with Arg head)
;     body - The body of the lambda (Expr)
;  Returns:
;     An Expr with Lambda head
(define (make-lambda location args body)
  (make-expr 'LambdaHead (list args body) location))


; make-define
;     Create a define expression node
; Arguments:
;     location - Source location
;     name - The name of the variable or function (string)
;     value - The value to assign (Expr. If function, head is Lambda)
;  Returns:
;     An Expr with Define head
(define (make-define location name value)
    (make-expr 'DefineHead (list name value) location))

; make-nil
;     Create a nil expression node
;  Arguments:
;     location - Source location
;  Returns:
;     An Expr with Nil head
(define (make-nil location)
  (make-expr 'NilHead '() location))  ; Nil has no arguments


; make-single-arg
;     Create an argument expression node
;  Arguments:
;     location - Source location
;     name - The argument name (string)
;  Returns:
;     An Expr with Arg head
(define (make-single-arg location name)
      (unless (string? name)
        (error (format "Expected a string for argument name, got: ~a" name) location))

      (make-expr 'ArgHead 
          (list
            '() ; no required args
           name ; single variadic arg
          )   ;
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
  (make-expr 'QuoteHead (list value) location))  ; Quote has one argument


; make-setbang
;     Create a set! expression node
;  Arguments:
;     location - Source location
;     name - The variable name to set (string)
;     value - The value to assign (Expr)
;  Returns:
;     An Expr with Set! head
(define (make-setbang location name value)
  (make-expr 'SetHead (list name value) location))


; make-unnamed-let
;     Create a let (**not named**) expression node
;  Arguments:
;     location - Source location
;     bindings - binings (Expr with Bindings head)
;     body - The body of the let (Expr)
;  Returns:
;     An Expr with UnNamedLet head
(define (make-unnamed-let location bindings body)
  (make-expr 'UnNamedLetHead (list bindings body) location))  

; make-named-let
;     Create a named let expression node
;  Arguments:
;     location - Source location
;     name - The name of the let (string)
;     bindings - The bindings (Expr with Bindings head)
;     body - The body of the let (Expr)
;  Returns:
;     An Expr with NamedLet head
(define (make-named-let location name bindings body)
  (make-expr 'NamedLetHead (list name bindings body) location)) 

; make-letstar
;     Create a let* expression node
;  Arguments:
;     location - Source location
;     bindings - The bindings (Expr with Bindings head)
;     body - The body of the let* (Expr)
;  Returns:
;     An Expr with LetStar head
(define (make-letstar location bindings body)
  (make-expr 'LetStarHead (list bindings body) location)) 

; make-letrec
;     Create a letrec expression node
;  Arguments:
;     location - Source location
;     bindings - The bindings (Expr with Bindings head)
;     body - The body of the letrec (Expr)
;  Returns:
;     An Expr with LetRec head
(define (make-letrec location bindings body)
  (make-expr 'LetRecHead (list bindings body) location)) 


; make-ifthen
;     Create an if expression node without else
;  Arguments:
;     location - Source location
;     test - The test expression (Expr)
;     then - The then expression (Expr)
;  Returns:
;     An Expr with If head
(define (make-ifthen location test then)
  (make-expr 'IfHead (list #f test then) location))
  ;                        ^^^ no else

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
  (make-expr 'IfHead (list #t test then else) location))
;                          ^^^ has else

; make-cond-clause
;     Create a cond clause expression node
;  Arguments:
;     location - Source location
;     test - The test expression (Expr)
;     body - The body of the cond clause (Expr)
;  Returns:
;     An Expr with CondClause head
(define (make-cond-clause location test body)
  (make-expr 'CondClauseHead (list test body) location))

; make-cond-noelse
;     Create a cond expression node without else
;  Arguments:
;     location - Source location
;     clauses - The list of cond clauses (Expr with CondClause head)
;  Returns:
;     An Expr with Cond head
(define (make-cond-noelse location clauses)
  (make-expr 'CondHead (list #f clauses) location))
;                            ^^^ no else

; make-cond-else
;     Create a cond expression node with else
;  Arguments:
;     location - Source location
;     clauses - The list of cond clauses (Expr with CondClause head)
;     else - The else expression (Expr)
;  Returns:
;     An Expr with Cond head
(define (make-cond-else location clauses else)
  (make-expr 'CondHead (list #t clauses else) location))
;                            ^^^ has else


; make-and
;     Create an and expression node
;  Arguments:
;     location - Source location
;     args - The list of arguments (Expr)
;  Returns:
;     An Expr with And head
(define (make-and location args)
  (make-expr 'AndHead args location))

; make-or
;     Create an or expression node
;  Arguments:
;     location - Source location
;     args - The list of arguments (Expr)
;  Returns:
;     An Expr with Or head
(define (make-or location args)
  (make-expr 'OrHead args location))

; make-begin
;    Create a begin expression node
; Arguments:
;     location - Source location
;     args - The list of expressions to execute (Expr)
;  Returns:
;     An Expr with Begin head
(define (make-begin location args)
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
  (make-expr 'DoLetHead (list name init step) location))

; make-do-final
;     Create a final expression node for do
;  Arguments:
;     location - Source location
;     cond - The condition expression (Expr)
;     body - The body of the do (Expr)
;  Returns:
;     An Expr with DoFinal head
(define (make-do-final location cond body)
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
  (make-expr 'DoHead (list do-lets do-final body) location))


; make-body
;     Create a body expression node
;  Arguments:
;     location - Source location
;     defines - The list of definitions (Expr with Define head)
;     body - The body of the expression (Expr)
;  Returns:
;     An Expr with Body head
(define (make-body location defines body)
  (make-expr 'BodyHead (list defines body) location))


; make-bind
;     Create a binding expression node
;  Arguments:
;     location - Source location
;     name - The name of the variable (string)
;     value - The value to bind (Expr)
;  Returns:
;     An Expr with Bind head
(define (make-bind location name value)
  (make-expr 'BindHead (list name value) location))  


; make-bindings
;     Create a bindings expression node
; Arguments:
;     location - Source location
;     bindings - The list of bindings (Expr with Bind head)
;  Returns:
;     An Expr with Bindings head
(define (make-bindings location bindings)
  (make-expr 'BindingsHead bindings location))


; make-load
;     Create a load expression node
;  Arguments:
;     location - Source location
;     filename - The name of the file to load (string)
;  Returns:
;     An Expr with Load head
(define (make-load location filename)
  (make-expr 'LoadHead (list filename) location))  


; make-sexpr
;     Create a s-expression node
;  Arguments:
;     location - Source location
;     args - The list of arguments (Expr)
;  Returns:
;     An Expr with S-Expr head
(define (make-sexpr location args)
  (make-expr 'S-ExprHead args location))