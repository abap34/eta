#lang racket

(require "../parser/ast.rkt"
         "../utils/error.rkt"
         "../parser/parser.rkt")

(provide desugar)

;; Helper function to generate unique variable names
(define gen-counter 0)

;  gen-unique-var
;     Generates a unique variable name for use in macro expansion.
;  Arguments:
;     loc - Location information for the generated variable
;     [prefix] - Optional prefix for the variable name (default: "genvar")
;  Returns:
;     An Expr with IdHead representing a unique variable
;  Notes:
;     Uses a counter to ensure uniqueness
(define (gen-unique-var loc [prefix "genvar"])
  (set! gen-counter (add1 gen-counter))
  (make-var loc (format "~a-~a" prefix gen-counter)))

;  get-bind-name
;     Extracts the name from a binding expression.
;  Arguments:
;     bind - An Expr with BindHead
;  Returns:
;     The name string of the binding
(define (get-bind-name bind)
  (let ([name-expr (first (Expr-args bind))])
    (first (Expr-args name-expr))))

;  get-bind-value
;     Extracts and desugars the value from a binding expression.
;  Arguments:
;     bind - An Expr with BindHead
;  Returns:
;     The desugared value of the binding
(define (get-bind-value bind)
  (desugar (second (Expr-args bind))))

;  desugar-app
;     Desugars an application expression.
;  Arguments:
;     expr - An Expr with AppHead
;  Returns:
;     A desugared application expression
(define (desugar-app expr)
  (let ([operator (first (Expr-args expr))]
        [args (second (Expr-args expr))]
        [loc (Expr-loc expr)])
    (make-app loc
              (desugar operator)
              (map desugar args))))

;  desugar-lambda
;     Desugars a lambda expression.
;  Arguments:
;     expr - An Expr with LambdaHead
;  Returns:
;     A desugared lambda expression
(define (desugar-lambda expr)
  (let ([args (first (Expr-args expr))]
        [body (second (Expr-args expr))]
        [loc (Expr-loc expr)])
    (make-lambda loc args (desugar body))))

;  desugar-define
;     Desugars a define expression.
;  Arguments:
;     expr - An Expr with DefineHead
;  Returns:
;     A desugared define expression
(define (desugar-define expr)
  (let ([name (first (Expr-args expr))]
        [value (second (Expr-args expr))]
        [loc (Expr-loc expr)])
    (make-define loc name (desugar value))))

;  desugar-if
;     Desugars an if expression.
;  Arguments:
;     expr - An Expr with IfHead
;  Returns:
;     A desugared if expression
(define (desugar-if expr)
  expr)

;  desugar-cond
;     Desugars a cond expression into nested if expressions.
;  Arguments:
;     expr - An Expr with CondHead
;  Returns:
;     A desugared expression with nested if expressions
(define (desugar-cond expr)
  (let ([clauses (first (Expr-args expr))]
        [else-expr (second (Expr-args expr))]
        [loc (Expr-loc expr)])
    (if (null? clauses)
        ; If there are no clauses, return the else expression
        (desugar else-expr)
        (desugar-cond-clauses clauses else-expr loc))))

;  desugar-cond-clauses
;     Helper function to desugar cond clauses into nested ifs.
;  Arguments:
;     clauses - List of cond clauses
;     else-expr - The else expression
;     loc - Location info
;  Returns:
;     A desugared expression with nested if expressions
(define (desugar-cond-clauses clauses else-expr loc)
  (if (null? clauses)
      (desugar else-expr)
      (let* ([first-clause (first clauses)]
             [rest-clauses (rest clauses)]
             [test (first (Expr-args first-clause))]
             [body (second (Expr-args first-clause))])
        (make-ifthenelse loc
                         (desugar test)
                         (desugar body)
                         (desugar-cond-clauses rest-clauses else-expr loc)))))

;  desugar-cond-clause
;     Desugars a single cond clause.
;  Arguments:
;     expr - An Expr with CondClauseHead
;  Returns:
;     A desugared cond clause
(define (desugar-cond-clause expr)
  (let ([test (first (Expr-args expr))]
        [body (second (Expr-args expr))]
        [loc (Expr-loc expr)])
    (make-cond-clause loc (desugar test) (desugar body))))

;  desugar-begin
;     Desugars a begin expression.
;  Arguments:
;     expr - An Expr with BeginHead
;  Returns:
;     A desugared begin expression
(define (desugar-begin expr)
  (let ([exprs (Expr-args expr)]
        [loc (Expr-loc expr)])
    (make-begin loc (map desugar exprs))))

;  desugar-unnamed-let
;     Desugars an unnamed let expression into a lambda application.
;  Arguments:
;     expr - An Expr with UnNamedLetHead
;  Returns:
;     A desugared lambda application
(define (desugar-unnamed-let expr)
  (let ([bindings (first (Expr-args expr))]
        [body (second (Expr-args expr))]
        [loc (Expr-loc expr)])
    (let ([bind-list (Expr-args bindings)])
      (desugar-let-to-lambda bind-list body loc))))

;  desugar-let-to-lambda
;     Helper function to convert a let to lambda application.
;  Arguments:
;     bind-list - List of bindings
;     body - Body expression
;     loc - Location information
;  Returns:
;     A lambda application expression
(define (desugar-let-to-lambda bind-list body loc)
  (let ([names (map get-bind-name bind-list)]
        [values (map get-bind-value bind-list)])
    (make-app loc
              (make-lambda loc
                           (make-list-arg loc names '())
                           (desugar body))
              values)))

;  desugar-named-let
;     Desugars a named let expression into a letrec with function application.
;  Arguments:
;     expr - An Expr with NamedLetHead
;  Returns:
;     A desugared letrec with function application
(define (desugar-named-let expr)
  (let ([name (first (Expr-args expr))]
        [bindings (second (Expr-args expr))]
        [body (third (Expr-args expr))]
        [loc (Expr-loc expr)])
    (let ([bind-list (Expr-args bindings)])
      (desugar-named-let-to-letrec name bind-list body loc))))

;  desugar-named-let-to-letrec
;     Helper function to convert a named let to letrec with function application.
;  Arguments:
;     name - Function name
;     bind-list - List of bindings
;     body - Body expression
;     loc - Location information
;  Returns:
;     A letrec with function application
(define (desugar-named-let-to-letrec name bind-list body loc)
  (let ([param-names (map get-bind-name bind-list)]
        [arg-values (map get-bind-value bind-list)])
    (let ([func (make-lambda loc
                             (make-list-arg loc param-names '())
                             (desugar body))])
      (desugar (make-letrec loc
                            (make-bindings loc (list (make-bind loc name func)))
                            (make-app loc name arg-values))))))

;  desugar-letrec
;     Desugars a letrec expression into a let with set! expressions.
;  Arguments:
;     expr - An Expr with LetRecHead
;  Returns:
;     A desugared let with set! expressions
(define (desugar-letrec expr)
  (let ([bindings (first (Expr-args expr))]
        [body (second (Expr-args expr))]
        [loc (Expr-loc expr)])
    (let ([bind-list (Expr-args bindings)])
      (desugar-letrec-to-let-set bind-list body loc))))

;  desugar-letrec-to-let-set
;     Helper function to convert a letrec to let with set! expressions.
;  Arguments:
;     bind-list - List of bindings
;     body - Body expression
;     loc - Location information
;  Returns:
;     A let with set! expressions
(define (desugar-letrec-to-let-set bind-list body loc)
  (let ([undefined-binds (make-undefined-bindings bind-list loc)]
        [set-exprs (make-set-expressions bind-list loc)])
    (desugar (make-unnamed-let loc
                               (make-bindings loc undefined-binds)
                               (make-begin loc
                                          (append set-exprs (list body)))))))

;  make-undefined-bindings
;     Creates bindings initialized to undefined.
;  Arguments:
;     bind-list - List of bindings
;     loc - Location information
;  Returns:
;     List of bindings initialized to undefined
(define (make-undefined-bindings bind-list loc)
  (map (lambda (bind)
         (let ([name (first (Expr-args bind))])
           (make-bind loc 
                      name
                      (make-const loc 'UndefinedConstNode '()))))
       bind-list))

;  make-set-expressions
;     Creates set! expressions for bindings.
;  Arguments:
;     bind-list - List of bindings
;     loc - Location information
;  Returns:
;     List of set! expressions
(define (make-set-expressions bind-list loc)
  (map (lambda (bind)
         (let ([name (first (Expr-args bind))]
               [value (second (Expr-args bind))])
           (make-setbang loc 
                         (first (Expr-args name))
                         (desugar value))))
       bind-list))

;  desugar-letstar
;     Desugars a let* expression into nested lets.
;  Arguments:
;     expr - An Expr with LetStarHead
;  Returns:
;     Desugared nested let expressions
(define (desugar-letstar expr)
  (let ([bindings (first (Expr-args expr))]
        [body (second (Expr-args expr))]
        [loc (Expr-loc expr)])
    (let ([bind-list (Expr-args bindings)])
      (desugar-letstar-to-nested-lets bind-list body loc))))

;  desugar-letstar-to-nested-lets
;     Helper function to convert a let* to nested lets.
;  Arguments:
;     bind-list - List of bindings
;     body - Body expression
;     loc - Location information
;  Returns:
;     Nested let expressions
(define (desugar-letstar-to-nested-lets bind-list body loc)
  (if (null? bind-list)
      (desugar body)
      (desugar (make-unnamed-let 
                    loc                
                    (make-bindings loc (list (first bind-list)))
                    (if (null? (rest bind-list))
                        body
                        (make-letstar loc
                                    (make-bindings loc (rest bind-list))
                                    body))))))

;  desugar-and
;     Desugars an and expression into nested if expressions.
;  Arguments:
;     expr - An Expr with AndHead
;  Returns:
;     Desugared nested if expressions
(define (desugar-and expr)
  (let ([args (Expr-args expr)]
        [loc (Expr-loc expr)])
    (cond
      [(null? args) (make-const loc 'BoolConstNode #t)]
      [(null? (rest args)) (desugar (first args))]
      [else (desugar-and-expressions args loc)])))

;  desugar-and-expressions
;     Helper function to convert and expressions to nested ifs.
;  Arguments:
;     args - List of expressions to AND
;     loc - Location information
;  Returns:
;     Nested if expressions
(define (desugar-and-expressions args loc)
  (make-ifthenelse loc
    (desugar (first args))
    (if (null? (rest (rest args)))
        (desugar (second args))
        (desugar-and-expressions (rest args) loc))
    (make-const loc 'BoolConstNode #f)))

;  desugar-or
;     Desugars an or expression into nested if expressions.
;  Arguments:
;     expr - An Expr with OrHead
;  Returns:
;     Desugared nested if expressions with let bindings
(define (desugar-or expr)
  (let ([args (Expr-args expr)]
        [loc (Expr-loc expr)])
    (cond
      [(null? args) (make-const loc 'BoolConstNode #f)]
      [(null? (rest args)) (desugar (first args))]
      [else (desugar-or-expressions args loc)])))

;  desugar-or-expressions
;     Helper function to convert or expressions to nested ifs with lets.
;  Arguments:
;     args - List of expressions to OR
;     loc - Location information
;  Returns:
;     Nested let-if expressions to handle short-circuit evaluation
(define (desugar-or-expressions args loc)
  (let ([temp-var (gen-unique-var loc "or")])
    (desugar (make-unnamed-let loc
                (make-bindings loc 
                    (list (make-bind loc 
                                temp-var 
                                (first args))))
                (make-ifthenelse loc
                    temp-var
                    temp-var
                    (if (null? (rest (rest args)))
                        (second args)
                        (make-or loc (rest args))))))))

;  desugar-set
;     Desugars a set! expression.
;  Arguments:
;     expr - An Expr with SetHead
;  Returns:
;     A desugared set! expression
(define (desugar-set expr)
  (let ([name (first (Expr-args expr))]
        [value (second (Expr-args expr))]
        [loc (Expr-loc expr)])
    (make-setbang loc name (desugar value))))

;  desugar-body
;     Desugars a body expression.
;  Arguments:
;     expr - An Expr with BodyHead
;  Returns:
;     A desugared body expression
(define (desugar-body expr)
  (let ([defines (first (Expr-args expr))]
        [exprs (second (Expr-args expr))]
        [loc (Expr-loc expr)])
    (make-body loc
        (map desugar defines)
        (map desugar exprs))))

;  desugar-bind
;     Desugars a bind expression.
;  Arguments:
;     expr - An Expr with BindHead
;  Returns:
;     A desugared bind expression
(define (desugar-bind expr)
  (let ([name (first (Expr-args expr))]
        [value (second (Expr-args expr))]
        [loc (Expr-loc expr)])
    (make-bind loc name (desugar value))))

;  desugar-bindings
;     Desugars a bindings expression.
;  Arguments:
;     expr - An Expr with BindingsHead
;  Returns:
;     A desugared bindings expression
(define (desugar-bindings expr)
  (let ([bindings (Expr-args expr)]
        [loc (Expr-loc expr)])
    (make-bindings loc (map desugar bindings))))

;  desugar
;     Main entry point for desugaring.
;  Arguments:
;     expr - The expression to desugar (Expr)
;  Returns:
;     Desugared expression (Expr)
;  Example:
;     (desugar (and a b)) ; => (if a (if b #t #f) #f)
;  Notes:
;     `devdocs/desugar.md` contains the desugaring rules.
(define (desugar expr)
   (if (list? expr)
     (map desugar expr)
      (begin
        (unless (Expr? expr)
          (error (format "desugar: expected Expr, got ~a" expr)))
        (let ([head (Expr-head expr)])
          (cond
            [(equal? head 'ConstHead)       expr]    
            [(equal? head 'IdHead)          expr]
            [(equal? head 'AppHead)         (desugar-app expr)]
            [(equal? head 'LambdaHead)      (desugar-lambda expr)]
            [(equal? head 'QuoteHead)       expr]
            [(equal? head 'DefineHead)      (desugar-define expr)]
            [(equal? head 'IfHead)          (desugar-if expr)]
            [(equal? head 'CondHead)        (desugar-cond expr)]
            [(equal? head 'CondClauseHead)  (desugar-cond-clause expr)]
            [(equal? head 'BeginHead)       (desugar-begin expr)]
            [(equal? head 'UnNamedLetHead)  (desugar-unnamed-let expr)]
            [(equal? head 'NamedLetHead)    (desugar-named-let expr)]
            [(equal? head 'LetRecHead)      (desugar-letrec expr)]
            [(equal? head 'LetStarHead)     (desugar-letstar expr)]
            [(equal? head 'AndHead)         (desugar-and expr)]
            [(equal? head 'OrHead)          (desugar-or expr)]
            [(equal? head 'LoadHead)        expr]
            [(equal? head 'SetHead)         (desugar-set expr)]
            [(equal? head 'DoHead)          (error "Do expressions not yet implemented")]
            [(equal? head 'DoLetHead)       (error "DoLet expressions not yet implemented")]
            [(equal? head 'DoFinalHead)     (error "DoFinal expressions not yet implemented")]
            [(equal? head 'BodyHead)        (desugar-body expr)]
            [(equal? head 'BindHead)        (desugar-bind expr)]
            [(equal? head 'BindingsHead)    (desugar-bindings expr)]
            [(equal? head 'ArgHead)         expr]
            [(equal? head 'S-ExprHead)      expr]
            [else (error (format "desugar: unknown expression type ~a" head))])))))