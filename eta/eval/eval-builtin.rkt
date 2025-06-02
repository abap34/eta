#lang racket

(require "../parser/ast.rkt"
         "runtime-values.rkt"
         "env.rkt"
         "../utils/error.rkt"
         "../utils/location.rkt"
         "../desugar/desugar.rkt"
         "interp.rkt")

(provide runtime-value->expr)

;  pair->list
;     Converts a RuntimeValue with PairTag to a list
;  Arguments:
;     pair-value - A RuntimeValue with PairTag or NilValueTag
;  Returns:
;     A list containing the values in the pair chain
(define (pair->list pair-value)
  (cond
    [(nil-value? pair-value) '()]
    [(not (pair-value? pair-value))
     (error "Internal error: expected a pair or nil, got: ~a" pair-value)]
    [else
     (let* ([pair (RuntimeValue-value pair-value)]
            [car-value (Pair-car pair)]
            [cdr-value (Pair-cdr pair)])
       (cons car-value (pair->list cdr-value)))]))

;  constant-value?
;     Checks if a RuntimeValue is a constant value
;  Arguments:
;     value - A RuntimeValue to check
;  Returns:
;     #t if the value is a constant (Int, Float, Boolean, String, Nil), #f otherwise
(define (constant-value? value)
  (or (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'IntTag))
      (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'FloatTag))
      (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'BooleanTag))
      (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'StringTag))
      (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'NilValueTag))))


(define (nil-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'NilValueTag)))
(define (symbol-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'SymbolTag)))
(define (pair-value? value)
    (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'PairTag)))


;  special-form?
;     Checks if a symbol name represents a special form
;  Arguments:
;     symbol-name - A symbol name to check
;  Returns:
;     #t if the symbol is a special form, #f otherwise
(define (special-form? symbol-name)
  (member symbol-name '(if lambda λ define set! quote begin cond and or let let* letrec)))

;  runtime-value->expr
;     Convert a RuntimeValue to an Expr structure
;  Arguments:
;     value - A RuntimeValue to convert
;     location - Source location to use for created expressions
;     visited - (Optional) List of already visited objects for cycle detection
;  Returns:
;     An Expr representing the runtime value
(define (runtime-value->expr value location [visited '()])
  ;; Check for cycles
  (when (member value visited eq?)
    (error "Circular reference detected in expression"))
  
  (let ([new-visited (cons value visited)])
    (cond
      ;; Integer constant
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'IntTag))
       (make-const location 'IntConstNode (RuntimeValue-value value))]
      
      ;; Float constant
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'FloatTag))
       (make-const location 'FloatConstNode (RuntimeValue-value value))]
      
      ;; Boolean constant
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'BooleanTag))
       (make-const location 'BoolConstNode (RuntimeValue-value value))]
      
      ;; String constant
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'StringTag))
       (make-const location 'StringConstNode (RuntimeValue-value value))]
      
      ;; Nil constant
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'NilValueTag))
       (make-const location 'NilConstNode '())]
      
      ;; Symbol
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'SymbolTag))
       (make-var location (RuntimeValue-value value))]
      
      ;; Pair (S-expression)
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'PairTag))
       (let* ([pair (RuntimeValue-value value)]
              [car-value (Pair-car pair)]
              [cdr-value (Pair-cdr pair)])
         ;; Check if this is a special form
         (if (and (symbol-value? car-value)
                  (special-form? (RuntimeValue-value car-value)))
             ;; Handle special forms
             (let ([symbol-name (RuntimeValue-value car-value)])
               (convert-special-form symbol-name cdr-value location new-visited))
             ;; Regular function application
             (let* ([func-expr (runtime-value->expr car-value location new-visited)]
                    [args-list (pair->list cdr-value)]
                    [arg-exprs (map (lambda (arg) (runtime-value->expr arg location new-visited)) args-list)])
               (make-app location func-expr arg-exprs))))]
      
      ;; Other types or error cases
      [else (error "Cannot convert value to expression: ~a" value)])))

;  convert-special-form
;     Convert a special form pair to the appropriate Expr structure
;  Arguments:
;     symbol-name - The name of the special form (if, lambda, etc.)
;     args - The arguments to the special form (as a RuntimeValue)
;     location - Source location to use for created expressions
;     visited - List of already visited objects for cycle detection
;  Returns:
;     An Expr representing the special form
(define (convert-special-form symbol-name args location visited)
  (let ([arg-list (pair->list args)])
    (cond
      ;; if special form
      [(equal? symbol-name 'if)
       (let ([arg-count (length arg-list)])
         (cond
           [(= arg-count 2) ;; (if test then)
            (let ([test-expr (runtime-value->expr (first arg-list) location visited)]
                  [then-expr (runtime-value->expr (second arg-list) location visited)])
              (make-ifthen location test-expr then-expr))]
           [(= arg-count 3) ;; (if test then else)
            (let ([test-expr (runtime-value->expr (first arg-list) location visited)]
                  [then-expr (runtime-value->expr (second arg-list) location visited)]
                  [else-expr (runtime-value->expr (third arg-list) location visited)])
              (make-ifthenelse location test-expr then-expr else-expr))]
           [else
            (error "Invalid if expression: expected 2 or 3 arguments, got ~a" arg-count)]))]
      
      ;; lambda special form
      [(or (equal? symbol-name 'lambda) (equal? symbol-name 'λ))
       (if (>= (length arg-list) 1)
           (let ([params (first arg-list)]
                 [body-exprs (rest arg-list)])
             (let ([param-expr (convert-params-to-arg-expr params location visited)]
                   [body-expr (convert-body-exprs body-exprs location visited)])
               (make-lambda location param-expr body-expr)))
           (error "Invalid lambda expression: expected at least 1 argument"))]
      
      ;; define special form
      [(equal? symbol-name 'define)
       (if (>= (length arg-list) 2)
           (let ([name (first arg-list)]
                 [value (second arg-list)])
             (if (symbol-value? name)
                 (let ([name-expr (runtime-value->expr name location visited)]
                       [value-expr (runtime-value->expr value location visited)])
                   (make-define location name-expr value-expr))
                 (error "First argument to define must be a symbol")))
           (error "Invalid define expression: expected at least 2 arguments"))]
      
      ;; set! special form
      [(equal? symbol-name 'set!)
       (if (= (length arg-list) 2)
           (let ([name (first arg-list)]
                 [value (second arg-list)])
             (if (symbol-value? name)
                 (let ([name-expr (runtime-value->expr name location visited)]
                       [value-expr (runtime-value->expr value location visited)])
                   (make-setbang location name-expr value-expr))
                 (error "First argument to set! must be a symbol")))
           (error "Invalid set! expression: expected 2 arguments"))]
      
      ;; quote special form
      [(equal? symbol-name 'quote)
       (if (= (length arg-list) 1)
           (let ([quoted-value (first arg-list)])
             (make-quote location (convert-to-sexpr quoted-value location visited)))
           (error "Invalid quote expression: expected 1 argument"))]
      
      ;; begin special form
      [(equal? symbol-name 'begin)
       (let ([body-exprs (map (lambda (expr) (runtime-value->expr expr location visited)) arg-list)])
         (make-begin location body-exprs))]
      
      ;; and special form
      [(equal? symbol-name 'and)
       (let ([and-exprs (map (lambda (expr) (runtime-value->expr expr location visited)) arg-list)])
         (make-and location and-exprs))]
      
      ;; or special form
      [(equal? symbol-name 'or)
       (let ([or-exprs (map (lambda (expr) (runtime-value->expr expr location visited)) arg-list)])
         (make-or location or-exprs))]
      
      ;; Default case for other special forms (not implemented yet)
      [else (error "Special form not yet implemented: ~a" symbol-name)])))

;  convert-params-to-arg-expr
;     Convert a parameter specification to an Arg expression
;  Arguments:
;     params - A RuntimeValue representing parameter list
;     location - Source location
;     visited - List of already visited objects for cycle detection
;  Returns:
;     An Expr with ArgHead
(define (convert-params-to-arg-expr params location visited)
  (cond
    ;; Symbol parameter (lambda x ...)
    [(symbol-value? params)
     (make-single-arg location (symbol->string (RuntimeValue-value params)))]
    
    ;; List of parameters (lambda (x y z) ...)
    [(or (pair-value? params) (nil-value? params))
     (let ([param-list (pair->list params)])
       ;; Process parameter list and detect dotted params
       (let process-params ([remaining param-list]
                           [processed '()]
                           [found-dotted? #f])
         (cond
           [(null? remaining)
            (make-list-arg location (reverse processed) #f)]
           [(and (= (length remaining) 1) (symbol-value? (car remaining)) found-dotted?)
            (make-list-arg location (reverse processed) (symbol->string (RuntimeValue-value (car remaining))))]
           [(symbol-value? (car remaining))
            (process-params (cdr remaining)
                           (cons (symbol->string (RuntimeValue-value (car remaining))) processed)
                           found-dotted?)]
           [else
            (error "Invalid parameter specification")])))]
    
    ;; Invalid parameter
    [else
     (error "Invalid parameter specification")]))

;  convert-body-exprs
;     Convert a list of body expressions to a Body expression
;  Arguments:
;     body-exprs - A list of RuntimeValues for body
;     location - Source location
;     visited - List of already visited objects for cycle detection
;  Returns:
;     An Expr with BodyHead
(define (convert-body-exprs body-exprs location visited)
  ;; Separate defines from other expressions using a functional approach
  (let-values ([(define-exprs other-exprs) 
                (partition (lambda (expr) 
                            (and (pair-value? expr)
                                 (let ([pair (RuntimeValue-value expr)])
                                   (and (symbol-value? (Pair-car pair))
                                        (equal? (RuntimeValue-value (Pair-car pair)) 'define)))))
                          body-exprs)])
    ;; Convert all expressions
    (let ([define-ast-exprs (map (lambda (expr) (runtime-value->expr expr location visited)) define-exprs)]
          [other-ast-exprs (map (lambda (expr) (runtime-value->expr expr location visited)) other-exprs)])
      (make-body location define-ast-exprs other-ast-exprs))))

;  convert-to-sexpr
;     Convert a RuntimeValue to an S-expression Expr
;  Arguments:
;     value - A RuntimeValue to convert
;     location - Source location
;     visited - List of already visited objects for cycle detection
;  Returns:
;     An Expr suitable for a quoted expression
(define (convert-to-sexpr value location visited)
  (cond
    ;; Simple types (integers, strings, etc.)
    [(or (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'IntTag))
         (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'FloatTag))
         (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'BooleanTag))
         (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'StringTag))
         (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'NilValueTag)))
     (runtime-value->expr value location visited)]
    
    ;; Symbol
    [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'SymbolTag))
     (make-var location (symbol->string (RuntimeValue-value value)))]
    
    ;; Pair (for lists and dotted pairs)
    [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'PairTag))
     ;; Extract the proper list and potential dotted tail using a helper function
     (let-values ([(items tail) (extract-list-and-tail value '())])
       ;; Convert all items to Expr
       (let ([item-exprs (map (lambda (item) (convert-to-sexpr item location visited)) items)]
             [tail-expr (if (null? tail) 
                            '() 
                            (convert-to-sexpr tail location visited))])
         (make-sexpr location item-exprs tail-expr)))]
    
    ;; Other types or error cases
    [else
     (error "Cannot convert value to s-expression: ~a" value)]))

;  extract-list-and-tail
;     Helper function to extract a proper list and its potential dotted tail
;  Arguments:
;     current - The current pair being processed
;     acc - Accumulated items in the list so far
;  Returns:
;     Two values: the list of items and the tail (if dotted pair)
(define (extract-list-and-tail current acc)
  (cond
    [(nil-value? current)
     (values (reverse acc) '())]
    [(pair-value? current)
     (let* ([pair (RuntimeValue-value current)]
            [car-val (Pair-car pair)]
            [cdr-val (Pair-cdr pair)])
       (if (or (nil-value? cdr-val) (pair-value? cdr-val))
           (extract-list-and-tail cdr-val (cons car-val acc))
           (values (reverse (cons car-val acc)) cdr-val)))]
    [else
     (error "Invalid value in s-expression")]))
