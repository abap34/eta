#lang racket

(require "../parser/tokenizer.rkt"
         "../desugar/desugar.rkt"
         "../parser/ast.rkt"
         "../parser/parser.rkt"
         "../utils/error.rkt"
         "runtime-values.rkt"
         "env.rkt"
         "stack-frame.rkt"
         )


(provide eval-expr 
         eval-toplevel-exprs
         collect-stack-trace-for-error
 )


; eval-toplevel-exprs
;    Main entry point for evaluating expressions in the interpreter.
; Arguments:
;    expr-list - A list of expressions to evaluate
;    env - The environment in which to evaluate
; Returns:
;    A list of evaluated expressions or a RuntimeError
; Example:
;   (eval-toplevel-exprs (list (make-const 'Num 1) (make-const 'Num 2) (make-const 'Num 3)) env)
;   => (1 2 3)
(define (eval-toplevel-exprs expr-list env)
  (eval-toplevel-exprs-cps expr-list env
    (lambda (result stack) result)
    (init-call-stack)))

;  eval-toplevel-exprs-cps
;     Evaluate a list of expressions in the given environment.
;     Returns the list of each evaluated expression in given order or return **first** error.
;  Arguments:
;     expr-list - A list of expressions to evaluate
;     env - The environment in which to evaluate
;     k - The continuation to receive the final result
;     stack - The current call stack
;  Returns:
;     Via k: A list of evaluated expressions or a RuntimeError
;  Example:
;   (eval-toplevel-exprs-cps (list (make-const 'Num 1) (make-const 'Num 2) (make-const 'Num 3)) env k stack)
;   => via k: (1 2 3)
(define (eval-toplevel-exprs-cps expr-list env k stack)
  (unless (and (list? expr-list)
               (andmap Expr? expr-list))
    (k (make-runtime-error (format "Internal error: expected a list of Expr, got: ~a" expr-list)) stack))
  
  (let loop ([exprs expr-list]
             [result '()]
             [current-stack stack])
    (if (null? exprs)
        (k (reverse result) current-stack)
        (eval-expr 
         (first exprs) 
         env
         (lambda (new-result new-stack)
           (if (RuntimeError? new-result)
               (k (collect-stack-trace-for-error new-result new-stack) new-stack)
               (loop (rest exprs)
                     (cons new-result result)
                     new-stack)))
         current-stack))))


;  eval-expr
;     Evaluate an expression based on its head type
;  Arguments:
;     expr - The **desugared** expression to evaluate
;     env - The environment in which to evaluate
;     k - The continuation to receive the result
;     stack - The current call stack
;     tail? - Whether this expression is in tail position (#t) or not (#f)
;  Returns:
;     Via k: An RuntimeValue representing the result
(define (eval-expr expr env k stack #:tail? [tail? #f])
  (unless (Expr? expr)
    (k (collect-stack-trace-for-error
        (make-runtime-error 
        (format "Internal error: expr must be an Expr, got: ~a" expr))
        stack)
       stack))
  
  (let ([head (Expr-head expr)])
    (cond
      [(equal? head 'ConstHead)  (eval-const expr env k stack)]
      [(equal? head 'IdHead)     (eval-var expr env k stack)]
      [(equal? head 'AppHead)    (eval-app expr env k stack #:tail? tail?)]
      [(equal? head 'LambdaHead) (eval-lambda expr env k stack)]
      [(equal? head 'QuoteHead)  (eval-quote expr env k stack)]
      [(equal? head 'DefineHead) (eval-define expr env k stack)]
      [(equal? head 'IfHead)     (eval-if expr env k stack #:tail? tail?)]
      [(equal? head 'SetHead)    (eval-set! expr env k stack)]
      [(equal? head 'BodyHead)   (eval-body expr env k stack #:tail? tail?)]
      [(equal? head 'CallCCHead) (eval-call/cc expr env k stack #:tail? tail?)]
      [(equal? head 'LoadHead)   (eval-load expr env k stack #:tail? tail?)]
      [else (k (make-runtime-error 
                (format "Internal error: unexpected expression ~a with head ~a" 
                        (pretty-print-Expr expr) head))
               stack)])))

;  eval-const
;     Evaluate a constant expression
;  Arguments:
;     expr - A Const expression with tag and value
;     env - The environment (unused for constants)
;     k - The continuation to receive the result
;     stack - The current call stack
;  Returns:
;     Via k: An RuntimeValue with appropriate tag and value
(define (eval-const expr env k stack)
  (let* ([const-args (Expr-args expr)]
         [node-typ (first const-args)]
         [value (second const-args)])
    (cond
      [(equal? node-typ 'IntConstNode)       (k (RuntimeValue 'IntTag value) stack)]
      [(equal? node-typ 'FloatConstNode)     (k (RuntimeValue 'FloatTag value) stack)]
      [(equal? node-typ 'BoolConstNode)      (k (RuntimeValue 'BooleanTag value) stack)]
      [(equal? node-typ 'StringConstNode)    (k (RuntimeValue 'StringTag value) stack)]
      [(equal? node-typ 'NilConstNode)       (k (RuntimeValue 'NilValueTag '()) stack)]
      [(equal? node-typ 'VoidConstNode)      (k (RuntimeValue 'VoidTag '()) stack)]
      [(equal? node-typ 'UndefinedConstNode) (k (RuntimeValue 'UndefinedTag 'undefined) stack)]
      [else (k (make-runtime-error 
                (format "Internal error: unexpected ConstNode type: ~a" node-typ)) stack)])))

;  eval-var
;     Evaluate a variable expression by looking it up in the environment
;  Arguments:
;     expr - A Var expression containing a variable name
;     env - The environment in which to look up the variable
;     k - The continuation to receive the result
;     stack - The current call stack
;  Returns:
;     Via k: The value bound to the variable or RuntimeError if variable is not defined
(define (eval-var expr env k stack)
  (let* ([var-args (Expr-args expr)]
         [name (first var-args)]
         [result (env-lookup env name)])        
    (if (EtaError? result)
        (k (localize-error-location result (Expr-loc expr)) stack)
        (k result stack))))


;  eval-quote
;     Evaluate a quoted expression by converting it to a runtime value
;  Arguments:
;     expr - A Quote expression containing the quoted form
;     env - The environment (unused for quote)
;     k - The continuation to receive the result
;     stack - The current call stack
;  Returns:
;     Via k: A RuntimeValue containing the quoted expression as a Scheme value
(define (eval-quote expr env k stack)
  (let* ([quote-args (Expr-args expr)]
         [quoted-form (first quote-args)])
    (k (quoted-expr->runtime-value quoted-form) stack)))

;  arg-to-param-spec
;     Convert an Arg expression to a ParamSpec structure
;  Arguments:
;     arg-expr - An Arg expression with required and variadic parameters
;  Returns:
;     A ParamSpec with required and variadic parameters
(define (arg-to-param-spec arg-expr)
  (unless (and (Expr? arg-expr)
               (eq? (Expr-head arg-expr) 'ArgHead))
    (error "Internal error: expected Arg expression, got: ~a" arg-expr))

  (let* ([arg-args (Expr-args arg-expr)]
         [required-params (first arg-args)]
         [variadic-params (second arg-args)])

    (make-param-spec required-params variadic-params)))

;  eval-lambda
;     Create a closure from a lambda expression
;  Arguments:
;     expr - A Lambda expression containing parameters and body
;     env - The environment to capture in the closure
;     k - The continuation to receive the result
;     stack - The current call stack
;  Returns:
;     Via k: An RuntimeValue containing an EtaClosure
(define (eval-lambda expr env k stack)
  (let* ([lambda-args (Expr-args expr)]
         [param-spec (arg-to-param-spec (first lambda-args))]
         [body (second lambda-args)]
         [loc (Expr-loc expr)])
    (k (make-runtime-value 'EtaClosureTag
                       (make-eta-closure param-spec body env loc #f)) stack)))
                       ;                                         ^^^ anonymous function name

;  eval-if
;     Evaluate a conditional expression
;  Arguments:
;     expr     - An If expression with test, then, and optional else clauses
;     env      - The environment in which to evaluate
;     k        - The continuation to receive the result
;     stack    - The current call stack
;     tail?    - Whether this expression is in tail position (#t) or not (#f)
;  Returns:
;     Via k: The result of evaluating the selected branch or a RuntimeError
(define (eval-if expr env k stack #:tail? [tail? #f])
  (let* ([if-args   (Expr-args expr)]
         [test-expr (first if-args)]
         [then-expr (second if-args)]
         [else-expr (third if-args)])
    (eval-expr test-expr env
      (lambda (test-result test-stack)
        (if (RuntimeError? test-result)
            (k test-result test-stack)
            (if (equal? (RuntimeValue-tag test-result) 'BooleanTag)
                (if (equal? (RuntimeValue-value test-result) #t)
                    (eval-expr then-expr env k test-stack #:tail? tail?)
                    (eval-expr else-expr env k test-stack #:tail? tail?))
                (k (make-runtime-error
                    (format "Only boolean value is allowed in condition. Given ~a" 
                            (RuntimeValue-tag test-result))
                    (Expr-loc test-expr)) 
                   test-stack))))
      stack)))

; attach-function-name
;     Attach a function name to a RuntimeValue for better error reporting
;  Arguments:
;     value - A RuntimeValue with EtaClosureTag
;     name  - The name to attach
;  Returns:
;     A new RuntimeValue with the function name attached
(define (attach-function-name value name)
  (when (not (RuntimeValue? value))
      (error "Internal error: expected RuntimeValue, got: ~a" value))
  
  (when (not (equal? (RuntimeValue-tag value) 'EtaClosureTag))
      (error "Internal error: expected EtaClosureTag, got: ~a" (RuntimeValue-tag value)))
  
  (let ([closure (RuntimeValue-value value)])
    (when (not (Closure? closure))
        (error "Internal error: expected Closure, got: ~a" closure))
    
    (make-runtime-value 'EtaClosureTag
                        (make-eta-closure 
                         (Closure-params-spec closure)
                         (Closure-body closure)
                         (Closure-captured-env closure)
                         (Closure-loc closure)
                         name))))


;  eval-define
;     Define a variable in the current environment
;  Arguments:
;     expr - A Define expression with name and value
;     env  - The environment in which to define the variable
;     k    - The continuation to receive the result
;     stack - The current call stack
;  Returns:
;     Via k: The defined value or a RuntimeError
(define (eval-define expr env k stack)
  (let* ([args    (Expr-args expr)]
         [var-expr (first args)]
         [val-expr (second args)]
         [var-name (first (Expr-args var-expr))])
    (eval-expr val-expr env
      (lambda (v new-stack)
        (if (RuntimeError? v)
            (k v new-stack)
              (let ([define-result (define-variable! env var-name (if (equal? (RuntimeValue-tag v) 'EtaClosureTag)
                                                                      (attach-function-name v var-name)
                                                                      v) #f)])
                (if (RuntimeError? define-result)
                    (k define-result new-stack)
                    (k v new-stack)))))
      stack)))

;  arity-check
;     Check if arguments match parameter specification
;  Arguments:
;     param-spec - A ParamSpec structure with required and variadic params
;     args - List of arguments to check
;  Returns:
;     #t if arity check passes, #f otherwise
(define (arity-check param-spec args)
  (unless (ParamSpec? param-spec)
    (error "Internal error: expected ParamSpec, got: ~a" param-spec))
    
  (let ([req-params (ParamSpec-required param-spec)]
        [variadic? (ParamSpec-variadic param-spec)])
    (if (has-rest? param-spec)
        (>= (length args) (length req-params))  ; With variadic param, need at least req-params
        (= (length args) (length req-params))))) ; Without variadic param, need exact match

;  assign-params!
;     Bind arguments to parameters in environment
;  Arguments:
;     param-spec - A ParamSpec structure with required and variadic params
;     args - List of arguments to bind
;     env - Environment to extend with bindings
;  Returns:
;     The environment with new bindings
(define (assign-params! param-spec args env)
  (let ([req-params (ParamSpec-required param-spec)]
        [variadic (ParamSpec-variadic param-spec)])

    (let ([req-count (length req-params)])
      (for ([i (in-range req-count)])
        (when (< i (length args))
          (define-variable! env (list-ref req-params i) (list-ref args i) #f))))

    (when variadic
      (let ([rest-args (list-tail args (min (length args) (length req-params)))])
        (define-variable! env variadic (list->Pair rest-args) #f)))
    
    env))

;  eval-app
;     Apply a function to arguments
;  Arguments:
;     expr - An App expression with operator and operands
;     env - The environment in which to evaluate
;     k - The continuation to receive the result
;     stack - The current call stack
;     tail? - Whether this expression is in tail position (#t) or not (#f)
;  Returns:
;     Via k: The result of applying the function to its arguments
(define (eval-app expr env k stack #:tail? [tail? #f])
  (let* ([app-args (Expr-args expr)]
         [operator-expr (first app-args)]
         [operand-exprs (first (rest app-args))]
         [loc (Expr-loc expr)])
  
    (eval-expr operator-expr env
      (lambda (operator-value op-stack)
        (if (RuntimeError? operator-value)
            (k operator-value op-stack)
            
            ; Handle continuations, closures and builtins
            (if (not (or (equal? (RuntimeValue-tag operator-value) 'EtaClosureTag)
                         (equal? (RuntimeValue-tag operator-value) 'EtaBuiltinTag)
                         (equal? (RuntimeValue-tag operator-value) 'EtaContinuationTag)))
                (k (make-runtime-error
                    (format "Application of non-function: ~a" (RuntimeValue-tag operator-value))
                    (Expr-loc operator-expr))
                   op-stack)
                
                (eval-toplevel-exprs-cps operand-exprs env
                  (lambda (args args-stack)
                    (if (RuntimeError? args)
                        (k args args-stack)
                        
                        (cond
                          [(equal? (RuntimeValue-tag operator-value) 'EtaContinuationTag)
                           (if (= (length args) 1)
                               (let ([cont (RuntimeValue-value operator-value)])
                                 ((Continuation-k cont) (first args) (Continuation-stack cont)))
                               (k (make-runtime-error
                                   (format "Continuation expects exactly one argument, got ~a" 
                                           (length args))
                                   (Expr-loc operator-expr))
                                  args-stack))]
                          [(equal? (RuntimeValue-tag operator-value) 'EtaClosureTag)
                           (apply-closure operator-value args env k args-stack loc #:tail? tail?)]
                          [else
                           (apply-builtin operator-value args env 
                                          (lambda (result final-stack)
                                            (if (RuntimeError? result)
                                                (k (collect-stack-trace-for-error 
                                                    (if (EtaError-location result)
                                                        result
                                                        (localize-error-location result loc))
                                                    final-stack)
                                                   final-stack)
                                                (k result final-stack)))
                                          args-stack)])))
                  op-stack))))
      stack)))


;  apply-builtin
;     Apply a builtin function to arguments
;  Arguments:
;     builtin - A Builtin structure
;     args - Evaluated arguments
;     env - The calling environment
;     k - The continuation to receive the result
;     stack - The current call stack
;  Returns:
;     Via k: The result of applying the builtin function
;  Notes:
;     Builtin implementation has responsibility to check arity
(define (apply-builtin builtin args env k stack)
  ; MEMO: Builtin implementation has responsibility to check arity
  (let ([result ((Builtin-proc (RuntimeValue-value builtin)) args env)])
    (if (RuntimeError? result)
        (k (collect-stack-trace-for-error result stack) stack)
        (k result stack))))

;  apply-closure
;     Apply a user-defined function to arguments
;  Arguments:
;     closure-value - A RuntimeValue containing a Closure
;     args - Evaluated arguments
;     env - The calling environment
;     k - The continuation to receive the result
;     stack - The current call stack
;     proc-loc - The source location of the procedure call (for error reporting)
;     tail? - Whether this application is in tail position (#t) or not (#f)
;  Returns:
;     Via k: The result of evaluating the function body in the extended environment
;  Notes:
;     Handles arity check and propagates errors properly
(define (apply-closure closure-value args env k stack proc-loc #:tail? [tail? #f])
  (let* ([closure (RuntimeValue-value closure-value)]
         [param-spec (Closure-params-spec closure)]
         [body (Closure-body closure)]
         [captured-env (Closure-captured-env closure)]
         [loc (Closure-loc closure)])

    (if (not (arity-check param-spec args))
        (k (collect-stack-trace-for-error 
            (make-runtime-error
            (format "Wrong number of arguments. Expected ~a, got ~a"
                    (length (ParamSpec-required param-spec))
                    (length args))
            proc-loc)
            stack) 
           stack)
            
        (let* ([func-env (make-child-env captured-env)]
               [func-env (assign-params! param-spec args func-env)]
               [call-frame (make-call-frame closure-value args proc-loc #f tail?)])
           
          (let ([push-result (call-stack-push stack call-frame)])
            (if (RuntimeError? push-result)
                (k (localize-error-location push-result proc-loc) stack)
                (let ([new-stack push-result])
                  (eval-body body func-env 
                                (lambda (result popped-stack)
                                  (k result popped-stack))
                                new-stack
                                #:tail? #t))))))))

;  eval-set!
;     Assign a new value to an existing variable
;  Arguments:
;     expr - A Set! expression with variable name and value
;     env - The environment in which to set the variable
;     k - The continuation to receive the result
;     stack - The current call stack
;  Returns:
;     Via k: The assigned value or a RuntimeError
(define (eval-set! expr env k stack)
  (let* ([set-args (Expr-args expr)]
         [var-expr (first set-args)]
         [val-expr (second set-args)]
         [var-name (first (Expr-args var-expr))])
    
    (if (not (defined? env var-name))
        (k (make-runtime-error
            (format "Cannot set undefined variable: ~a" var-name)
            (Expr-loc expr))
           stack)
        
        (eval-expr val-expr env
                  (lambda (val new-stack)
                    (if (RuntimeError? val)
                        (k val new-stack)
                        (let ([set-result (define-variable! env var-name val #t)])
                          (if (RuntimeError? set-result)
                              (k (localize-error-location set-result (Expr-loc expr)) new-stack)
                              (k val new-stack)))))
                      stack))))

; eval-body-of-body
;     Evaluate a sequence of expressions in the given environment.
;     Returns the **last evaluated expression** or **first** error through continuations.
;     WARN: This function is used for evaluating the list of expressions in a body. Don't use it directly.
; Arguments:
;     expr-list - A list of expressions to evaluate
;     env - The environment in which to evaluate
;     k - The continuation to receive the final result
;     stack - The current call stack
; Returns:
;     Via k: The last evaluated expression or a RuntimeError
(define (eval-body-of-body expr-list env k stack #:tail? [tail? #f])
  (if (null? expr-list)
      (k (collect-stack-trace-for-error
          (make-runtime-error (format "Cannot evaluate empty expression list"))
          stack) 
         stack)
      (let loop ([exprs expr-list]
                 [result '()]
                 [current-stack stack])
        (if (null? exprs)
            (k (first result) current-stack)
            (let ([is-last-expr? (null? (rest exprs))])
              (eval-expr 
                (first exprs) 
                env 
                (lambda (new-result new-stack)
                  (if (RuntimeError? new-result)
                      (k (collect-stack-trace-for-error new-result new-stack) new-stack)
                      (loop (rest exprs)
                            (cons new-result result)
                            new-stack)))
                current-stack
                #:tail? (and tail? is-last-expr?)))))))

;  eval-call/cc
;     Evaluate a call/cc expression
;  Arguments:
;     expr - A CallCC expression containing a procedure
;     env - The environment in which to evaluate
;     k - The continuation to receive the result
;     stack - The current call stack
;     tail? - Whether this expression is in tail position (#t) or not (#f)
;  Returns:
;     Via k: The result of applying the procedure to the reified continuation
(define (eval-call/cc expr env k stack #:tail? [tail? #f])
  (let ([proc-expr (first (Expr-args expr))]
        [loc (Expr-loc expr)])  ; Get the location for error reporting
    ; First, evaluate the procedure
    (eval-expr 
     proc-expr 
     env
     (lambda (proc-value new-stack)
       (if (RuntimeError? proc-value)
           (k proc-value new-stack)
           (let ([reified-cont (make-continuation k new-stack)])
             (cond
               [(and (RuntimeValue? proc-value) 
                     (equal? (RuntimeValue-tag proc-value) 'EtaClosureTag))
                (apply-closure proc-value (list reified-cont) env k new-stack loc #:tail? tail?)]
               [(and (RuntimeValue? proc-value)
                     (equal? (RuntimeValue-tag proc-value) 'EtaBuiltinTag))
                (let ([builtin (RuntimeValue-value proc-value)])
                  (apply-builtin builtin (list reified-cont) env k new-stack))]
               [else 
                (k (make-runtime-error 
                    (format "call/cc requires a procedure as an argument, got: ~a" 
                            (if (RuntimeValue? proc-value) 
                                (runtime-value->string proc-value) 
                                proc-value))
                    loc)
                   new-stack)]))))
     stack)))

;  eval-body
;     Evaluate a body expression (a list of defines and expressions)
;  Arguments:
;     expr - A Body expression containing a list of expressions
;     env - The environment in which to evaluate
;     k - The continuation to receive the result
;     stack - The current call stack
;     tail? - Whether this expression is in tail position (#t) or not (#f)
;  Returns:
;     Via k: The result of the last expression or void if empty
(define (eval-body expr env k stack #:tail? [tail? #f])
  (let ([defines (first (Expr-args expr))]
        [exps    (second (Expr-args expr))])
    
    (unless (and (list? defines)
                (andmap Expr? defines))
      (k (make-runtime-error 
          (format "Internal error: expected a list of Expr for defines, got: ~a" defines))
         stack))
    
    (unless (and (list? exps)
                (andmap Expr? exps))
      (k (make-runtime-error
          (format "Internal error: expected a list of Expr for expressions, got: ~a" exps))
         stack))
    
    (eval-toplevel-exprs-cps defines env
      (lambda (result new-stack)
        (if (RuntimeError? result)
            (k result new-stack)
            (if (null? exps)
                (k (RuntimeValue 'VoidTag '()) new-stack)
                (eval-body-of-body exps env k new-stack #:tail? tail?))))
      stack)))


; string-const-node?
;     Check if a node is a StringConstNode
(define (string-const-node? node)
  (and (Expr? node)
       (eq? (Expr-head node) 'ConstHead)
       (let ([args (Expr-args node)])
         (and (= (length args) 2)
              (equal? (first args) 'StringConstNode)))))


;  eval-load
;     Evaluate a Load expression to load and execute a file
;  Arguments:
;     expr - A Load expression containing the file name
;     env - The environment in which to evaluate
;     k - The continuation to receive the result
;     stack - The current call stack
;     tail? - Whether this expression is in tail position (#t) or not (#f)
;  Returns:
;     Via k: The result of evaluating the loaded file or a RuntimeError
(define (eval-load expr env k stack #:tail? [tail? #f])
  (let* ([load-args (Expr-args expr)])
    (unless (and (list? load-args)
                 (= (length load-args) 1)
                 (string-const-node? (first load-args)))
      (k (make-runtime-error
          (format "Load expression must have exactly one string argument, got: ~a" load-args))
         stack))

    (let* ([filename (first load-args)]
           [file-exists? (file-exists? filename)])
        (if (not file-exists? )
            (k (make-runtime-error
                (format "File not found: ~a" filename)
                (Expr-loc expr))
              stack)
            (let* ([content (file->string filename)] 
                   [tokens (tokenize content filename)])
                  (if (EtaError? tokens)
                    ; MEMO: localize error location is not needed here. It is better to display
                    ; the original location from the file.
                    (k tokens stack)
                    (let ([parsed-result (desugar (parse tokens))])
                      (if (EtaError? parsed-result)
                          (k parsed-result stack)  
                          (let ([result (eval-toplevel-exprs parsed-result env)])
                            (k result stack))))))))))
;  expr->runtime-value
;     Convert an Expr to a RuntimeValue using Pair and Symbol tags
;  Arguments:
;     expr - An Expr to convert
;  Returns:
;     A RuntimeValue representing the expression as a Scheme value
(define (expr->runtime-value expr)
  (cond
    ; Constants
    [(equal? (Expr-head expr) 'ConstHead)
     (let* ([const-args (Expr-args expr)]
            [node-typ (first const-args)]
            [value (second const-args)])
       (cond
         [(equal? node-typ 'IntConstNode)       (RuntimeValue 'IntTag value)]
         [(equal? node-typ 'FloatConstNode)     (RuntimeValue 'FloatTag value)]
         [(equal? node-typ 'BoolConstNode)      (RuntimeValue 'BooleanTag value)]
         [(equal? node-typ 'StringConstNode)    (RuntimeValue 'StringTag value)]
         [(equal? node-typ 'NilConstNode)       (RuntimeValue 'NilValueTag '())]
         [(equal? node-typ 'VoidConstNode)      (RuntimeValue 'VoidTag '())]
         [(equal? node-typ 'UndefinedConstNode) (RuntimeValue 'UndefinedTag 'undefined)]
         [else 
          (make-runtime-error
           (format "Internal error: unexpected ConstNode type in quoted expression: ~a" node-typ))]))]
    
    ; Identifiers (convert to symbols)
    [(equal? (Expr-head expr) 'IdHead)
     (let ([name (first (Expr-args expr))])
       (RuntimeValue 'SymbolTag name))]
    
    ; S-expressions (list structure)
    [(equal? (Expr-head expr) 'S-ExprHead)
     (let* ([sexpr-args (Expr-args expr)]
            [items (first sexpr-args)]
            [dotted (second sexpr-args)])
       ; Build the list recursively from right to left
       (let ([tail-value (if (null? dotted)
                            (RuntimeValue 'NilValueTag '())
                            (expr->runtime-value dotted))])
         (foldr
          (lambda (item acc)
            (let* ([item-value (expr->runtime-value item)]
                   [pair (make-Pair item-value acc)])
              (RuntimeValue 'PairTag pair)))
          tail-value
          items)))]
    
    ; Fallback for other expressions
    [else
     (make-runtime-error
      (format "Internal error: Cannot convert complex expression to S-expression: ~a" 
              (pretty-print-Expr expr)))]))

;  quoted-expr->runtime-value
;     Process a quoted expression for eval-quote
;  Arguments:
;     expr - The expression from the quote form
;  Returns:
;     A RuntimeValue representing the quoted expression
(define (quoted-expr->runtime-value expr)
  ; Direct conversion for S-expressions and other forms
  (expr->runtime-value expr))

;  collect-stack-trace-for-error
;     Collects stack trace information for a runtime error
;  Arguments:
;     error - The RuntimeError object
;     stack - The current call stack
;  Returns:
;     A RuntimeError object with stack trace information added
(define (collect-stack-trace-for-error error stack)
  (if (RuntimeError? error)
      (add-stack-trace-to-error error (call-stack->stack-trace stack))
      error))