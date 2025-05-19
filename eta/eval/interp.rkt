#lang racket

(require "../parser/ast.rkt"
         "../utils/error.rkt"
         "runtime-values.rkt"
         "env.rkt"
         "stack-frame.rkt"
         )


(provide eval-expr 
         eval-each-expr
 )


; eval-sequence
;     Evaluate a sequence of expressions in the given environment.
;     Returns the **last evaluated expression** or **first** error through continuations.
; Arguments:
;     expr-list - A list of expressions to evaluate
;     env - The environment in which to evaluate
;     k - The continuation to receive the final result
;     stack - The current call stack
; Returns:
;     Via k: The last evaluated expression or a RuntimeError
(define (eval-sequence expr-list env k stack)
  (if (null? expr-list)
      (k (make-runtime-error (format "Cannot evaluate empty expression list")) stack)
      (let loop ([exprs expr-list]
                 [result '()]
                 [current-stack stack])
        (if (null? exprs)
            (k (first result) current-stack)
            (eval-expr 
              (first exprs) 
              env 
              (lambda (new-result new-stack)
                (if (RuntimeError? new-result)
                    (k new-result new-stack)
                    (loop (rest exprs)
                          (cons new-result result)
                          new-stack)))
              current-stack)))))

; eval-each-expr
;    Main entry point for evaluating expressions in the interpreter.
; Arguments:
;    expr-list - A list of expressions to evaluate
;    env - The environment in which to evaluate
; Returns:
;    A list of evaluated expressions or a RuntimeError
; Example:
;   (eval-each-expr (list (make-const 'Num 1) (make-const 'Num 2) (make-const 'Num 3)) env)
;   => (1 2 3)
(define (eval-each-expr expr-list env)
  (eval-each-expr-cps expr-list env
    (lambda (result stack) result)
    (init-call-stack)))


;  eval-each-expr-cps
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
;   (eval-each-expr-cps (list (make-const 'Num 1) (make-const 'Num 2) (make-const 'Num 3)) env k stack)
;   => via k: (1 2 3)
(define (eval-each-expr-cps expr-list env k stack)
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
               (k new-result new-stack)
               (loop (rest exprs)
                     (cons new-result result)
                     new-stack)))
         current-stack))))


; with-successfull-eval
;     Evaluate a procedure if the result is not a RuntimeError
;  Arguments:
;     result - RuntimeError or EtaValue or list of EtaValues
;     proc   - The procedure to evaluate if result is not a RuntimeError
;  Returns:
;     The result of the evaluation or the RuntimeError
(define (with-successfull-eval result proc)
  (if (RuntimeError? result)
      result
      (if (or (EtaValue? result)
              (and (list? result) (andmap EtaValue? result)))
          (proc result)
          (error "Internal error: expected EtaValue, got: ~a" result))))



;; with-successfull-eval-cps
;;    Evaluate a procedure if the result is not a RuntimeError
;; Arguments:
;;    result - RuntimeError or EtaValue or list of EtaValues
;;    success-cont - Continuation with result if evaluation was successful
;;    error-cont - Continuation with error if result is RuntimeError
;; Returns:
;;    The result of applying the appropriate continuation
(define (with-successfull-eval-cps result success-cont error-cont stack)
  (if (RuntimeError? result)
      (error-cont result stack)
      (if (or (EtaValue? result)
              (and (list? result) (andmap EtaValue? result)))
          (success-cont result stack)
          (error-cont (make-runtime-error (format "Internal error: expected EtaValue, got: ~a" result)) stack))))

;; apply-continuation
;;    Apply a continuation function to a value
;; Arguments:
;;    k - The continuation function
;;    result - The result value to pass to the continuation
;;    stack - The call stack
;; Returns:
;;    The result of applying the continuation to the value
(define (apply-continuation k result stack)
  (k result stack))

;; create-error-continuation
;;    Create a continuation that passes an error upward
;; Arguments:
;;    k - The parent continuation
;; Returns:
;;    A continuation that passes errors to the parent continuation
(define (create-error-continuation k)
  (lambda (result stack)
    (if (RuntimeError? result)
        (k result stack)
        (k (make-runtime-error "Expected error, got value") stack))))


;  eval-expr
;     Evaluate an expression based on its head type
;  Arguments:
;     expr - The **desugared** expression to evaluate
;     env - The environment in which to evaluate
;     k - The continuation to receive the result
;     stack - The current call stack
;  Returns:
;     Via k: An EtaValue representing the result
(define (eval-expr expr env k stack)
  (unless (Expr? expr)
    (k (make-runtime-error 
        (format "Internal error: expr must be an Expr, got: ~a" expr))
       stack))
  
  (let ([head (Expr-head expr)])
    (cond
      [(equal? head 'ConstHead)  (eval-const expr env k stack)]
      [(equal? head 'IdHead)     (eval-var expr env k stack)]
      [(equal? head 'AppHead)    (eval-app expr env k stack)]
      [(equal? head 'LambdaHead) (eval-lambda expr env k stack)]
      [(equal? head 'QuoteHead)  (eval-quote expr env k stack)]
      [(equal? head 'DefineHead) (eval-define expr env k stack)]
      [(equal? head 'IfHead)     (eval-if expr env k stack)]
      [(equal? head 'SetHead)    (eval-set! expr env k stack)]
      [(equal? head 'BodyHead)   (eval-body expr env k stack)]
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
;     Via k: An EtaValue with appropriate tag and value
(define (eval-const expr env k stack)
  (let* ([const-args (Expr-args expr)]
         [node-typ (first const-args)]
         [value (second const-args)])
    (cond
      [(equal? node-typ 'IntConstNode)       (k (EtaValue 'IntTag value) stack)]
      [(equal? node-typ 'FloatConstNode)     (k (EtaValue 'FloatTag value) stack)]
      [(equal? node-typ 'BoolConstNode)      (k (EtaValue 'BooleanTag value) stack)]
      [(equal? node-typ 'StringConstNode)    (k (EtaValue 'StringTag value) stack)]
      [(equal? node-typ 'NilConstNode)       (k (EtaValue 'NilValueTag '()) stack)]
      [(equal? node-typ 'VoidConstNode)      (k (EtaValue 'VoidTag '()) stack)]
      [(equal? node-typ 'UndefinedConstNode) (k (EtaValue 'UndefinedTag 'undefined) stack)]
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
;     Evaluate a quoted expression by returning it as an EtaValue
;  Arguments:
;     expr - A Quote expression containing the quoted form
;     env - The environment (unused for quote)
;     k - The continuation to receive the result
;     stack - The current call stack
;  Returns:
;     Via k: An EtaValue containing the quoted expression
(define (eval-quote expr env k stack)
  (let* ([quote-args (Expr-args expr)]
         [quoted-form (first quote-args)])
    (k (make-runtime-value 'EtaExprTag quoted-form) stack)))

;  convert-to-param-spec
;     Convert an Arg expression to a ParamSpec structure
;  Arguments:
;     arg-expr - An Arg expression with required and variadic parameters
;  Returns:
;     A ParamSpec with required and variadic parameters
(define (convert-to-param-spec arg-expr)
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
;     Via k: An EtaValue containing an EtaClosure
(define (eval-lambda expr env k stack)
  (let* ([lambda-args (Expr-args expr)]
         [param-spec (convert-to-param-spec (first lambda-args))]
         [body (second lambda-args)]
         [loc (Expr-loc expr)])
    (k (make-runtime-value 'EtaClosureTag
                       (make-eta-closure param-spec body env loc)) stack)))


;  arity-check
;     Check if arguments match parameter specification
;  Arguments:
;     param-spec - A ParamSpec structure with required and variadic params
;     args - List of arguments to check
;  Returns:
;     #t if arity check passes, #f otherwise
(define (arity-check param-spec args)
  (let ([req-params (ParamSpec-required param-spec)]
        [variadic? (ParamSpec-variadic param-spec)])
    (if variadic?
        (>= (length args) (length req-params))  ; With variadic param, need at least req-params
        (= (length args) (length req-params))))) ; Without variadic param, need exact match

;  assign-params
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

    ; Bind required parameters
    (let ([req-count (length req-params)])
      (for ([i (in-range req-count)])
        (when (< i (length args))
          (define-variable! env (list-ref req-params i) (list-ref args i) #f))))

    ; Bind variadic parameter if present
    (when variadic
      (let ([rest-args (list-tail args (min (length args) (length req-params)))])
        (define-variable! env variadic
          (EtaValue 'ListTag rest-args) #f)))

    env))

;  eval-if
;     Evaluate a conditional expression
;  Arguments:
;     expr     - An If expression with test, then, and optional else clauses
;     env      - The environment in which to evaluate
;     k        - The continuation to receive the result
;     stack    - The current call stack
;  Returns:
;     Via k: The result of evaluating the selected branch or a RuntimeError
(define (eval-if expr env k stack)
  (let* ([if-args   (Expr-args expr)]
         [test-expr (first if-args)]
         [then-expr (second if-args)]
         [else-expr (third if-args)])
    (eval-expr test-expr env
      (lambda (test-result test-stack)
        (if (RuntimeError? test-result)
            (k test-result test-stack)
            (if (equal? (EtaValue-tag test-result) 'BooleanTag)
                (if (equal? (EtaValue-value test-result) #t)
                    (eval-expr then-expr env k test-stack)
                    (eval-expr else-expr env k test-stack))
                (k (make-runtime-error
                    (format "Only boolean value is allowed in condition. Given ~a" 
                            (EtaValue-tag test-result))
                    (Expr-loc test-expr)) 
                   test-stack))))
      stack)))

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
            (begin
              (define-variable! env var-name v #f) ; non breaking set
              (k v new-stack))))
      stack)))

;  eval-app
;     Apply a function to arguments
;  Arguments:
;     expr - An App expression with operator and operands
;     env - The environment in which to evaluate
;     k - The continuation to receive the result
;     stack - The current call stack
;  Returns:
;     Via k: The result of applying the function to its arguments
(define (eval-app expr env k stack)
  (let* ([app-args (Expr-args expr)]
         [operator-expr (first app-args)]
         [operand-exprs (first (rest app-args))]
         [loc (Expr-loc expr)])
    
    ; Evaluate the operator
    (eval-expr operator-expr env
      (lambda (operator-value op-stack)
        (if (RuntimeError? operator-value)
            (k operator-value op-stack)
            
            ; Check if operator is a function
            (if (not (or (equal? (EtaValue-tag operator-value) 'EtaClosureTag)
                       (equal? (EtaValue-tag operator-value) 'EtaBuiltinTag)))
                ; Error if not a function
                (k (make-runtime-error
                    (format "Application of non-function: ~a" (EtaValue-tag operator-value))
                    (Expr-loc operator-expr))
                   op-stack)
                
                ; Evaluate arguments
                (eval-each-expr-cps operand-exprs env
                  (lambda (args args-stack)
                    (if (RuntimeError? args)
                        (k args args-stack)
                        
                        ; Apply the function
                        (if (equal? (EtaValue-tag operator-value) 'EtaClosureTag)
                            (apply-closure-cps operator-value args env k args-stack loc)
                            (apply-builtin-cps operator-value args env 
                                             (lambda (result final-stack)
                                               ; Localize error location if necessary
                                               (if (and (RuntimeError? result)
                                                       (not (EtaError-location result)))
                                                   (k (localize-error-location result loc) final-stack)
                                                   (k result final-stack)))
                                             args-stack))))
                  op-stack))))
      stack)))

;  apply-builtin
;     Apply a builtin function to arguments
;  Arguments:
;     builtin - A Builtin structure
;     args - Evaluated arguments
;     env - The calling environment
;  Returns:
;     The result of applying the builtin function
;  Raises:
;     RuntimeError if arity check fails
(define (apply-builtin builtin args env)
  ; MEMO: Builtin implementation has responsibility to check arity
  (let ([result ((Builtin-proc (EtaValue-value builtin)) args env)])
    (if (EtaValue? result)
        result
        (make-runtime-error result))))

;  apply-builtin-cps
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
(define (apply-builtin-cps builtin args env k stack)
  ; MEMO: Builtin implementation has responsibility to check arity
  (let ([result ((Builtin-proc (EtaValue-value builtin)) args env)])
    (if (EtaValue? result)
        (k result stack)
        (k (make-runtime-error result) stack))))

;  apply-closure
;     Apply a user-defined function to arguments
;  Arguments:
;     closure-value - A EtaValue containing a Closure
;     args - Evaluated arguments
;     env - The calling environment
;  Returns:
;     The result of evaluating the function body in the extended environment
;  Raises:
;     RuntimeError if arity check fails
(define (apply-closure closure-value args env)
  (let* (
         [closure (EtaValue-value closure-value)]
         [param-spec (Closure-params-spec closure)]
         [body (Closure-body closure)]
         [captured-env (Closure-captured-env closure)]
         [loc (Closure-loc closure)])

    ; Check arity
    (if (not (arity-check param-spec args))
      (make-runtime-error
       (format "Wrong number of arguments. Expected ~a, got ~a"
               (length (ParamSpec-required param-spec))
               (length args)))

    ; Create a new environment with captured env as parent
    (let* ([func-env (make-child-env captured-env)]
           ; Assign arguments to parameters
           [func-env (assign-params! param-spec args func-env)])

      ; Evaluate the body in the extended environment
      (eval-body body func-env)))))

;  apply-closure-cps
;     Apply a user-defined function to arguments
;  Arguments:
;     closure-value - A EtaValue containing a Closure
;     args - Evaluated arguments
;     env - The calling environment
;     k - The continuation to receive the result
;     stack - The current call stack
;     proc-loc - The source location of the procedure call (for error reporting)
;  Returns:
;     Via k: The result of evaluating the function body in the extended environment
;  Notes:
;     Handles arity check and propagates errors properly
(define (apply-closure-cps closure-value args env k stack proc-loc)
  (let* ([closure (EtaValue-value closure-value)]
         [param-spec (Closure-params-spec closure)]
         [body (Closure-body closure)]
         [captured-env (Closure-captured-env closure)]
         [loc (Closure-loc closure)])
    
    ; Check arity
    (if (not (arity-check param-spec args))
        (k (make-runtime-error
            (format "Wrong number of arguments. Expected ~a, got ~a"
                    (length (ParamSpec-required param-spec))
                    (length args))
            proc-loc) 
           stack)
            
        ; Create frame for call and push to stack
        (let* ([func-env (make-child-env captured-env)]
               ; Assign arguments to parameters
               [func-env (assign-params! param-spec args func-env)]
               ; Create call frame
               [call-frame (make-call-frame closure-value args func-env proc-loc #f)])
           
          ; Push frame to stack
          (let ([push-result (call-stack-push stack call-frame)])
            ; Check if push was successful or we got a stack overflow error
            (if (RuntimeError? push-result)
                ; Report stack overflow error
                (k (localize-error-location push-result proc-loc) stack)
                ; Proceed with execution using new stack
                (let ([new-stack push-result])
                  ; Evaluate the body in the extended environment
                  (eval-body body func-env 
                                (lambda (result popped-stack)
                                  ; Pop the frame when done
                                  (k result popped-stack))
                                new-stack))))))))

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
    
    ; Check if variable exists
    (if (not (defined? env var-name))
        (k (make-runtime-error
            (format "Cannot set undefined variable: ~a" var-name)
            (Expr-loc expr))
           stack)
        
        ; Evaluate the value expression
        (eval-expr val-expr env
                  (lambda (val new-stack)
                    (if (RuntimeError? val)
                        (k val new-stack)
                        (begin
                          ; Set the variable
                          (define-variable! env var-name val #t)
                          ; Return the value
                          (k val new-stack))))
                      stack))))

;  eval-body
;     Evaluate a body expression (a list of defines and expressions)
;  Arguments:
;     expr - A Body expression containing a list of expressions
;     env - The environment in which to evaluate
;     k - The continuation to receive the result
;     stack - The current call stack
;  Returns:
;     Via k: The result of the last expression or void if empty
(define (eval-body expr env k stack)
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
    
    (eval-each-expr-cps defines env
      (lambda (result new-stack)
        (if (RuntimeError? result)
            (k result new-stack)
            (if (null? exps)
                (k (EtaValue 'VoidTag '()) new-stack)
                (eval-sequence exps env k new-stack))))
      stack)))
