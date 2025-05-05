#lang racket

(require "../parser/ast.rkt"
         "../utils/error.rkt"
         "runtime-values.rkt"
         "env.rkt"
         )


(provide eval-expr 
         eval-each-expr
 )


; eval-sequence
;     Evaluate a sequence of expressions in the given environment.
;     Returns the **last evaluated expression** or **first** error.
; Arguments:
;     expr-list - A list of expressions to evaluate
;     env - The environment in which to evaluate
; Returns:
;     The last evaluated expression or a RuntimeError
(define (eval-sequence expr-list env)
  (if (null? expr-list)
      (make-runtime-error (format "Cannot evaluate empty expression list"))
      (let loop ([exprs expr-list]
                 [result '()])
        (if (null? exprs)
            (first result)
            (let ([new-result (eval-expr (first exprs) env)])
              (if (RuntimeError? new-result)
                  new-result
                  (loop (rest exprs)
                        (cons new-result result))))))))


; eval-each-expr
;     Evaluate a list of expressions in the given environment.
;     Returns the list of each evaluated expression in given order or return **first** error.
;     !! Does not return last value of the list !!
;  Arguments:
;     expr-list - A list of expressions to evaluate
;     env - The environment in which to evaluate
;  Returns:
;     A list of evaluated expressions or a RuntimeError
;  Example:
;   (eval-each-expr (list (make-const 'Num 1) (make-const 'Num 2) (make-const 'Num 3)) env)
;   => (1 2 3)
(define (eval-each-expr expr-list env)
   (let loop ([exprs expr-list]
                 [result '()])
        (if (null? exprs)
            (reverse result)
            (let ([new-result (eval-expr (first exprs) env)])
              (if (RuntimeError? new-result)
                  new-result
                  (loop (rest exprs)
                        (cons new-result result)))))))


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


;  eval-expr
;     Evaluate an expression based on its head type
;  Arguments:
;     expr - The **desugared** expression to evaluate.
;     env - The environment in which to evaluate
;  Returns:
;     An EtaValue representing the result
(define (eval-expr expr env)
  (unless (Expr? expr)
    (error "Internal error: expr must be an Expr, got: ~a" expr))

  (let ([head (Expr-head expr)])
    (cond
      [(equal? head 'ConstHead)  (eval-const expr env)]
      [(equal? head 'VarHead)    (eval-var expr env)]
      [(equal? head 'AppHead)    (eval-app expr env)]
      [(equal? head 'LambdaHead) (eval-lambda expr env)]
      [(equal? head 'QuoteHead)  (eval-quote expr env)]
      [(equal? head 'DefineHead) (eval-define expr env)]
      [(equal? head 'IfHead)     (eval-if expr env)]
      [(equal? head 'SetHead)    (eval-set! expr env)]
      [(equal? head 'NilHead)    (eval-nil expr env)]
      [(equal? head 'BodyHead)   (eval-body expr env)]
      [else (error "Internal error: unexpected ExprHead after desugaring: ~a"
                   (ExprHead->name head))])))

;  eval-const
;     Evaluate a constant expression
;  Arguments:
;     expr - A Const expression with tag and value
;     env - The environment (unused for constants)
;  Returns:
;     An EtaValue with appropriate tag and value
(define (eval-const expr env)
  (let* ([const-args (Expr-args expr)]
         [const-tag (first const-args)]
         [value (second const-args)])
    (cond
      [(equal? const-tag 'Num) (EtaValue 'NumberTag value)]
      [(equal? const-tag 'Bool) (EtaValue 'BooleanTag value)]
      [(equal? const-tag 'String) (EtaValue 'StringTag value)]
      [(equal? const-tag 'Void) (EtaValue 'VoidTag '())]
      [(equal? const-tag 'Undefined) (EtaValue 'UndefinedTag 'undefined)])))

;  eval-var
;     Evaluate a variable expression by looking it up in the environment
;  Arguments:
;     expr - A Var expression containing a variable name
;     env - The environment in which to look up the variable
;  Returns:
;     The value bound to the variable
;  Raises:
;     RuntimeError if variable is not defined
(define (eval-var expr env)
  (let* ([var-args (Expr-args expr)]
         [name (first var-args)]
         [result (env-lookup env name)])        
         (if (EtaError? result)
             (localize-error-location result (Expr-loc expr))
             result
         )
  )
)


;  eval-quote
;     Evaluate a quoted expression by returning it as an EtaValue
;  Arguments:
;     expr - A Quote expression containing the quoted form
;     env - The environment (unused for quote)
;  Returns:
;     An EtaValue containing the quoted expression
(define (eval-quote expr env)
  (let* ([quote-args (Expr-args expr)]
         [quoted-form (first quote-args)])
    (make-runtime-value 'EtaExprTag quoted-form)))

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
;  Returns:
;     An EtaValue containing an EtaClosure
(define (eval-lambda expr env)
  (let* ([lambda-args (Expr-args expr)]
         [param-spec (convert-to-param-spec (first lambda-args))]
         [body (second lambda-args)]
         [loc (Expr-loc expr)])
    (make-runtime-value 'EtaClosureTag
                        (make-eta-closure param-spec body env loc))))


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
;  Returns:
;     The result of evaluating the selected branch or a RuntimeError
(define (eval-if expr env)
  (let* ([if-args   (Expr-args expr)]
         [has-else  (first if-args)]
         [test-expr (second if-args)]
         [then-expr (third if-args)]
         [else-expr (fourth if-args)])
    (with-successfull-eval (eval-expr test-expr env)
      (lambda (test-result)
        (if (equal? (EtaValue-tag test-result) 'BooleanTag)
            (if (equal? (EtaValue-value test-result) #t)
                (eval-expr then-expr env)
                (if has-else
                    (eval-expr else-expr env)
                    (EtaValue 'VoidTag '())))
            (make-runtime-error
             (format "Only boolean value is allowed in condition. Given ~a" (EtaValue-tag test-result))
             (Expr-loc test-expr)))))))

;  eval-define
;     Define a variable in the current environment
;  Arguments:
;     expr - A Define expression with name and value
;     env  - The environment in which to define the variable
;  Returns:
;     The defined value or a RuntimeError
(define (eval-define expr env)
  (let* ([args    (Expr-args expr)]
         [var-expr (first args)]
         [val-expr (second args)]
         [var-name (first (Expr-args var-expr))])
    (with-successfull-eval (eval-expr val-expr env)
      (lambda (v)
        (define-variable! env var-name v #f) ; non breaking set
        v))))

;  eval-app
;     Apply a function to arguments
;  Arguments:
;     expr - An App expression with operator and operands
;     env - The environment in which to evaluate
;  Returns:
;     The result of applying the function to its arguments
;  Raises:
;     RuntimeError if the operator is not applicable or if arity check fails
(define (eval-app expr env)
  (let* ([app-args (Expr-args expr)]
         [operator-expr (first app-args)]
         [operand-exprs  (first (rest app-args))])

    (with-successfull-eval (eval-expr operator-expr env)
      (lambda (operator-value)
        (if (not (or (equal? (EtaValue-tag operator-value) 'EtaClosureTag)
                     (equal? (EtaValue-tag operator-value) 'EtaBuiltinTag)))
          (make-runtime-error
           (format "Application of non-function: ~a" (EtaValue-tag operator-value))
           (Expr-loc operator-expr))

        (with-successfull-eval (eval-each-expr operand-exprs env)
          (lambda (args)
            (let ([result
                   (if (equal? (EtaValue-tag operator-value) 'EtaClosureTag)
                       (apply-closure operator-value args env)
                       (apply-builtin operator-value args env))])

              (if (and (RuntimeError? result)
                       (not (EtaError-location result)))
                  (localize-error-location result (Expr-loc expr))
                  result)))))))))

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


;  eval-set!
;     Assign a new value to an existing variable
;  Arguments:
;     expr - A Set! expression with variable name and value
;     env - The environment in which to set the variable
;  Returns:
;     The assigned value
;  Raises:
;     RuntimeError if variable does not exist
(define (eval-set! expr env)
  (let* ([set-args (Expr-args expr)]
         [var-expr (first set-args)]
         [val-expr (second set-args)]
         [var-name (first (Expr-args var-expr))]
         [val (eval-expr val-expr env)])

    ; Check if variable exists
    (unless (defined? env var-name)
      (make-runtime-error
       (format "Cannot set undefined variable: ~a" var-name)
       (Expr-loc expr)
       ))

    ; Set the variable
    (define-variable! env var-name val)

    ; Return the value
    val))

;  eval-body
;     Evaluate a body expression (a list of defines and expressions)
;  Arguments:
;     expr - A Body expression containing a list of expressions
;     env - The environment in which to evaluate
;  Returns:
;     The result of the last expression or void if empty
(define (eval-body expr env)
  (let ([defines (first (Expr-args expr))]
        [exps    (second (Expr-args expr))])
    (with-successfull-eval (eval-each-expr defines env)
      (lambda (result)
        (if (null? exps)
            (EtaValue 'VoidTag '())
            (eval-sequence exps env))))))

;  eval-nil
;     Evaluate a nil expression (empty list)
;  Arguments:
;     expr - A Nil expression
;     env - The environment (unused for nil)
;  Returns:
;     An EtaValue representing the empty list
(define (eval-nil expr env)
  (EtaValue 'NilValueTag '()))
