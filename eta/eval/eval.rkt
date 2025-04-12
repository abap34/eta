#lang racket

(provide eta-eval)

(require "../parser/ast.rkt"
         "env.rkt"
         "../utils/error.rkt")

;  apply-builtin-function
;     Applies a built-in function to the given arguments after evaluating them.
;  Arguments:
;      func - The function to apply (a symbol like '+, '-, etc.)
;      arg-exprs - The argument expressions to evaluate
;      env - The environment in which to evaluate the arguments
;      expr-loc - The source location of the expression (for error reporting)
;  Returns:
;      The result of applying the function, or an EtaError on failure.
(define (apply-builtin-function func arg-exprs env expr-loc)
  (let ([eval-args (map (lambda (a) (eta-eval a env)) arg-exprs)])
    (if (ormap EtaError? eval-args)
        (first (filter EtaError? eval-args)) ; Return the first error
        (cond
          [(eq? func '+) (apply + eval-args)]
          [(eq? func '-) (apply - eval-args)]
          [(eq? func '*) (apply * eval-args)]
          [(eq? func '/) 
           (if (and (> (length eval-args) 1) (= (cadr eval-args) 0))
               (make-runtime-error "Division by zero" expr-loc)
               (apply / eval-args))]
          [else (make-runtime-error 
                 "Unsupported function application" 
                 expr-loc)]))))

;  eta-eval
;     Evaluates an AST node in the given environment and returns the result.
;  Arguments:
;      expr - The AST expression to evaluate (an Expr struct).
;      env - The environment in which to evaluate the expression.
;  Returns:
;      The result of evaluating the expression, or an EtaError on failure.
;  Example:
;      (eta-eval (parse "(+ 1 2)") global-env) ; => 3
(define (eta-eval expr env)
  (match expr
    ;; Const
    [(Expr head args _) #:when (eq? head Const)
     (first args)]
    
    ;; Variable lookup
    [(Expr head args loc) #:when (eq? head Var)
     (let ([name (first args)])
       (let ([val (env-lookup env name)])
         (if (NotFound? val)
             (make-runtime-error 
              (format "Undefined variable: ~a" (NotFound-name val)) 
              loc)
             val)))]
    
    ;; Application
    [(Expr head args loc) #:when (eq? head App)
     (let* ([f-expr (first args)]
            [arg-exprs (rest args)]
            [f-val (eta-eval f-expr env)])
       
       ;; Check if function evaluation resulted in an error
       (if (EtaError? f-val)
           f-val
           ;; Handle built-in functions
           (if (member f-val '(+ - * /))
               (apply-builtin-function f-val arg-exprs env loc)
               (make-runtime-error 
                "Unsupported function application" 
                loc))))]
    
    ;; Quote
    [(Expr head args _) #:when (eq? head Quote)
     (first args)]
    
    [_ (make-runtime-error 
        "Unsupported AST node" 
        (if (Expr? expr) (Expr-loc expr) #f))])
)
