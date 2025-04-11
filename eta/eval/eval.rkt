#lang racket

(provide eta-eval)

(require "../parser/ast.rkt"
         "env.rkt")

;  eta-eval
;     Evaluates an AST node in the given environment and returns the result.
;  Arguments:
;      expr - The AST expression to evaluate (an Expr struct).
;      env - The environment in which to evaluate the expression.
;  Returns:
;      The result of evaluating the expression.
;  Example:
;      (eta-eval (parse "(+ 1 2)") global-env) ; => 3
(define (eta-eval expr env)
  (match expr
    ;; Cost
    [(Expr head args _) #:when (eq? head Const)
     (first args)]
    
    ;; Variable lookup
    [(Expr head args _) #:when (eq? head Var)
     (env-lookup env (first args))]
    
    ;; Application
    [(Expr head args _) #:when (eq? head App)
     (let* ([f-expr (first args)]
            [arg-exprs (rest args)]
            [f-val (eta-eval f-expr env)])
       (cond
         ;; TODO: better registration of built-in functions
         [(eq? f-val '+) (apply + (map (lambda (a) (eta-eval a env)) arg-exprs))]
         [(eq? f-val '-) (apply - (map (lambda (a) (eta-eval a env)) arg-exprs))]
         [(eq? f-val '*) (apply * (map (lambda (a) (eta-eval a env)) arg-exprs))]
         [(eq? f-val '/) (apply / (map (lambda (a) (eta-eval a env)) arg-exprs))]
         [else (error "Unsupported function application" f-val)]))]
    
    ;; Quote
    [(Expr head args _) #:when (eq? head Quote)
     (first args)]
    
    [_ (error "Unsupported AST node" expr)]))
