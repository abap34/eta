#lang racket

(require "env.rkt"
         "builtins.rkt"
         "interp.rkt"
         "runtime-values.rkt"
         "../utils/error.rkt"
         "../utils/console.rkt"
         "../parser/tokenizer.rkt"
         "../parser/parser.rkt"
         "../parser/ast.rkt"
         "../desugar/desugar.rkt"
)

(provide init-basic-env
         eta-eval
         eta-eval-toplevel
         format-eval-result
         exit-with-eval-result
)


; EvalResult
;  Represents the result of evaluating an expression.
;    - success? - A boolean indicating if the evaluation was successful. (bool)
;    - value - The evaluated value or an error message. 
;               if success? is #t, this is the evaluated value. (EtaValue | list of EtaValue)
;               if success? is #f, this is an error. (EtaError)
(struct EvalResult (success? value) #:transparent)

; format-eval-result
;   Converts an EvalResult to a string representation.
; Arguments:
;   result - An EvalResult to convert.
; Returns:
;   A string representation of the EvalResult.
(define (format-eval-result result source)
  (if (EvalResult-success? result)
      (if (list? (EvalResult-value result))
          (string-join (map (lambda (v) (colorize (runtime-value->string v) 'cyan)) (EvalResult-value result)) "\n")
          (colorize (runtime-value->string (EvalResult-value result)) 'cyan))
        (colorize (format-error-with-source (EvalResult-value result) source) 'red)))

; exit-with-eval-result
;   Exits the program with the given EvalResult.
; Arguments:
;   result - An EvalResult to exit with.
; Returns:
;   Never returns (exits the program).
(define (exit-with-eval-result result source)
  (if (EvalResult-success? result)
    (exit 0)
    (begin
      (displayln (format-eval-result result source))
      (exit 1))))    

; init-basic-env
;   Initializes the basic environment for the eta interpreter.
;   Environment includes builtin functions.
; Arguments:
;   None
; Returns:
;   An environment with builtin functions and variables.
(define (init-basic-env)
  (add-builtins-to-env (init-toplevel-env)))


; eta-eval
;   Evaluates an expression in the given environment.
; Arguments:
;   env - The environment in which to evaluate the expression.
;   source - The source code to evaluate. (string)
; Returns:
;  EvalResult containing success status and evaluated value.
(define (eta-eval env source)
   (unless (string? source)
     (error "eta-eval: source must be a string"))

  (let ([tokens (tokenize source)])
    (if (EtaError? tokens)
        (EvalResult #f tokens)
        (let ([parsed-result (desugar (parse tokens))])
          (if (EtaError? parsed-result)
              (EvalResult #f parsed-result)
              (let ([result (eval-each-expr parsed-result env)])
                (if (RuntimeError? result)
                    (EvalResult #f result)
                    (EvalResult #t result))))))))

; eta-eval-toplevel
;    Evaluates an script in new toplevel environment.
; Arguments:
;    source - The source code to evaluate. (string)
; Returns:
;    EvalResult containing success status and evaluated value.
(define (eta-eval-toplevel source)
  (let ([global-env (init-basic-env)])
    (eta-eval global-env source)))



