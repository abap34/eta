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
         racket/exn
)

(provide init-basic-env
         eta-eval
         eta-eval-toplevel
         eta-eval-in-thread
         eval-in-thread
         format-eval-result
         exit-with-eval-result
         with-clean-break-state
         EvalResult-success?
         EvalResult-value
         reset-break-handler  
         eta-eval-toplevel-in-thread
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
        (let ([error-value (EvalResult-value result)])
          (if (EvaluationInterruptedError? error-value)
              (string-append (colorize "Evaluation interrupted by user" 'yellow) "\n")
              (colorize (format-error-with-source error-value source) 'red)))))

; exit-with-eval-result
;   Exits the program with the given EvalResult.
; Arguments:
;   result - An EvalResult to exit with.
;   source - The source code that was evaluated.
; Returns:
;   Never returns (exits the program).
(define (exit-with-eval-result result source)
  (if (EvalResult-success? result)
    (exit 0)
    (begin
      (let ([error-value (EvalResult-value result)])
        (if (EvaluationInterruptedError? error-value)
            (begin
              (displayln (colorize "Evaluation interrupted by user" 'yellow))
              (exit 130)) 
            (begin
              (displayln (format-eval-result result source))
              (exit 1))))))) 

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
              (let ([result (eval-toplevel-exprs parsed-result env)])
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

; eta-eval-toplevel-in-thread
;    Evaluates a script in new toplevel environment with interruption support.
; Arguments:
;    source - The source code to evaluate. (string)
; Returns:
;    EvalResult containing success status and evaluated value.
(define (eta-eval-toplevel-in-thread source)
  (let ([global-env (init-basic-env)])
    (eta-eval-in-thread global-env source)))

; eta-eval-in-thread
;   Evaluates an expression in a separate thread with interruption support.
; Arguments:
;   env - The environment in which to evaluate the expression.
;   source - The source code to evaluate. (string)
; Returns:
;  EvalResult containing success status and evaluated value.
(define (eta-eval-in-thread env source)
  (unless (string? source)
    (error "Internal error: eta-eval-in-thread: source must be a string"))

  (let ([tokens (tokenize source)])
    (if (EtaError? tokens)
        (EvalResult #f tokens)
        (let ([parsed-result (desugar (parse tokens))])
          (if (EtaError? parsed-result)
              (EvalResult #f parsed-result)
              ;; Evaluate in thread with proper break state management
              (with-clean-break-state 
               (lambda () (eval-in-thread parsed-result env))))))))

; eval-in-thread
;   Evaluates expressions in a separate thread with interruption support.
; Arguments:
;   expr-list - The expressions to evaluate
;   env - The environment in which to evaluate
; Returns:
;   An EvalResult containing either the result or an interruption error
(define (eval-in-thread expr-list env)
  (define evaluation-channel (make-channel))
  (define evaluation-custodian (make-custodian))
  
  (define evaluation-thread
    (parameterize ([current-custodian evaluation-custodian])
      (thread
       (lambda ()
         (with-handlers ([exn:break? 
                          (lambda (exn) 
                            (channel-put evaluation-channel 
                                         (EvalResult #f (make-evaluation-interrupted-error))))])
           (define result (eval-toplevel-exprs expr-list env))
           (if (RuntimeError? result)
               (channel-put evaluation-channel (EvalResult #f result))
               (channel-put evaluation-channel (EvalResult #t result))))))))
  
  (define (handle-ctrl-c)
    (with-handlers ([exn? (lambda (e) 
                            (printf "Debug: Exception in interrupt handler: ~a\n" (exn-message e)))])
      (break-thread evaluation-thread)
      (custodian-shutdown-all evaluation-custodian))
    (EvalResult #f (make-evaluation-interrupted-error)))
  
  (define result
    (with-handlers ([exn:break? (lambda (exn) (handle-ctrl-c))])
      (sync/timeout 
       #f  ;; Wait indefinitely
       (handle-evt evaluation-channel identity))))
  
  ;; Ensure resources are cleaned up properly
  (with-handlers ([exn? (lambda (e) (void))])
    (when (thread-running? evaluation-thread)
      (kill-thread evaluation-thread))
    (custodian-shutdown-all evaluation-custodian))
  
  result)

; reset-break-handler
;   Resets the break handler to ensure a clean state.
; Arguments:
;   None
; Returns:
;   None
(define (reset-break-handler)
  (break-enabled #f)
  (break-enabled #t)
  (void))

; with-clean-break-state
;   Executes a thunk with a clean break state.
;   Ensures break handling is reset before and after execution.
; Arguments:
;   thunk - The thunk to execute
; Returns:
;   The result of the thunk
(define (with-clean-break-state thunk)
  (reset-break-handler)
  (dynamic-wind
    (lambda () (void))
    thunk
    reset-break-handler))

