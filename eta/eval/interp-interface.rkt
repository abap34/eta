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
;               if success? is #t, this is the evaluated value. (RuntimeValue | list of RuntimeValue)
;               if success? is #f, this is an error. (EtaError)
(struct EvalResult (success? value) #:transparent)

; format-eval-result
;   Converts an EvalResult to a string representation
; Arguments:
;   result - An EvalResult to convert.
;   [source-getter] - Optional function to get source code from a file identifier.
; Returns:
;   A string representation of the EvalResult.
(define (format-eval-result result [source-getter #f])
  ;; Recursively flatten and colorize all values
  (define (flatten-and-format val)
    (cond
      [(list? val) (apply append (map flatten-and-format val))]
      [else (list (colorize (runtime-value->string val) 'cyan))]))

  (if (EvalResult-success? result)
      (string-join (flatten-and-format (EvalResult-value result)) "\n")
      (let ([error-value (EvalResult-value result)])
        (if (EvaluationInterruptedError? error-value)
            (string-append (colorize "Evaluation interrupted by user" 'yellow) "\n")
            (colorize (format-error-with-source error-value source-getter) 'red)))))


; exit-with-eval-result
;   Exits the program with the given EvalResult.
; Arguments:
;   result - An EvalResult to exit with.
;   [source-getter] - Optional function to get source code from a file identifier.
; Returns:
;   Never returns (exits the program).
(define (exit-with-eval-result result [source-getter #f])
  (if (EvalResult-success? result)
    (exit 0)
    (begin
      (let ([error-value (EvalResult-value result)])
        (if (EvaluationInterruptedError? error-value)
            (begin
              (displayln (colorize "Evaluation interrupted by user" 'yellow))
              (exit 130)) 
            (begin
              (displayln (format-eval-result result source-getter))
              (exit 1))))))) 

; init-basic-env
;   Initializes the basic environment for the eta interpreter.
;   Environment includes builtin functions and standard library.
; Arguments:
;   None
; Returns:
;   An environment with builtin functions, standard library, and variables.
(define (init-basic-env)
  (let* ([env (add-builtins-to-env (init-toplevel-env))]
         [stdlib-dir (build-path (current-directory) "eta" "stdlib")]
         [list-lib-path (build-path stdlib-dir "list.eta")]
         [math-lib-path (build-path stdlib-dir "math.eta")]
         [higher-order-lib-path (build-path stdlib-dir "higher-order.eta")]
         [vector-lib-path (build-path stdlib-dir "vector.eta")])
    
    ;; Load standard library modules in order (dependencies first)
    (define (load-library-file env file-path)
      (if (file-exists? file-path)
          (let* ([content (file->string file-path)]
                 [result (eta-eval env content (path->string file-path))])
            (if (EvalResult-success? result)
                env
                (begin
                  (displayln (format "Warning: Error loading standard library file: ~a" 
                                    (path->string file-path)))
                  (displayln (format "  Error: ~a" 
                                    (EvalResult-value result)))
                  env)))
          (begin
            (displayln (format "Warning: Standard library file not found: ~a" 
                              (path->string file-path)))
            env)))
    
    ;; Load modules in order (list, math, string, higher-order)
    (let* ([env (load-library-file env list-lib-path)]
           [env (load-library-file env math-lib-path)]
           [env (load-library-file env higher-order-lib-path)]
           [env (load-library-file env vector-lib-path)])
      env)))


; eta-eval
;   Evaluates an expression in the given environment.
; Arguments:
;   env - The environment in which to evaluate the expression.
;   source - The source code to evaluate. (string)
; Returns:
;  EvalResult containing success status and evaluated value.
(define (eta-eval env source [file #f])
   (unless (string? source)
     (error "eta-eval: source must be a string"))

  (let ([tokens (tokenize source file)])
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
;    file - Optional file name or source identifier. (string, symbol, or #f)
; Returns:
;    EvalResult containing success status and evaluated value.
(define (eta-eval-toplevel source [file #f])
  (let ([global-env (init-basic-env)])
    (eta-eval global-env source file)))

; eta-eval-toplevel-in-thread
;    Evaluates a script in new toplevel environment with interruption support.
; Arguments:
;    source - The source code to evaluate. (string)
;    file - Optional file name or source identifier. (string, symbol, or #f)
; Returns:
;    EvalResult containing success status and evaluated value.
(define (eta-eval-toplevel-in-thread source [file #f])
  (let ([global-env (init-basic-env)])
    (eta-eval-in-thread global-env source file)))

; eta-eval-in-thread
;   Evaluates an expression in a separate thread with interruption support.
; Arguments:
;   env - The environment in which to evaluate the expression.
;   source - The source code to evaluate. (string)
;   file - Optional file name or source identifier. (string, symbol, or #f)
; Returns:
;  EvalResult containing success status and evaluated value.
(define (eta-eval-in-thread env source [file 'repl])
  (unless (string? source)
    (error "Internal error: eta-eval-in-thread: source must be a string"))

  (let ([tokens (tokenize source file)])
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
