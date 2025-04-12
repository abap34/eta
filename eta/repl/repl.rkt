#lang racket

(provide init-repl initialize-env)

(require "../eval/eval.rkt"
         "../eval/env.rkt"
         "../utils/error.rkt"
         "../utils/location.rkt"
         "../utils/console.rkt"
         "../parser/parser.rkt"
         "../parser/ast.rkt"
         "../parser/tokenizer.rkt")

(define (banner)
  (display (bold "✨ Welcome to "))
  (display (bold (colorize "eta" 'blue)))
  (displayln (bold " REPL! ✨")))

(define (prompt)
  (display (colorize "eta> " 'green)))

;  initialize-env
;     Sets up the global environment with built-in functions and values.
;  Arguments:
;      env - The environment to initialize.
;  Returns:
;      The initialized environment.
(define (initialize-env env)
  (env-set! env "+" '+)
  (env-set! env "-" '-)
  (env-set! env "*" '*)
  (env-set! env "/" '/)
  env)

;  repl-loop
;     Simple Read-Eval-Print Loop for eta.
;  Arguments:
;      env - The environment in which to evaluate expressions.
;  Returns:
;      Never returns (loops indefinitely).
(define (repl-loop env)
  (prompt)
  (let ([input (read-line)])
    (cond
      [(eof-object? input)
       (displayln (colorize "\nGoodbye!" 'yellow))
       (exit)]
      [else
       (let* ([tokens (tokenize input)]
              [parsed-result (parse tokens)])
         (if (EtaError? parsed-result)
             ;; Handle parsing error
             (displayln 
                         (format-error-with-source parsed-result input)
                         )
             ;; Evaluate expression
             (let ([eval-result (eta-eval parsed-result env)])
               (if (EtaError? eval-result)
                   ;; Handle evaluation error
                   (displayln  
                              (format-error-with-source eval-result input)
                              )
                   ;; Show result
                   (displayln (colorize (format "=> ~a" eval-result) 'cyan))))))
         (repl-loop env)])))

;  init-repl
;     Initializes and starts the eta REPL.
;  Arguments:
;      None
;  Returns:
;      Never returns (starts the REPL loop).
(define (init-repl)
  (let ([global-env (make-env #f)])
    (banner)
    (initialize-env global-env)
    (repl-loop global-env)))
