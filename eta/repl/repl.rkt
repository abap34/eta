#lang racket

(provide init-repl)

(require "../utils/error.rkt"
         "../utils/console.rkt"
         "../parser/parser.rkt"
         "../parser/tokenizer.rkt"
         "../eval/interp.rkt"
         "../eval/runtime-values.rkt"
         "../eval/env.rkt"
         "../eval/builtins.rkt")

  

(define (banner)
  (display (bold "✨ Welcome to "))
  (display (bold (colorize "eta" 'blue)))
  (displayln (bold " REPL! ✨")))

(define (prompt)
  (display (colorize "eta> " 'green)))


(define (init-basic-env)
  (add-builtins-to-env (init-toplevel-env)))

;  repl-loop
;     Simple Read-Eval-Print Loop for eta.
;     Currently only parses and pretty prints the result, without evaluation.
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
      [(string=? input "")
       (repl-loop env)]
      [else
       (let ([tokens (tokenize input)])
           (if (TokenizeError? tokens)
              (begin
                (displayln (format-error-with-source tokens input))
                (repl-loop env))
              (let ([parsed-result (parse tokens)])
                (if (ParseError? parsed-result)
                 (begin
                (displayln (format-error-with-source tokens input))
                (repl-loop env))
                (let ([result (eval env parsed-result)])
                  (if (RuntimeError? result)
                    (begin
                      (displayln (format-error-with-source result input))
                      (repl-loop env))
                    (begin
                      (displayln (colorize (format "=> ~a" (runtime-value->string result)) 'cyan))
                      (repl-loop env))))))))])))
                    
;  init-repl
;     Initializes and starts the eta REPL.
;  Arguments:
;      None
;  Returns:
;      Never returns (starts the REPL loop).
(define (init-repl)
  (let ([global-env (init-basic-env)])
    (banner)
    (repl-loop global-env)))
