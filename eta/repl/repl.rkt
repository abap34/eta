#lang racket

(provide init-repl)

(require "../utils/error.rkt"
         "../utils/console.rkt"
         "../eval/interp-interface.rkt")
  

(define (banner)
  (display (bold "✨ Welcome to "))
  (display (bold (colorize "eta" 'blue)))
  (displayln (bold " REPL! ✨")))

(define (prompt)
  (display (colorize "eta> " 'green)))


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
       (with-clean-break-state
        (lambda ()
          (with-handlers ([exn:break? (lambda (e)
                                        (displayln (colorize "Evaluation interrupted." 'yellow)))])
            (displayln (format-eval-result (eta-eval-in-thread env input 'repl))))))])
    ;; Always reset break state before next loop iteration
    (reset-break-handler)
    (repl-loop env)))
                    
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
