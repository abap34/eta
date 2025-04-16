#lang racket

(provide init-repl)

(require "../utils/error.rkt"
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

;  get-parse-result
;     Helper function to extract the result from parser output
;  Arguments:
;     parser-result - The result from the parser (either a single Expr or a list containing one Expr)
;  Returns:
;     The Expr object (unwrapped from list if necessary)
(define (get-parse-result parser-result)
  (if (and (list? parser-result) (= (length parser-result) 1))
      (first parser-result)
      parser-result))

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
       (let* ([tokens (tokenize input)]
              [raw-result (parse tokens)]
              [result (get-parse-result raw-result)])
         
         (cond
           [(TokenizeError? result)
            (displayln (format-error-with-source result input))]
           [(ParseError? result)
            (displayln (format-error-with-source result input))]
           [(EtaError? result)
            (displayln (format-error-with-source result input))]
           [else
            (begin
              (displayln (colorize "Tokenized Input:" 'blue))
              (for-each (lambda (token)
                          (displayln (format-token token)))
                        tokens)
              (displayln (colorize "Parsed Result:" 'blue))
              (displayln (pretty-print-Expr result)))]))
       (repl-loop env)])))

;  init-repl
;     Initializes and starts the eta REPL.
;  Arguments:
;      None
;  Returns:
;      Never returns (starts the REPL loop).
(define (init-repl)
  (let ([global-env #f])    ; mock
    (banner)
    (repl-loop global-env)))
