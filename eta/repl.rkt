#lang racket

(provide init-repl)

(require "eval.rkt"
         "utils/console.rkt"
         "parser/parser.rkt"
         "parser/ast.rkt")

(define (banner)
  (display (bold "✨ Welcome to "))
  (display (bold (colorize "eta" 'blue)))
  (displayln (bold " REPL! ✨")))

(define (prompt)
  (display (colorize "eta> " 'green)))

;; repl-loop
;;   The main Read-Eval-Print Loop for eta.
;;   Reads user input, parses it to AST, evaluates it, and prints the result.
;; Arguments:
;;   None
;; Returns:
;;   Never returns (loops indefinitely).
;; Example:
;;   (repl-loop) ; starts the REPL
(define (repl-loop)
  (prompt)
  (let ([input (read-line)])
    (cond
      [(eof-object? input)
       (displayln (colorize "\n👋 Bye!" 'yellow))
       (exit)]

      [(string=? input ":ast")
       (displayln "Current AST Mode: Debug")
       (repl-loop)]

      [else
       (let ([ast (with-handlers ([exn:fail? (lambda (e)
                                               (displayln (colorize (format "Parse error: ~a"
                                                                            (exn-message e))
                                                                    'red))
                                               (repl-loop))])
                    (parse input))])
         (displayln (bold "Parsed AST:"))
         (displayln (colorize (pretty-print-Expr ast) 'green))

         (let ([result
                (with-handlers ([exn:fail? (lambda (e)
                                             (displayln (colorize (format "Evaluation error: ~a"
                                                                          (exn-message e))
                                                                  'red))
                                             #f)])
                  (eta-eval ast))])
           (when result
             (displayln (bold "=> "))
             (displayln (colorize (pretty-print-Expr result) 'cyan))))

         (repl-loop))])))

;; init-repl
;;   Initializes and starts the eta REPL.
;; Arguments:
;;   None
;; Returns:
;;   Never returns (starts the REPL loop).
;; Example:
;;   (init-repl) ; initializes and starts the REPL
(define (init-repl)
  (banner)
  (repl-loop))
