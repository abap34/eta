#lang racket

(require "eta/repl/repl.rkt")

(define (print-help)
  (displayln "eta - a toy Scheme interpreter\n")
  (displayln "Usage:")
  (displayln "  eta                    ; Start REPL")
  (displayln "  eta <file>.scm         ; Run Scheme file")
  (displayln "  eta --help             ; Show this help")
  (displayln "  eta --version          ; Show version info"))

(define (print-version)
  (displayln "eta version 0.0.1"))

(define args (current-command-line-arguments))

(cond
  [(= (vector-length args) 0) (init-repl)]
  [(equal? (vector-ref args 0) "--help") (print-help)]
  [(equal? (vector-ref args 0) "--version") (print-version)]
  [(equal? (vector-ref args 0) "--script") (displayln "eta --script is not implemented yet")]
  [else (displayln "Unknown command")])

(define (main)
  (cond
    [(= (vector-length args) 0) (init-repl)]
    [(equal? (vector-ref args 0) "--help") (print-help)]
    [(equal? (vector-ref args 0) "--version") (print-version)]
    [(equal? (vector-ref args 0) "--script") (displayln "eta --script is not implemented yet")]
    [else (displayln "Unknown command")]))

(main)
