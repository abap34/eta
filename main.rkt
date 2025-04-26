#lang racket

(require "eta/eval/interp-interface.rkt"
         "eta/repl/repl.rkt"
         racket/file)


      
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

; read-file
;    Reads a file and returns its content as a string.
;  Arguments:
;    filename - The name of the file to read.
;  Returns:
;    The content of the file as a string.
(define (read-file filename)
  (file->string filename))

; run-file
;    Runs a source file in the eta interpreter.
;  Arguments:
;    filename - The name of the file to run.
;  Returns:
;    None
(define (run-file filename)
  (let* ([content (read-file filename)]
         [result (eta-eval-toplevel content)])
    (exit-with-eval-result result content)))

(define (main)
  (cond
    [(= (vector-length args) 0) (init-repl)]
    [(equal? (vector-ref args 0) "--help") (print-help)]
    [(equal? (vector-ref args 0) "--version") (print-version)]
    [(equal? (vector-ref args 0) "--script") 
          (if (equal? (vector-length args) 2)
              (let ([filename (vector-ref args 1)])
                    (run-file filename))
              (displayln "Usage: eta --script <file>.scm"))] 
    [else (displayln "Unknown command")]))


(main)
