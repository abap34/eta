(import (rnrs) (eta repl))

(define args
  (command-line)
)

(define (print-help)
  (display "eta - a toy Scheme interpreter\n\n")
  (display "Usage:\n")
  (display "  eta                    ; Start REPL\n")
  (display "  eta <file>.scm         ; Run Scheme file\n")
  (display "  eta --help             ; Show this help\n")
  (display "  eta --version          ; Show version info\n")
)

(define (print-version)
  (display "eta version 0.0.1\n")
)

(cond ((null? (cdr args)) (init-repl))
  ((equal? (cadr args) "--help") (print-help))
  ((equal? (cadr args) "--version") (print-version))
  ((equal? (cadr args) "--script") (display "eta --script is not implemented yet\n"))
  (else (display "Unknown command\n"))
)
