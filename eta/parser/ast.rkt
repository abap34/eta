#lang racket

(provide Expr string-join print-Expr pretty-print-Expr make-Expr Expr-head Expr-args)

(struct Expr (head args) #:transparent)

(define (string-join lst sep)
  (if (null? lst)
    ""
    (let loop
      ((lst lst) (acc ""))
      (if (null? (cdr lst))
        (string-append acc (car lst))
        (loop (cdr lst) (string-append acc (car lst) sep))
      )
    )
  )
)

(define (make-Expr head args)
  (Expr head args)
)

(define (print-Expr expr)
  (let ((head (Expr-head expr)) (args (Expr-args expr)))
    (string-append "(" (symbol->string head) " " (string-join (map print-Expr args)
     " ") ")")
  )
)

;  pretty-print-Expr
;     Pretty prints an expression AST with proper indentation and formatting.
;  Arguments:
;      expr - The expression to print.
;      (indent) - Current indentation level (default 0).
;  Returns:
;      A formatted string representation of the expression.
;  Example:
;      (pretty-print-Expr (make-Expr 'define (list (make-Expr 'id (list 'x)) (make-Expr 'const (list 42)))))
;      ; => "(define
;      ;       (id x)
;      ;       (const 42))"
(define (pretty-print-Expr expr (indent 0))
  (define indent-str
    (make-string (* indent 2) #\space)
  )

  (define (pp-args args (current-indent (+ indent 1)))
    (define args-indent-str
      (make-string (* current-indent 2) #\space)
    )
    (cond ((null? args) "")
      ((= (length args) 1) (string-append " " (pretty-print-Expr (car args) current-indent)))
      (else
        (string-append
          "\n"
          (string-join (map (λ (arg) (string-append args-indent-str (pretty-print-Expr
           arg current-indent))) args) "\n")
        )
      )
    )
  )

  (let ((head (Expr-head expr)) (args (Expr-args expr)))
    (cond ((eq? head 'id) (format "(id ~a)" (car args)))
      ((eq? head 'const)
        (cond ((or (number? (car args)) (boolean? (car args))) (format "(const ~a)"
         (car args)))
          ((string? (car args)) (format "(const \"~a\")" (car args)))
          ((null? (car args)) "(const ())")
          (else (format "(const ~a)" (car args)))
        )
      )

      ((eq? head 'quote) (format "(quote ~a)" (pretty-print-Expr (car args) (+ indent
       1))))

      ((eq? head 'args) (format "(args ~a)" (string-join (map symbol->string args)
       " ")))

      (else (format "(~a~a)" (symbol->string head) (pp-args args)))
    )
  )
)
