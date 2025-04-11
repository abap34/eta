#lang racket

(require racket/string)


(provide Expr
         print-Expr
         pretty-print-Expr
         Expr-head
         Expr-args
         astext)

(struct Expr (head args) #:transparent)

(define (make-Expr head args)
  (Expr head args))

(define (print-Expr expr)
  (format "(~a ~a)"
          (symbol->string (Expr-head expr))
          (string-join (map print-Expr (Expr-args expr)) " ")))

(define (pretty-print-Expr expr [indent 0])
  (define (indent-str n)
    (make-string (* n 2) #\space))

  (define (pp-args args [current-indent (add1 indent)])
    (define args-indent-str (indent-str current-indent))
    (cond
      [(null? args) ""]
      [(= (length args) 1)
       (string-append " " (pretty-print-Expr (car args) current-indent))]
      [else
       (string-append
        "\n"
        (string-join
         (map (λ(arg)
                (string-append args-indent-str
                               (pretty-print-Expr arg current-indent)))
              args)
         "\n"))]))

  (define head (Expr-head expr))
  (define args (Expr-args expr))
  (cond
    [(eq? head 'id)
     (format "(id ~a)" (car args))]

    [(eq? head 'const)
     (define val (car args))
     (cond
       [(or (number? val) (boolean? val))
        (format "(const ~a)" val)]
       [(string? val)
        (format "(const \"~a\")" val)]
       [(null? val)
        "(const ())"]
       [else
        (format "(const ~a)" val)])]

    [(eq? head 'quote)
     (format "(quote ~a)" (pretty-print-Expr (car args) (add1 indent)))]

    [(eq? head 'args)
     (format "(args ~a)"
             (string-join (map symbol->string args) " "))]

    [else
     (format "(~a~a)" (symbol->string head) (pp-args args))]))

;  astext
;     Convert an AST node to text representation based on its position information.
;  Arguments:
;      expr - The AST node to convert
;      source - The original source text (as a string or list of lines)
;  Returns:
;      String representation of the AST node that matches its original position in the source
;  Example:
;      (astext (parse "(define x 42)") source-text) ; => "(define x 42)"
;  Notes:
;      This function relies on accurate position information in the AST nodes.
;      If the AST node doesn't have position information, it falls back to a simple text representation.
(define (astext expr [source #f])
  ;; Convert source to lines if it's provided as a string
  (define lines
    (cond
      [(string? source) (string-split source "\n")]
      [(list? source) source]
      [else #f]))
  
  ;; Extract position information from the node if available
  (define start-line (if (has-position? expr) (get-start-line expr) #f))
  (define start-col (if (has-position? expr) (get-start-col expr) #f))
  (define end-line (if (has-position? expr) (get-end-line expr) #f))
  (define end-col (if (has-position? expr) (get-end-col expr) #f))
  
  ;; If position info and source are available, extract the text directly
  (if (and start-line start-col end-line end-col lines)
      (extract-text-from-position lines start-line start-col end-line end-col)
      ;; Otherwise, fall back to a simple representation
      (fallback-astext expr)))

;; Helper function to check if a node has position information
(define (has-position? expr)
  (and (struct? expr)
       (let ([fields (struct->vector expr)])
         (and (>= (vector-length fields) 5)
              (number? (vector-ref fields 1)) ; start-line
              (number? (vector-ref fields 2)) ; start-col
              (number? (vector-ref fields 3)) ; end-line
              (number? (vector-ref fields 4)))))) ; end-col

;; Extract position information from a node
(define (get-start-line expr) (vector-ref (struct->vector expr) 1))
(define (get-start-col expr) (vector-ref (struct->vector expr) 2))
(define (get-end-line expr) (vector-ref (struct->vector expr) 3))
(define (get-end-col expr) (vector-ref (struct->vector expr) 4))

;; Extract text from the given position in the source
(define (extract-text-from-position lines start-line start-col end-line end-col)
  (if (= start-line end-line)
      ;; Single line case
      (substring (list-ref lines (- start-line 1))
                (- start-col 1)
                (- end-col 1))
      ;; Multi-line case
      (string-append
       ;; First line (from start-col to end of line)
       (substring (list-ref lines (- start-line 1))
                 (- start-col 1))
       "\n"
       ;; Middle lines (complete lines)
       (string-join
        (for/list ([i (in-range (+ start-line 1) end-line)])
          (list-ref lines (- i 1)))
        "\n")
       ;; Last line (from start of line to end-col)
       (if (> end-line start-line)
           (string-append
            "\n"
            (substring (list-ref lines (- end-line 1))
                      0
                      (- end-col 1)))
           ""))))

;; Fallback text representation if position info is unavailable
(define (fallback-astext expr)
  (cond
    [(not (struct? expr)) (format "~a" expr)]
    [(eq? (Expr-head expr) 'id) (symbol->string (car (Expr-args expr)))]
    [(eq? (Expr-head expr) 'const)
     (let ([val (car (Expr-args expr))])
       (cond
         [(string? val) (format "\"~a\"" val)]
         [(null? val) "()"]
         [else (format "~a" val)]))]
    [(eq? (Expr-head expr) 'quote)
     (string-append "'" (fallback-astext (car (Expr-args expr))))]
    [else
     (string-append
      "("
      (symbol->string (Expr-head expr))
      (if (null? (Expr-args expr))
          ""
          (string-append " " 
                        (string-join
                         (map fallback-astext (Expr-args expr))
                         " ")))
      ")")]))
