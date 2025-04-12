#lang racket

(require "tokenizer.rkt")
(require "ast.rkt")
(require "../utils/error.rkt")
(require "../utils/location.rkt")

(provide parse parse-exp EtaError?)

;  get-last-token-location
;     Gets the location of the last token in a token list,
;     or creates a default location if the list is empty
;  Arguments:
;      tokens - A list of tokens
;  Returns:
;      A Location object, either from the last token or a default (1,1,1,1) location
(define (get-last-token-location tokens)
  (match tokens
    [(cons (Token _ _ loc) rest)
     (if (null? rest)
         loc
         (get-last-token-location rest))]
    [_ (Location 1 1 1 1)]))  ; Default location if no tokens

;  infer-error-location
;     Tries to infer a reasonable error location based on context
;  Arguments:
;      tokens - The tokens being parsed
;      default-line - Default line number if can't be inferred (default: 1)
;      default-col - Default column number if can't be inferred (default: 1)
;  Returns:
;      A Location object with the best guess at error position
(define (infer-error-location tokens [default-line 1] [default-col 1])
  (match tokens
    [(cons (Token _ _ loc) _) loc]
    [(list) (Location default-line default-col default-line default-col)]
    [_ (if (pair? tokens)
           (get-last-token-location tokens)
           (Location default-line default-col default-line default-col))]))

(define (expect-token-type expected-type tokens)
  (match tokens
    [(cons (Token typ val loc) rest)
     (if (eq? typ expected-type)
         (values val loc rest)
         (values (make-parse-error 
                  (format "Expected ~a but got ~a" expected-type typ) 
                  loc )
                 #f
                 tokens))]
    [_ (values (make-parse-error 
                "Unexpected end of tokens" 
                (infer-error-location tokens))
               #f
               tokens)]))

(define (peek-type tokens)
  (match tokens
    [(cons (Token typ _ _) _) typ]
    [_ EOF]))

(define (parse-const tokens)
  (match tokens
    [(cons (Token typ val loc) rest)
     (cond
       [(eq? typ Num)
        (values (make-expr Const (list (string->number val)) loc) rest)]
       [(eq? typ Bool)
        (values (make-expr Const (list (equal? val "#t")) loc) rest)]
       [(eq? typ String)
        (values (make-expr Const (list val) loc) rest)]
       [else
        (values (make-parse-error "Expected constant" loc)
                rest)])]))

(define (parse-var tokens)
  (match tokens
    [(cons (Token typ name loc) rest)
     #:when (eq? typ Id)
     (values (make-expr Var (list name) loc) rest)]
    [(cons (Token typ _ loc) _)
     (values (make-parse-error "Expected identifier" loc `(got ,typ))
             tokens)]
    [_ (values (make-parse-error "Expected identifier" 
                                (infer-error-location tokens)
                                )
               tokens)]))

(define (parse-quote tokens)
  (match tokens
    [(cons (Token typ _ loc) rest)
     #:when (eq? typ QuoteSym)
     (define-values (quoted-expr rest2) (parse-exp rest))
     (if (EtaError? quoted-expr)
         (values quoted-expr rest2)
         (values (make-expr Quote (list quoted-expr) loc) rest2))]
    [(cons (Token typ _ loc) _)
     (values (make-parse-error "Expected quote" loc )
             tokens)]
    [_ (values (make-parse-error "Expected quote" 
                               (infer-error-location tokens)
                               )
               tokens)]))

;  create-span-location
;     Creates a new location that spans from the start location to the end location
;  Arguments:
;      start-loc - The starting location
;      end-loc - The ending location
;  Returns:
;      A new Location that spans from the start to the end
(define (create-span-location start-loc end-loc)
  (Location (Location-sline start-loc) 
            (Location-scol start-loc)
            (Location-eline end-loc)
            (Location-ecol end-loc)))

;  handle-parse-result
;     Processes the result of a parse operation and handles error checking
;  Arguments:
;      result - The result of a parse operation
;      rest - The remaining tokens
;      success-fn - Function to call on success with (result, rest) as arguments
;  Returns:
;      The values returned by success-fn on success, or the error and rest on failure
(define (handle-parse-result result rest success-fn)
  (if (EtaError? result)
      (values result rest)
      (success-fn result rest)))

;  parse-and-expect-rparen
;     Parses an expression and then expects a right parenthesis
;  Arguments:
;      tokens - The tokens to parse
;      start-loc - The location of the opening parenthesis
;      make-result-fn - Function to create the final expression from the parsed result
;  Returns:
;      Values with the created expression and the remaining tokens, or an error
(define (parse-and-expect-rparen tokens start-loc make-result-fn)
  (define-values (expr rest1) (parse-exp tokens))
  (handle-parse-result expr rest1
    (lambda (expr rest1)
      (define-values (rparen-val rparen-loc rest2) (expect-token-type RParen rest1))
      (handle-parse-result rparen-val rest2
        (lambda (_1 _2)
          (values (make-result-fn expr (create-span-location start-loc rparen-loc)) rest2))))))

(define (parse-paren-exp tokens)
  (match tokens
    [(cons (Token typ _ loc) rest)
     #:when (eq? typ LParen)
     (match rest
       [(cons (Token kw-typ val _) after)
        (cond
          [(and (eq? kw-typ Keyword) (equal? val "quote"))
           (parse-and-expect-rparen after loc
             (lambda (expr span-loc)
               (make-expr Quote (list expr) span-loc)))]

          [else
           (define-values (f rest1) (parse-exp rest))
           (handle-parse-result f rest1
             (lambda (f rest1)
               (define-values (args rest2) (parse-expr-list rest1))
               (handle-parse-result args rest2
                 (lambda (args rest2)
                   (define-values (rparen-val rparen-loc rest3) (expect-token-type RParen rest2))
                   (handle-parse-result rparen-val rest3
                     (lambda (_1 _2)
                       (values (make-expr App (cons f args) (create-span-location loc rparen-loc)) rest3)))))))])]
       [_ (values (make-parse-error "Unexpected form in paren expression" loc)
                  rest)])]
    [(cons (Token typ _ loc) _)
     (values (make-parse-error "Expected left paren" loc )
             tokens)]
    [_ (values (make-parse-error "Expected left paren" 
                              (infer-error-location tokens)
                              )
               tokens)]))

(define (parse-expr-list tokens)
  (define (loop ts acc)
    (match (peek-type ts)
      [typ #:when (eq? typ RParen) (values (reverse acc) ts)]
      [typ #:when (eq? typ EOF) 
       (values (make-parse-error "Unexpected end in list" 
                              (infer-error-location ts)
                              ) 
               ts)]
      [_ (define-values (expr rest) (parse-exp ts))
         (handle-parse-result expr rest
           (lambda (expr rest)
             (loop rest (cons expr acc))))]))
  (loop tokens '()))

(define (parse-exp tokens)
  (match (peek-type tokens)
    [typ #:when (or (eq? typ Num) 
                   (eq? typ Bool) 
                   (eq? typ String)) (parse-const tokens)]
    [typ #:when (eq? typ Id) (parse-var tokens)]
    [typ #:when (eq? typ QuoteSym) (parse-quote tokens)]
    [typ #:when (eq? typ LParen) (parse-paren-exp tokens)]
    [(cons (Token typ _ loc) _)
     (values (make-parse-error "Unexpected token in expression" loc)
             tokens)]
    [_ (values (make-parse-error "Unexpected token in expression" 
                              (infer-error-location tokens)
                              )
               tokens)]))

(define (parse tokens)
  (define-values (expr rest) (parse-exp tokens))
  (if (EtaError? expr)
      expr
      (if (null? rest)
          expr
          (make-parse-error "Extra tokens after parse" 
                         (infer-error-location rest)
                         ))))
