#lang racket

(require "tokenizer.rkt")
(require "ast.rkt")

(provide parse parse-exp)

(define (expect-token-type expected-type tokens)
  (match tokens
    [(cons (Token typ val loc) rest)
     (if (eq? typ expected-type)
         (values val loc rest)
         (error (format "Expected ~a but got ~a" expected-type typ)))]
    [_ (error "Unexpected end of tokens")]))

(define (peek-type tokens)
  (match tokens
    [(cons (Token typ _ _) _) typ]
    [_ EOF]))

(define (parse-const tokens)
  (match tokens
    [(cons (Token typ val loc) rest)
     (cond
       [(or (eq? typ Num) (eq? typ Bool) (eq? typ String))
        (values (make-expr Const (list val) loc) rest)]
       [else
        (error "Expected constant")])]))

(define (parse-var tokens)
  (match tokens
    [(cons (Token typ name loc) rest)
     #:when (eq? typ Id)
     (values (make-expr Var (list name) loc) rest)]
    [_ (error "Expected identifier")]))

(define (parse-quote tokens)
  (match tokens
    [(cons (Token typ _ loc) rest)
     #:when (eq? typ QuoteSym)
     (define-values (quoted-expr rest2) (parse-exp rest))
     (values (make-expr Quote (list quoted-expr) loc) rest2)]
    [_ (error "Expected quote")]))

(define (parse-paren-exp tokens)
  (match tokens
    [(cons (Token typ _ loc) rest)
     #:when (eq? typ LParen)
     (match rest
       [(cons (Token kw-typ val _) after)
        (cond
          [(and (eq? kw-typ Keyword) (equal? val "quote"))
           (define-values (e1 rest1) (parse-exp after))
           (define-values (_ rparen-loc rest2) (expect-token-type RParen rest1))
           (define new-loc 
             (Location (Location-sline loc) 
                       (Location-scol loc)
                       (Location-eline rparen-loc)
                       (Location-ecol rparen-loc)))
           (values (make-expr Quote (list e1) new-loc) rest2)]

          [else
           (define-values (f rest1) (parse-exp rest))
           (define-values (args rest2) (parse-expr-list rest1))
           (define-values (_ rparen-loc rest3) (expect-token-type RParen rest2))
           (define new-loc 
             (Location (Location-sline loc) 
                       (Location-scol loc)
                       (Location-eline rparen-loc)
                       (Location-ecol rparen-loc)))
           (values (make-expr App (cons f args) new-loc) rest3)])]
       [_ (error "Unexpected form in paren expression")])]
    [_ (error "Expected left paren")]))

(define (parse-expr-list tokens)
  (define (loop ts acc)
    (match (peek-type ts)
      [typ #:when (eq? typ RParen) (values (reverse acc) ts)]
      [typ #:when (eq? typ EOF) (error "Unexpected end in list")]
      [_ (define-values (expr rest) (parse-exp ts))
         (loop rest (cons expr acc))]))
  (loop tokens '()))

(define (parse-exp tokens)
  (match (peek-type tokens)
    [typ #:when (or (eq? typ Num) 
                   (eq? typ Bool) 
                   (eq? typ String)) (parse-const tokens)]
    [typ #:when (eq? typ Id) (parse-var tokens)]
    [typ #:when (eq? typ QuoteSym) (parse-quote tokens)]
    [typ #:when (eq? typ LParen) (parse-paren-exp tokens)]
    [_ (error "Unexpected token in expression")]))

(define (parse tokens)
  (define-values (expr rest) (parse-exp tokens))
  (if (null? rest)
      expr
      (error "Extra tokens after parse")))
