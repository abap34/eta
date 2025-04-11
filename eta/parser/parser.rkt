#lang racket

(require "tokenizer.rkt") 

(provide parse parse-exp)

(struct Expr (head args loc) #:transparent)

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
    [_ 'EOF]))

(define (parse-const tokens)
  (match tokens
    [(cons (Token typ val loc) rest)
     (cond
       [(member typ '(Num Bool String))
        (values (Expr 'const (list val) loc) rest)]
       [else
        (error "Expected constant")])]))

(define (parse-var tokens)
  (match tokens
    [(cons (Token 'Id name loc) rest)
     (values (Expr 'var (list name) loc) rest)]
    [_ (error "Expected identifier")]))

(define (parse-quote tokens)
  (match tokens
    [(cons (Token 'Quote _ loc) rest)
     (define-values (quoted-expr rest2) (parse-exp rest))
     (values (Expr 'quote (list quoted-expr) loc) rest2)]
    [_ (error "Expected quote")]))

(define (parse-paren-exp tokens)
  (match tokens
    [(cons (Token 'LParen _ loc) rest)
     (match rest
       [(cons (Token typ val _) after)
        (cond
          [(and (eq? typ 'Keyword) (equal? val "quote"))
           (define-values (e1 rest1) (parse-exp after))
           (define-values (_ __ rest2) (expect-token-type 'RParen rest1))
           (values (Expr 'quote (list e1) loc) rest2)]

          [else
           ;; General application
           (define-values (f rest1) (parse-exp rest))
           (define-values (args rest2) (parse-expr-list rest1))
           (define-values (_ __ rest3) (expect-token-type 'RParen rest2))
           (values (Expr 'app (cons f args) loc) rest3)])]
       [_ (error "Unexpected form in paren expression")])]
    [_ (error "Expected left paren")]))

(define (parse-expr-list tokens)
  (define (loop ts acc)
    (match (peek-type ts)
      ['RParen (values (reverse acc) ts)]
      ['EOF (error "Unexpected end in list")]
      ))
  (loop tokens '()))

(define (parse-exp tokens)
  (match (peek-type tokens)
    [(or 'Num 'Bool 'String) (parse-const tokens)]
    ['Id                    (parse-var tokens)]
    ['Quote                 (parse-quote tokens)]
    ['LParen                (parse-paren-exp tokens)]
    [_                      (error "Unexpected token in expression")]))

(define (parse tokens)
  (define-values (expr rest) (parse-exp tokens))
  (if (null? rest)
      expr
      (error "Extra tokens after parse")))
