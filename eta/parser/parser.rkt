#lang racket

(require "tokenizer.rkt"
         "ast.rkt"
         "../utils/error.rkt"
         "../utils/location.rkt")

(provide parse
         parse-as
        )


;  delay-parser
;     Creates a parser that delays evaluation of another parser until needed
;  Arguments:
;      parser-thunk - A thunk (function of no arguments) that returns a parser
;  Returns:
;      A parser that evaluates the thunk only when tokens need to be parsed
;  Example:
;      (delay-parser (lambda () parse-exp))
;  Notes:
;      Use this to break circular dependencies between parsers
(define (delay-parser parser-thunk)
  (lambda (tokens)
    ((parser-thunk) tokens)))

;  parser-ref
;     Creates a delayed reference to another parser function
;  Arguments:
;      parser-expr - Expression that evaluates to a parser
;  Returns:
;      A parser that evaluates parser-expr only when needed
;  Example:
;      (parser-ref parse-exp)
;  Notes:
;      Use this to break circular dependencies between parsers
(define-syntax-rule (parser-ref parser-expr)
  (delay-parser (lambda () parser-expr)))


; loc
;    Creates a location object for a
;      - token
;      - list of tokens
;      - expr
;      - list of exprs
;  Arguments:
;      arg - Token, list of tokens, Expr, or list of Expr
;  Returns:
;      A Location object representing the location of the argument
(define (loc arg)
  (cond
    [(Token? arg) (Token-loc arg)]
    [(list? arg)
     (if (empty? arg)
         #f
         (create-span-location
          (loc (first arg))
          (loc (last arg))))]
    [(Expr? arg) (Expr-loc arg)]
    [else #f]))

; make-parser-error
;     Creates a parse error with a message and location
; Arguments:
;     msg - Error message
;     location - Location of the error
; Returns:
;     A ParseError object
(define (make-parser-error msg location)
  (if location
      (EtaError ParseError msg location)
      (EtaError ParseError msg #f)))


;  token-error
;     Creates a specific parser error for an expected token type.
;  Arguments:
;     expected - String describing what was expected
;     token    - The actual token found (or #f if no token available)
;  Returns:
;     An EtaError structure
(define (token-error expected token)
  (if token
      (make-parser-error (string-append "Expected " expected ", got " 
                                  (format-token token))
                   (Token-loc token))
      (make-parser-error (string-append "Expected " expected ", but no token found")
                   #f)))


;; ---------- Basic Parser Combinators ----------


;  try-parser
;     Attempts to apply a parser, returning its result whether success or failure
;  Arguments:
;      parser - Parser function to try
;      tokens - Tokens to parse
;  Returns:
;      The result of the parser (either success pair or error)
(define (try-parser parser tokens)
  (parser tokens))

; logging
;     Logs message when a parser is applied and return failure
;  Arguments:
;      msg - Message to log
;  Returns:
;       Always return (parser-error) with the empty message
(define (logging msg)
  (lambda (tokens)
    (display (format "Log: ~a\n" msg))
    (make-parser-error "" (tokens-span tokens))))


; assert-all-producers
;     Asserts that all parsers in a list are valid parser functions
;  Arguments:
;      parsers - List of parsers to check
;  Returns:
;      None if all parsers are valid
;  Raises:
;      exn:fail - If any parser is not a valid function
(define (assert-all-producers parsers)
  (for-each (lambda (p)
            (unless (procedure? p)
              (error 'assert-all-producers "Expected a procedure but found: ~s" p)))
          parsers))

;  any-of
;     Tries each parser in order, returning the result of the first successful parser
;     or the error from the parser that progressed furthest in the input.
;  Arguments:
;      parsers - List of parsers to try
;  Returns:
;      A parser that returns the result of the first successful parser
;      or the furthest error if all parsers fail
;  Notes:
;      The "furthest error" is determined by which parser consumed
;      the most tokens based on error location.
(define (any-of . parsers)
  ; assert parser is a list of procedures
  (assert-all-producers parsers)
  (lambda (tokens)
    (let loop ([ps parsers] [ts tokens] [best-error #f])
      (if (empty? ps)
          (or best-error (make-parser-error "No parsers succeeded" (tokens-span tokens)))
          (let* ([parser (first ps)]
                 [result (parser ts)])
            (if (EtaError? result)
                (let* ([current-loc (EtaError-location result)]
                       [best-loc (and best-error (EtaError-location best-error))]
                       [new-best-error
                        (cond
                          [(not best-error) result]
                          [(not current-loc) best-error]
                          [(not best-loc) result]
                          [(location<? best-loc current-loc) result]
                          [else best-error])])
                  ;; Continue with next parser, keeping track of the furthest error
                  (loop (rest ps) ts new-best-error))
                ;; Success! Return the result
                result))))))




;  sequence
;     Applies a sequence of parsers in order, collecting their results
;  Arguments:
;      parsers - List of parser functions to apply in sequence
;      [error-handler] - Optional function to handle errors (default: return first error)
;  Returns:
;      A new parser that returns a list of all parser results or appropriate error
(define (sequence . parsers)
  (assert-all-producers parsers)
  (lambda (tokens)
    (let loop ([ps parsers] [ts tokens] [results '()])
      (if (empty? ps)
          (cons (reverse results) ts)
          (let ([parser (first ps)])
            (let ([result (parser ts)])
              (if (EtaError? result)
                  result  ; Return the error
                  (match result
                    [(cons r rest-tokens)
                     (loop (rest ps) rest-tokens (cons r results))]))))))))

;  label
;     Wraps a parser with a descriptive label for better error messages
;  Arguments:
;      desc - Description string for error messages
;      parser - Parser function to wrap
;  Returns:
;      A new parser with enhanced error messages
(define (label desc parser)
  ; validate that parser is procedure
  (unless (procedure? parser)
    (error (format "Parser is not a procedure. Got: ~a" parser)))
  (lambda (tokens)
    (let ([result (parser tokens)])
      (if (EtaError? result)
          (make-parser-error 
           (format "~a ─ ~a" desc (EtaError-message result))
           (EtaError-location result))
          result))))

;  map-parser
;     Transforms the result of a parser using a function
;  Arguments:
;      parser - Parser function to transform
;      f - Function to apply to parser result
;  Returns:
;      A new parser that applies f to the result of parser
(define (map-parser parser f)
  (lambda (tokens)
    (let ([result (parser tokens)])
      (if (EtaError? result)
          result
          (match result
            [(cons r rest-tokens)
             (cons (f r) rest-tokens)])))))

;  token-pred
;     Creates a parser that matches a token satisfying a predicate
;  Arguments:
;      pred - Predicate function applied to token
;      expected-desc - Description of expected token (for error messages)
;  Returns:
;      A parser function that matches tokens satisfying the predicate
(define (token-pred pred expected-desc)
  (lambda (tokens)
    (if (empty? tokens)
        (make-parser-error 
         (format "Expected ~a, got end of input" expected-desc)
         (tokens-span tokens))  ; Use tokens-span to get location at end of input
        (let ([token (first tokens)])
          (if (pred token)
              (cons token (rest tokens))
              (token-error expected-desc token))))))

;  token-type
;     Creates a parser that matches a token of a specific type
;  Arguments:
;      type - Token type to match
;  Returns:
;      A parser function that matches tokens of the given type
(define (token-type type)
  (token-pred (lambda (token) (eq? (Token-typ token) type))
              (format "token of type ~a" type)))


;  keyword
;     Creates a parser that matches a specific keyword
;  Arguments:
;      keyword - Keyword to match (as a string)
;  Returns:
;      A parser function that matches the keyword
 (define (keyword kw)
  (unless (eta-keyword? kw)
    (error 'keyword "Expected a keyword, but got: ~a" kw))

  (token-pred (lambda (token) 
                (and (eq? (Token-typ token) 'KeywordToken)
                     (equal? (Token-val token) kw)))
              (format "keyword ~a" kw)))

;  zero-or-more
;     Applies a parser zero or more times, collecting all results
;     and handling errors gracefully.
;  Arguments:
;      parser - Parser to apply repeatedly
;      [separator-parser] - Optional parser for separators between items
;  Returns:
;      A parser that returns a list of all results
(define (zero-or-more parser [separator-parser #f])
  (lambda (tokens)
    (let loop ([ts tokens] [results '()])
      (let ([result (parser ts)])
        (if (EtaError? result)
            (cons (reverse results) ts)  ; End on error without consuming tokens
            (match result
              [(cons parsed-val remaining-tokens)
               (if (equal? remaining-tokens ts)
                   ;; Parser didn't consume any tokens, prevent infinite loop
                   (cons (reverse (cons parsed-val results)) remaining-tokens)
                   (if separator-parser
                       ;; Try to parse a separator
                       (let ([sep-result (separator-parser remaining-tokens)])
                         (if (EtaError? sep-result)
                             ;; No separator found, we're done
                             (cons (reverse (cons parsed-val results)) remaining-tokens)
                             ;; Separator found, continue parsing
                             (loop (cdr sep-result) (cons parsed-val results))))
                       ;; No separator parser, continue
                       (loop remaining-tokens (cons parsed-val results))))]))))))

;  one-or-more
;     Applies a parser one or more times, collecting all results
;     and providing better error messages.
;  Arguments:
;      parser - Parser to apply repeatedly
;      [error-msg] - Optional error message when no matches are found
;      [separator-parser] - Optional parser for separators between items
;  Returns:
;      A parser that returns a list of all results (at least one)
(define (one-or-more parser [error-msg "Expected at least one item"] [separator-parser #f])
  (lambda (tokens)
    (let ([first-result (parser tokens)])
      (if (EtaError? first-result)
          (if (empty? tokens)
              (make-parser-error (string-append error-msg "; found end of input") #f)
              (make-parser-error (string-append error-msg "; found " 
                                          (if (empty? tokens) 
                                              "end of input"
                                              (format-token (first tokens))))
                           (if (empty? tokens) #f (Token-loc (first tokens)))))
          (match first-result
            [(cons first-val rest-tokens)
             (let ([rest-result ((zero-or-more parser separator-parser) rest-tokens)])
               (match rest-result
                 [(cons rest-vals final-tokens)
                  (cons (cons first-val rest-vals) final-tokens)]))])))))

;  maybe
;     Applies a parser optionally, returning a default value if the parser fails
;     without consuming any tokens or generating an error.
;  Arguments:
;      parser - Parser to try to apply
;      [default-value] - Value to return if parser fails (default: #f)
;  Returns:
;      A parser that tries to apply parser, but doesn't fail if it can't
(define (maybe parser [default-value #f])
  (lambda (tokens)
    (let ([result (parser tokens)])
      (if (EtaError? result)
          (cons default-value tokens)  ; Return default value without consuming tokens
          result))))



;; ---------- Terminal Parsers ----------

;; Left parenthesis parser
(define lparen
   (token-type 'LParenToken))

;; Right parenthesis parser
(define rparen
  (token-type 'RParenToken))

;; Dot symbol parser
(define dot-sym
 (token-type 'DotSymToken))

;; Quote symbol parser
(define quote-sym
   (token-type 'QuoteSymToken))

; Int
(define integer
  (map-parser
   (token-type 'IntToken)
   (lambda (token)
     (let ([loc (Token-loc token)])
       (make-const loc 'IntConstNode (string->number (Token-val token)))))))
      
; Float
(define float
  (map-parser
   (token-type 'FloatToken)
   (lambda (token)
     (let ([loc (Token-loc token)])
       (make-const loc 'FloatConstNode (string->number (Token-val token)))))))

; Bool
(define boolean 
  (map-parser
   (token-type 'BoolToken)
   (lambda (token)
     (let ([loc (Token-loc token)])
       (make-const loc 'BoolConstNode (if (equal? (Token-val token) "#t")
                                 #t
                                 #f))))))

; String
(define string 
  (map-parser
   (token-type 'StringToken)
   (lambda (token)
     (let ([loc (Token-loc token)])
       (make-const loc 'StringConstNode (Token-val token))))))


;; ---------- Non-terminal Parsers ----------



;  Const ::= Num | Bool | String
(define parse-const 
  (any-of
    (label "Num" (any-of integer float))
    (label "Bool" boolean)
    (label "String" string)))

; Id
(define parse-id 
  (map-parser
   (token-type 'IdToken)
   (lambda (token)
     (let ([loc (Token-loc token)])
       (make-var loc (Token-val token))))))

; (define Id Exp)
(define parse-variable-define
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "define" (keyword "define"))
    (label "Id" (parser-ref parse-id))
    (label "Exp" (parser-ref parse-exp))  ;; Use parser-ref for delayed evaluation
    (label "RParen" rparen))
    (lambda (result)
      (let ([name (third result)]
            [value (fourth result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-define loc name value)))))


(define (get-var-name id-expr)
  (if (and (Expr? id-expr)
           (equal? (Expr-head id-expr) 'IdHead))
      (first (Expr-args id-expr))
      (error "Faild to get var name. Expected Expr with Var head, got: ~a" id-expr)))

; (define (Id Id* [. Id]) Body)
(define parse-function-define 
    (map-parser
      (sequence
       (label "LParen" lparen)
       (label "define" (keyword "define"))
       (label "LParen" lparen)
       (label "Id" (parser-ref parse-id))
       (label "Id*" (zero-or-more (parser-ref parse-id)))
       (label "[. Id]" (maybe (sequence dot-sym (parser-ref parse-id))))
       (label "RParen" rparen)
       (label "Body" (parser-ref parse-body))
       (label "RParen" rparen))
      (lambda (result)
        (let ([name (fourth result)]
              [args (map get-var-name (fifth result))]
              [variadic-arg (if (sixth result)
                                (get-var-name (second (sixth result)))
                                '())]
              [body (eighth result)]
              [loc (create-span-location
                    (loc (first result))
                    (loc (last result)))])
          (make-define loc name 
                       (make-lambda loc 
                                     (make-list-arg loc args variadic-arg)
                                     body))))))


;  Define ::= (define Id Exp)
;            | (define (Id Id* [. Id*]) Body)
(define parse-define
  (any-of 
      (label "Variable Define" (parser-ref parse-variable-define))
      (label "Function Define" (parser-ref parse-function-define))
      ))



; Lambda ::= (lambda Arg Body)
(define parse-lambda
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "lambda" (keyword "lambda"))
    (label "Arg" (parser-ref parse-arg))
    (label "Body" (parser-ref parse-body))
    (label "RParen" rparen))
    (lambda (result)
      (let ([args (third result)]
            [body (fourth result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-lambda loc args body)))))



; SingleId = Id
(define parse-single-arg 
  (map-parser
   (token-type 'IdToken)
   (lambda (token)
     (let ([loc (Token-loc token)])
       (make-single-arg loc (Token-val token))))))

; MultipleIds = (Id* [Id . Id])
(define parse-list-arg 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "Id*" (zero-or-more (parser-ref parse-id)))
    (label "[. Id]" (maybe (sequence dot-sym (parser-ref parse-id))))
    (label "RParen" rparen))
    (lambda (result)
      (let ([required-args (map get-var-name (second result))]
            [variadic-args (if (third result) 
                                (get-var-name (second (third result)))
                               '())]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-list-arg loc required-args variadic-args)))))

; Arg ::= Id                                    ; Single argument
;     | (Id* [Id . Id])                         ; Argument list 
(define parse-arg
   (any-of
     (label "Single Id" (parser-ref parse-single-arg))
     (label "List of Ids" (parser-ref parse-list-arg))))

; App ::= (Exp Exp*)
(define parse-app 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "Exp" (parser-ref parse-exp))
    (label "Exp*" (zero-or-more (parser-ref parse-exp)))
    (label "RParen" rparen))
    (lambda (result)
      (let ([op (second result)]
            [args (third result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-app loc op args)))))

; Quote ::= (quote S-Exp)
(define parse-quote 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "quote" (keyword "quote"))
    (label "S-Exp" (parser-ref parse-s-exp))
    (label "RParen" rparen))
    (lambda (result)
      (let ([s-exp (third result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-quote loc s-exp)))))

; QuoteShorthand ::= 'S-Exp
(define parse-quote-shorthand 
  (map-parser
   (sequence
    (label "Quote" quote-sym)
    (label "S-Exp" (parser-ref parse-s-exp)))
    (lambda (result)
      (let ([s-exp (second result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-quote loc s-exp)))))

; Set! ::= (set! Id Exp)
(define parse-set!
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "set!" (keyword "set!"))
    (label "Id" (parser-ref parse-id))
    (label "Exp" (parser-ref parse-exp))
    (label "RParen" rparen))
    (lambda (result)
      (let ([name (third result)]
            [value (fourth result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-setbang loc name value)))))


; Let ::= (let [Id] Bindings Body)
(define parse-let 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "let" (keyword "let"))
    (label "[Id]" (maybe (parser-ref parse-id)))
    (label "Bindings" (parser-ref parse-bindings))
    (label "Body" (parser-ref parse-body))
    (label "RParen" rparen))
    (lambda (result)
      (let ([name (third result)]
            [bindings (fourth result)]
            [body (fifth result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (if name
            (make-named-let loc name bindings body)
            (make-unnamed-let loc bindings body))))))


; LetStar ::= (let* Bindings Body)
(define parse-let*
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "let*" (keyword "let*"))
    (label "Bindings" (parser-ref parse-bindings))
    (label "Body" (parser-ref parse-body))
    (label "RParen" rparen))
    (lambda (result)
      (let ([bindings (third result)]
            [body (fourth result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-letstar loc bindings body)))))

; Letrec ::= (letrec Bindings Body)
(define parse-letrec   
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "letrec" (keyword "letrec"))
    (label "Bindings" (parser-ref parse-bindings))
    (label "Body" (parser-ref parse-body))
    (label "RParen" rparen))
    (lambda (result)
      (let ([bindings (third result)]
            [body (fourth result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-letrec loc bindings body)))))


; If ::= (if Exp Exp [Exp])
(define parse-if
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "if" (keyword "if"))
    (label "Cond" (parser-ref parse-exp))
    (label "Then" (parser-ref parse-exp))
    (label "Else" (maybe (parser-ref parse-exp)))
    (label "RParen" rparen))
    (lambda (result)
      (let ([cond-exp (third result)]
            [then-exp (fourth result)]
            [else-exp (fifth result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (if else-exp
            (make-ifthenelse loc cond-exp then-exp else-exp)
            (make-ifthen loc cond-exp then-exp))))))

; Cond ::= (cond (Exp Exp+)* [(else Exp+)])
(define parse-cond 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "cond" (keyword "cond"))
    (label "CondClauses" (zero-or-more (parser-ref parse-cond-clause)))
    (label "Else" (maybe (sequence 
                             (label "LParen" lparen)
                             (keyword "else") 
                             (one-or-more (parser-ref parse-exp))
                             (label "RParen" rparen))))
    (label "RParen" rparen))
    (lambda (result)
      (let ([clauses (third result)]
            [else-exps (fourth result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (if else-exps
            (make-cond-else loc clauses (third else-exps))
            (make-cond-noelse loc clauses))))))


; CondClause ::= (Exp Exp+)
(define parse-cond-clause 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "Cond" (parser-ref parse-exp))
    (label "Then" (one-or-more (parser-ref parse-exp)))
    (label "RParen" rparen))
    (lambda (result)
      (let ([cond-exp (second result)]
            [then-exps (third result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-cond-clause loc cond-exp then-exps)))))


; And ::= (and Exp*)
(define parse-and 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "and" (keyword "and"))
    (label "Exp*" (zero-or-more (parser-ref parse-exp)))
    (label "RParen" rparen))
    (lambda (result)
      (let ([args (third result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-and loc args)))))

; Or ::= (or Exp*)
(define parse-or 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "or" (keyword "or"))
    (label "Exp*" (zero-or-more (parser-ref parse-exp)))
    (label "RParen" rparen))
    (lambda (result)
      (let ([args (third result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-or loc args)))))


; Begin ::= (begin Exp*)
(define parse-begin 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "begin" (keyword "begin"))
    (label "Exp*" (zero-or-more (parser-ref parse-exp)))
    (label "RParen" rparen))
    (lambda (result)
      (let ([args (third result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-begin loc args)))))


; Do ::= (do (DoLet*) DoFinal Body)
(define parse-do 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "do" (keyword "do"))
    (label "DoLet*" (zero-or-more (parser-ref parse-do-let)))
    (label "DoFinal" (parser-ref parse-do-final))
    (label "Body" (parser-ref parse-body))
    (label "RParen" rparen))
    (lambda (result)
      (let ([do-lets (third result)]
            [do-final (fourth result)]
            [body (fifth result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-do loc do-lets do-final body)))))

; DoLet ::= (Id Exp Exp)
(define parse-do-let 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "Id" (parser-ref parse-id))
    (label "InitExp" (parser-ref parse-exp))
    (label "StepExp" (parser-ref parse-exp))
    (label "RParen" rparen))
    (lambda (result)
      (let ([name (second result)]
            [init (third result)]
            [step (fourth result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-do-let loc name init step)))))


; DoFinal ::= (Exp Exp*)
(define parse-do-final
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "Cond" (parser-ref parse-exp))
    (label "Body" (zero-or-more (parser-ref parse-exp)))
    (label "RParen" rparen))
    (lambda (result)
      (let ([cond-exp (second result)]
            [body-exps (third result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-do-final loc cond-exp body-exps)))))

; CallCC ::= (call/cc | call-with-current-continuation Exp)
(define parse-call/cc
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "call/cc" (any-of (keyword "call/cc")
                             (keyword "call-with-current-continuation")))
    (label "Exp" (parser-ref parse-exp))
    (label "RParen" rparen))
    (lambda (result)
      (let ([exp (third result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-call/cc exp loc)))))


; Body ::= Define* Exp+
(define parse-body 
  (map-parser
   (sequence
    (label "Define*" (zero-or-more (parser-ref parse-define)))
    (label "Exp+" (one-or-more (parser-ref parse-exp))))
    (lambda (result)
      (let* ([defines (first result)]
            [exps (second result)]
            [loc (create-span-location
                  (if (empty? defines) 
                      (loc (first exps))
                      (loc (first defines)))
                  (loc (last exps)))])
        (make-body loc defines exps)))))


; Bindings ::= (Bind*)
(define parse-bindings
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "Bind*" (zero-or-more (parser-ref parse-bind)))
    (label "RParen" rparen))
    (lambda (result)
      (let ([bindings (second result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-bindings loc bindings)))))

; Bind ::= (Id Exp)
(define parse-bind 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "Id" (parser-ref parse-id))
    (label "Exp" (parser-ref parse-exp))
    (label "RParen" rparen))
    (lambda (result)
      (let ([name (second result)]
            [value (third result)]
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-bind loc name value)))))


; S-Exp ::= Const | Id | Keywords | (S-Exp* [S-Exp . S-Exp])
(define parse-s-exp 
  (any-of
    (label "Const" (parser-ref parse-const))
    (label "Id" (parser-ref parse-id))
    (label "Keyword" (map-parser
                      (token-type 'KeywordToken)
                      (lambda (token)
                        (let ([loc (Token-loc token)])
                          (make-var loc (Token-val token))))))
    (label "NestedS-Exp" (parser-ref parse-nested-s-exp))))

; NestedS-Exp ::= (S-Exp* [. S-Exp])
(define parse-nested-s-exp 
   (map-parser
   (sequence
    (label "LParen" lparen)
    (label "S-Exp*" (zero-or-more (parser-ref parse-s-exp)))
    (label "[S-Exp . S-Exp]" (maybe (sequence dot-sym (parser-ref parse-s-exp))))
    (label "RParen" rparen))
    (lambda (result)
      (let* ([s-exps (second result)]
             [tail (third result)]
             [loc (create-span-location
                    (loc (first result))
                    (loc (last result)))])
        (if tail
            (make-sexpr loc s-exps (second tail))
            (make-sexpr loc s-exps '()))))))

; Load ::= (load String)
; MEMO: discuss with Load is really needed
(define parse-load 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "load" (keyword "load"))
    (label "String" (token-type 'StringToken)) ; Note: not `string`. 
    (label "RParen" rparen))
    (lambda (result)
      (let ([filename (Token-val (third result))]  ; Extract the filename  
            [loc (create-span-location
                  (loc (first result))
                  (loc (last result)))])
        (make-load loc filename)))))



; Exp ::= Const                                 ; Constant
;     | Id                                      ; Variable
;     | (lambda Arg Body)                       ; Lambda abstraction
;     | (quote S-Exp)                           ; Quote 
;     | ('S-Exp)                                ; Quote shorthand 
;     | (set! Id Exp)                           ; Assignment
;     | (let [Id] Bindings Body)                ; Let
;     | (let* Bindings Body)                    ; Let* (Note 4)
;     | (letrec Bindings Body)                  ; Letrec
;     | (if Exp Exp [Exp])                      ; Conditional (if)
;     | (cond (Exp Exp+)* [(else Exp+)])        ; Conditional (cond) (Note 5)
;     | (and Exp*)                              ; Logical AND
;     | (or Exp*)                               ; Logical OR
;     | (begin Exp*)                            ; Sequential execution
;     | (do ((Id Exp Exp)*) (Exp Exp*) Body)    ; Iteration
;     | (Exp Exp*)                              ; Function application
(define parse-exp 
  (any-of
    (label "Const" parse-const)
    (label "Id" parse-id)
    (label "Lambda" parse-lambda)
    (label "Quote" parse-quote)
    (label "QuoteShorthand" parse-quote-shorthand)
    (label "Set!" parse-set!)
    (label "Let" parse-let)
    (label "Let*" parse-let*)
    (label "Letrec" parse-letrec)
    (label "If" parse-if)
    (label "Cond" parse-cond)
    (label "And" parse-and)
    (label "Or" parse-or)
    (label "Begin" parse-begin)
    (label "Do" parse-do)
    (label "CallCC" parse-call/cc)
    (label "App" parse-app)
  )
)


; Toplevel ::= 
;            | Load
;            | Define
;            | Exp
(define parse-toplevel
  (any-of
    (label "Load" parse-load)
    (label "Define" parse-define)
    (label "Exp" parse-exp)))


; Script ::= Toplevel*
(define parse-script
   (zero-or-more parse-toplevel))
   
;  parse-as
;     Parse input string using a specific parser function.
;  Arguments:
;     input - A string to parse
;     parser-fn - The specific parser function to use (e.g., parse-define, parse-exp)
;  Returns:
;     The parsed AST node or an error if parsing fails
;  Example:
;     (parse-as "(define (f x y) (+ x y))" parse-define)
;  Notes:
;     This allows testing specific parser components directly
(define (parse-as tokens parser-fn)
  (unless (and (list? tokens)
               (andmap Token? tokens))
    (error 'parse-as "Expected a list of tokens, but got: ~s" tokens))

  (let* ([result (parser-fn tokens)])
    (if (EtaError? result)
        result
        (match result
          [(cons ast rest-tokens)
           (if (and (not (empty? rest-tokens))
                    (not (and (= (length rest-tokens) 1)
                              (eq? (Token-typ (first rest-tokens)) 'EOFToken))))
               (make-parser-error "Unexpected tokens after parsing" 
                             (tokens-span rest-tokens))
               ast)]))))

; parse
;   Entry point of parsing.
; Arguments:
;     tokens - list of tokens
; Returns:
;     An expression tree or an error if parsing fails
(define (parse tokens)
  (parse-as tokens parse-script))
