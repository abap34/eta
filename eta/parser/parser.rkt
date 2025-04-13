#lang racket

(require "tokenizer.rkt"
         "ast.rkt"
         "../utils/error.rkt"
         "../utils/location.rkt")

(provide parse)


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


;; -- ast builders 


;  make-const
;     Create a constant expression node
;  Arguments:
;     location - Source location
;     tag - The type of the constant (e.g., number, string, boolean)
;     value - The constant value (e.g., 42, "hello", #t)
;  Returns:
;     An Expr with Const head
(define (make-const location tag value)
  (make-expr Const (list tag value) location))  ; TODO: validate tag

;  make-symbol
;     Create a symbol (variable) expression node
;  Arguments:
;     location - Source location
;     name - The symbol name (string)
;  Returns:
;     An Expr with Var head
(define (make-var location name)
      ; MEMO: Overwrite keyword is allowed. So we don't need to check like:
      ; (make-parse-error (format "Cannot use keyword ~a as variable name" name) location)
      (make-expr Var (list name) location))


; make-app
;     Create an application expression node
;  Arguments:
;     location - Source location
;     op - The operator to apply (Expr with any head).   
;     args - The arguments to the operator (list of Expr)
;  Returns:
;     An Expr with App head
(define (make-app location op args)
  (make-expr App (list op args) location))  

; make-lambda
;    Create a lambda expression node
;  Arguments:
;     location - Source location
;     args - The arguments to the lambda (Expr with Arg head)
;     body - The body of the lambda (Expr)
;  Returns:
;     An Expr with Lambda head
(define (make-lambda location args body)
  (make-expr Lambda (list args body) location))


; make-define
;     Create a define expression node
; Arguments:
;     location - Source location
;     name - The name of the variable or function (string)
;     value - The value to assign (Expr. If function, head is Lambda)
;  Returns:
;     An Expr with Define head
(define (make-define location name value)
    (make-expr Define (list name value) location))

; make-nil
;     Create a nil expression node
;  Arguments:
;     location - Source location
;  Returns:
;     An Expr with Nil head
(define (make-nil location)
  (make-expr Nil '() location))  ; Nil has no arguments


; make-single-arg
;     Create an argument expression node
;  Arguments:
;     location - Source location
;     name - The argument name (string)
;  Returns:
;     An Expr with Arg head
(define (make-single-arg location name)
      (make-expr Arg 
          '()           ; no required args
          (list name)   ; single variadic arg
      location))

; make-list-arg
;     Create a list of argument expression nodes
;  Arguments:
;     location - Source location
;     required-args - List of required argument names (strings)
;     variadic-args - List of variadic argument names (strings)
;  Returns:
;     An Expr with Arg head
(define (make-list-arg location required-args variadic-args)
  (make-expr Arg required-args variadic-args location))


; make-quote
;     Create a quote expression node
;  Arguments:
;     location - Source location
;     value - The quoted value (Expr)
;  Returns:
;     An Expr with Quote head
(define (make-quote location value)
  (make-expr Quote (list value) location))  ; Quote has one argument


; make-setbang
;     Create a set! expression node
;  Arguments:
;     location - Source location
;     name - The variable name to set (string)
;     value - The value to assign (Expr)
;  Returns:
;     An Expr with Set! head
(define (make-setbang location name value)
  (make-expr Set! (list name value) location))


; make-unnamed-let
;     Create a let (**not named**) expression node
;  Arguments:
;     location - Source location
;     bindings - binings (Expr with Bindings head)
;     body - The body of the let (Expr)
;  Returns:
;     An Expr with UnNamedLet head
(define (make-unnamed-let location bindings body)
  (make-expr UnNamedLet (list bindings body) location))  

; make-named-let
;     Create a named let expression node
;  Arguments:
;     location - Source location
;     name - The name of the let (string)
;     bindings - The bindings (Expr with Bindings head)
;     body - The body of the let (Expr)
;  Returns:
;     An Expr with NamedLet head
(define (make-named-let location name bindings body)
  (make-expr NamedLet (list name bindings body) location)) 

; make-letstar
;     Create a let* expression node
;  Arguments:
;     location - Source location
;     bindings - The bindings (Expr with Bindings head)
;     body - The body of the let* (Expr)
;  Returns:
;     An Expr with LetStar head
(define (make-letstar location bindings body)
  (make-expr LetStar (list bindings body) location)) 

; make-letrec
;     Create a letrec expression node
;  Arguments:
;     location - Source location
;     bindings - The bindings (Expr with Bindings head)
;     body - The body of the letrec (Expr)
;  Returns:
;     An Expr with LetRec head
(define (make-letrec location bindings body)
  (make-expr LetRec (list bindings body) location)) 


; make-ifthen
;     Create an if expression node without else
;  Arguments:
;     location - Source location
;     test - The test expression (Expr)
;     then - The then expression (Expr)
;  Returns:
;     An Expr with If head
(define (make-ifthen location test then)
  (make-expr If (list #f test then) location))
  ;                   ^^^ no else

; make-ifthenelse
;     Create an if expression node with else
;  Arguments:
;     location - Source location
;     test - The test expression (Expr)
;     then - The then expression (Expr)
;     else - The else expression (Expr)
;  Returns:
;     An Expr with If head
(define (make-ifthenelse location test then else)
  (make-expr If (list #t test then else) location))
;                     ^^^ has else

; make-cond-clause
;     Create a cond clause expression node
;  Arguments:
;     location - Source location
;     test - The test expression (Expr)
;     body - The body of the cond clause (Expr)
;  Returns:
;     An Expr with CondClause head
(define (make-cond-clause location test body)
  (make-expr CondClause (list test body) location))

; make-cond-noelse
;     Create a cond expression node without else
;  Arguments:
;     location - Source location
;     clauses - The list of cond clauses (Expr with CondClause head)
;  Returns:
;     An Expr with Cond head
(define (make-cond-noelse location clauses)
  (make-expr Cond (list #f clauses) location))
;                   ^^^ no else

; make-cond-else
;     Create a cond expression node with else
;  Arguments:
;     location - Source location
;     clauses - The list of cond clauses (Expr with CondClause head)
;     else - The else expression (Expr)
;  Returns:
;     An Expr with Cond head
(define (make-cond-else location clauses else)
  (make-expr Cond (list #t clauses else) location))
;                      ^^^ has else


; make-and
;     Create an and expression node
;  Arguments:
;     location - Source location
;     args - The list of arguments (Expr)
;  Returns:
;     An Expr with And head
(define (make-and location args)
  (make-expr And args location))

; make-or
;     Create an or expression node
;  Arguments:
;     location - Source location
;     args - The list of arguments (Expr)
;  Returns:
;     An Expr with Or head
(define (make-or location args)
  (make-expr Or args location))

; make-begin
;    Create a begin expression node
; Arguments:
;     location - Source location
;     args - The list of expressions to execute (Expr)
;  Returns:
;     An Expr with Begin head
(define (make-begin location args)
  (make-expr Begin args location))

; make-do-let
;     Create a binding expression node for do
;  Arguments:
;     location - Source location
;     name - The name of the variable (string)
;     init - The initial value (Expr)
;     step - The step value (Expr)
;  Returns:
;     An Expr with DoLet head
(define (make-do-let location name init step)
  (make-expr DoLet (list name init step) location))

; make-do-final
;     Create a final expression node for do
;  Arguments:
;     location - Source location
;     cond - The condition expression (Expr)
;     body - The body of the do (Expr)
;  Returns:
;     An Expr with DoFinal head
(define (make-do-final location cond body)
  (make-expr DoFinal (list cond body) location))


; make-do
;     Create a do expression node
;  Arguments:
;     location - Source location
;     do-lets - The list of do-let bindings (Expr with DoLet head)
;     do-final - The final expression (Expr with DoFinal head)
;     body - The body of the do (Expr)
;  Returns:
;     An Expr with Do head
(define (make-do location do-lets do-final body)
  (make-expr Do (list do-lets do-final body) location))


; make-body
;     Create a body expression node
;  Arguments:
;     location - Source location
;     defines - The list of definitions (Expr with Define head)
;     body - The body of the expression (Expr)
;  Returns:
;     An Expr with Body head
(define (make-body location defines body)
  (make-expr Body (list defines body) location))


; make-bind
;     Create a binding expression node
;  Arguments:
;     location - Source location
;     name - The name of the variable (string)
;     value - The value to bind (Expr)
;  Returns:
;     An Expr with Bind head
(define (make-bind location name value)
  (make-expr Bind (list name value) location))  


; make-bindings
;     Create a bindings expression node
; Arguments:
;     location - Source location
;     bindings - The list of bindings (Expr with Bind head)
;  Returns:
;     An Expr with Bindings head
(define (make-bindings location bindings)
  (make-expr Bindings bindings location))


; make-load
;     Create a load expression node
;  Arguments:
;     location - Source location
;     filename - The name of the file to load (string)
;  Returns:
;     An Expr with Load head
(define (make-load location filename)
  (make-expr Load (list filename) location))  

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
;       must fail
(define (logging msg)
  (lambda (tokens)
    (display (format "Log: ~a\n" msg))
    (make-parser-error "" (tokens-span tokens))))


;  any-of
;     Tries each parser in order, returning the result of the first successful parser
;     or the most relevant error from all tried parsers.
;  Arguments:
;      parsers - List of parsers to try
;      [error-msg] - Optional error message when all parsers fail
;  Returns:
;      A parser that returns the result of the first successful parser
;      or the most relevant error if all parsers fail
(define (any-of . parsers)
  (lambda (tokens)
    (let loop ([ps parsers] [ts tokens])
      (if (empty? ps)
          (make-parser-error "No parsers succeeded" (tokens-span tokens))
          (let ([parser (first ps)])
            (let ([result (parser ts)])
              (if (EtaError? result)
                  (loop (rest ps) ts)  ; Try the next parser
                  result)))))))
;  sequence
;     Applies a sequence of parsers in order, collecting their results
;  Arguments:
;      parsers - List of parser functions to apply in sequence
;      [error-handler] - Optional function to handle errors (default: return first error)
;  Returns:
;      A new parser that returns a list of all parser results or appropriate error
(define (sequence . parsers)
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
           (format "Error parsing ~a: ~a" desc (EtaError-message result))
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
         #f)
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
  (token-pred (lambda (token) 
                (and (eq? (Token-typ token) Id)
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

;  Terminal Parsers
;     Collection of parsers for each terminal symbol in the Eta language.
;     These parsers serve as the building blocks for more complex parsers.
;  Notes:
;     Each parser consumes a token of a specific type and returns it with
;     the remaining tokens. They can be combined with higher-level combinators
;     to build more complex parsing logic.

;; Left parenthesis parser
(define lparen
  (label "lparen" (token-type LParen)))

;; Right parenthesis parser
(define rparen
  (label "rparen" (token-type RParen)))

;; Dot symbol parser
(define dot-sym
  (label "dot" (token-type DotSym)))

;; Quote symbol parser
(define quote-sym
  (label "quote" (token-type QuoteSym)))

;; Boolean parser
(define boolean
  (label "boolean" (token-type Bool)))

;; Number parserp
(define number
  (label "number" (token-type Num)))

;; String parser
(define string
  (label "string" (token-type String)))

;; Identifier parser
(define identifier
  (label "identifier" (token-type Id)))

;; EOF parser
(define eof-parser
  (label "end-of-file" (token-type EOF)))

;; ---------- Non-terminal Parsers ----------

; Num
(define parse-number
  (map-parser
   (token-type Num)
   (lambda (token)
     (let ([loc (Token-loc token)])
       (make-const loc 'Num (Token-val token))))))
      

; Bool
(define parse-boolean 
  (map-parser
   (token-type Bool)
   (lambda (token)
     (let ([loc (Token-loc token)])
       (make-const loc 'Bool (Token-val token))))))

; String
(define parse-string 
  (map-parser
   (token-type String)
   (lambda (token)
     (let ([loc (Token-loc token)])
       (make-const loc 'String (Token-val token))))))


; ()
(define parse-nil
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "RParen" rparen))
    (lambda (result)
      (let ([loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (make-nil loc)))))


;  Const ::= Num | Bool | String | ()
(define parse-const 
  (any-of
    (label "Num" parse-number)
    (label "Bool" parse-boolean)
    (label "String" parse-string)
    (label "Nil" parse-nil)))


; Id
(define parse-id 
  (map-parser
   (token-type Id)
   (lambda (token)
     (let ([loc (Token-loc token)])
       (make-var loc (Token-val token))))))

; (define Id Exp)
(define parse-variable-define
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "define" (keyword "define"))
    (label "Id" identifier)
    (label "Exp" (parser-ref parse-exp))  ;; Use parser-ref for delayed evaluation
    (label "RParen" rparen))
    (lambda (result)
      (let ([name (second result)]
            [value (third result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (make-define loc name value)))))

; (define (Id Id* [. Id*]) Body)
(define parse-function-define 
    (map-parser
      (sequence
       (label "LParen" lparen)
       (label "define" (keyword "define"))
       (label "Id" identifier)
       (label "Id*" (zero-or-more identifier))
       (label "[. Id*]" (maybe (sequence dot-sym (zero-or-more identifier))))
       (label "Body" (parser-ref parse-body))
       (label "RParen" rparen))
      (lambda (result)
        (let ([name (second result)]
              [args (third result)]
              [variadic-args (if (fourth result) 
                                 (list (fourth result)) 
                                 '())]
              [body (fifth result)]
              [loc (create-span-location
                    (Token-loc (first result))
                    (Token-loc (last result)))])
          (make-define loc name 
                       (make-lambda loc 
                                     (make-list-arg loc args variadic-args)
                                     body))))))


;  Define ::= (define Id Exp)
;            | (define (Id Id* [. Id*]) Body)
(define parse-define
  (any-of 
      (label "Variable Define" (parser-ref parse-variable-define))
      (logging "Variable Define Failed")
      (label "Function Define" (parser-ref parse-function-define))
      (logging "Function Define Failed")
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
      (let ([args (second result)]
            [body (third result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (make-lambda loc args body)))))



; Single Id
(define parse-single-arg 
  (map-parser
   (token-type Id)
   (lambda (token)
     (let ([loc (Token-loc token)])
       (make-single-arg loc (Token-val token))))))

; Multiple Ids
(define parse-list-arg 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "Id*" (one-or-more identifier))
    (label "[. Id]" (maybe (sequence dot-sym identifier)))
    (label "RParen" rparen))
    (lambda (result)
      (let ([required-args (second result)]
            [variadic-args (if (third result) 
                               (list (third result)) 
                               '())]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (make-list-arg loc required-args variadic-args)))))

; Arg ::= Id                                    ; Single argument
;     | (Id* [Id . Id])                         ; Argument list 
(define parse-arg
   (any-of
    (list
     (label "Single Id" (parser-ref parse-single-arg))
      (label "List of Ids" (parser-ref parse-list-arg)))))



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
                  (Token-loc (first result))
                  (Token-loc (last result)))])
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
                  (Token-loc (first result))
                  (Token-loc (last result)))])
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
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (make-quote loc s-exp)))))

; Set! ::= (set! Id Exp)
(define parse-set!
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "set!" (keyword "set!"))
    (label "Id" identifier)
    (label "Exp" (parser-ref parse-exp))
    (label "RParen" rparen))
    (lambda (result)
      (let ([name (second result)]
            [value (third result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (make-setbang loc name value)))))


; Let ::= (let [Id] Bindings Body)
(define parse-let 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "let" (keyword "let"))
    (label "[Id]" (maybe identifier))
    (label "Bindings" (parser-ref parse-bindings))
    (label "Body" (parser-ref parse-body))
    (label "RParen" rparen))
    (lambda (result)
      (let ([name (second result)]
            [bindings (third result)]
            [body (fourth result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
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
      (let ([bindings (second result)]
            [body (third result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
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
      (let ([bindings (second result)]
            [body (third result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
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
      (let ([cond-exp (second result)]
            [then-exp (third result)]
            [else-exp (fourth result)]
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
    (label "Else" (maybe (sequence (keyword "else") (one-or-more (parser-ref parse-exp)))))
    (label "RParen" rparen))
    (lambda (result)
      (let ([clauses (second result)]
            [else-exps (third result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (if else-exps
            (make-cond-else loc clauses else-exps)
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
                  (Token-loc (first result))
                  (Token-loc (last result)))])
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
      (let ([args (second result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
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
      (let ([args (second result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
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
                  (Token-loc (first result))
                  (Token-loc (last result)))])
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
      (let ([do-lets (second result)]
            [do-final (third result)]
            [body (fourth result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (make-do loc do-lets do-final body)))))

; DoLet ::= (Id Exp Exp)
(define parse-do-let 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "Id" identifier)
    (label "InitExp" (parser-ref parse-exp))
    (label "StepExp" (parser-ref parse-exp))
    (label "RParen" rparen))
    (lambda (result)
      (let ([name (second result)]
            [init (third result)]
            [step (fourth result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
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
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (make-do-final loc cond-exp body-exps)))))

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
                      (Expr-loc (first exps))
                      (Expr-loc (first defines)))
                  (Expr-loc (last exps)))])
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
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (make-bindings loc bindings)))))

; Bind ::= (Id Exp)
(define parse-bind 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "Id" identifier)
    (label "Exp" (parser-ref parse-exp))
    (label "RParen" rparen))
    (lambda (result)
      (let ([name (second result)]
            [value (third result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
        (make-bind loc name value)))))


; S-Exp ::= Const | Id | (S-Exp S-Exp*)
(define parse-s-exp 
  (any-of
    (label "Const" (parser-ref parse-const))
    (label "Id" (parser-ref parse-id))
    (label "S-Expr" (parser-ref parse-s-exp))))


; Load ::= (load String)
; MEMO: discuss with Load is really needed
(define parse-load 
  (map-parser
   (sequence
    (label "LParen" lparen)
    (label "load" (keyword "load"))
    (label "String" parse-string)
    (label "RParen" rparen))
    (lambda (result)
      (let ([filename (second result)]
            [loc (create-span-location
                  (Token-loc (first result))
                  (Token-loc (last result)))])
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
    (label "App" parse-app)
  )
)


; Toplevel ::= 
;            | Define
;            | Exp
;            | Load
(define parse-toplevel
  (any-of
    (label "Define" parse-define)
    (label "Exp" parse-exp)
    (label "Load" parse-load)))



; parse
;   Entry point of parsing.
; Arguments:
;     tokens - list of tokens
; Returns:
;     An expression tree or an error if parsing fails
(define (parse tokens)
  (try-parser parse-toplevel tokens))
