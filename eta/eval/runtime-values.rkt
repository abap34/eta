#lang racket

(require rebellion/type/enum  
        "../parser/ast.rkt")

(provide runtime-value->string
         make-param-spec
         make-eta-closure
         make-runtime-value
         make-void
         runtime-value->string
         ParamSpec?
         Closure
         Closure?
         Builtin
         Builtin?
         Builtin-proc
         EtaValue
         EtaBuiltin
         EtaValue-tag
         EtaExpr
         EtaStruct
         make-builtin
         Number
         String
         Boolean
         EtaValue-value
         List
         NilValue
         EtaClosure
         Void
         Undefined
         EtaValue?
         ParamSpec-required
         ParamSpec-variadic
         Closure-params-spec
         Closure-body
         Closure-captured-env
         Closure-loc
         arity-check
)


(define-enum-type EtaValueTag
  (Number
   String
   Boolean
   NilValue
   List
   EtaExpr
   EtaBuiltin
   EtaClosure 
   EtaStruct
   Undefined
   Void
  )
)

(define (tag-checker pred tag expect-pass-msg)
  (lambda (tag)
    (if (pred tag)
        tag
        (error "Internal error: tag ~a must be pass ~a." tag expect-pass-msg))))

; runtime-value definition
(struct EtaValue (tag value) #:transparent)


; ParamSpec
;     A structure to represent the parameter specification of a function
; Arguments:
;    required - A list of required parameter names (strings)
;    variadic - A string representing the variadic parameter name
; Returns:
;    A ParamSpec object
(struct ParamSpec (required variadic) #:transparent)
(define (make-param-spec required variadic)
  (if (and (list? required) 
            (or (string? variadic) (null? variadic))
            (andmap string? required))
      (ParamSpec required variadic)
      (error (format "Internal error: required must be a list of strings, and variadic must be a string or #f. But got ~a" (list required variadic)))))


(define (has-rest? param-spec)
  (ParamSpec-variadic param-spec))

; arity-check
;     Check the arity of a function call
; Arguments:
;    param-spec - A ParamSpec object
;    args - A list of arguments 
; Returns:
;    #t if the arity is correct, #f otherwise
; Example:
;    (arity-check (make-param-spec (list "x" "y") "z") (list 1 2 3 4)) ; => #t  (x=1, y=2, z=(list 3 4))
;    (arity-check (make-param-spec (list "x" "y") "z") (list 1 2 3))   ; => #t  (x=1, y=2, z=(list 3))
;    (arity-check (make-param-spec (list "x" "y") "z") (list 1 2))     ; => #t  (x=1, y=2, z=())      
;    (arity-check (make-param-spec (list "x" "y") #f) (list 1))        ; => #f  
;    (arity-check (make-param-spec (list "x" "y") #f) (list 1 2))      ; => #f
(define (arity-check param-spec args)
  (define num-required (length (ParamSpec-required param-spec)))
  (define num-args (length args))
  (if (has-rest? param-spec)
      (>= num-args num-required)
      (= num-args num-required))
)

; Builtin
;     A structure to represent a built-in function
; Arguments:
;    proc - A procedure object (args env -> RuntimeValue)
(struct Builtin (proc) #:transparent)
(define (make-builtin proc)
  (if (procedure? proc)
      (EtaValue EtaBuiltin (Builtin proc))
      (error (format "Internal error: proc must be a procedure, but got ~a" proc))))

(struct Closure (params-spec body captured-env loc) #:transparent)
(define (make-eta-closure param-spec body captured-env loc)
    (Closure param-spec body captured-env  loc))  ; TODO: validation
      
(struct StructInstance (name fields) #:transparent)


(define (make-runtime-value tag value)
  (match tag
    [(== Number) 
      (tag-checker number? tag "number?")
      ]
    [(== String)
      (tag-checker string? tag "string?")]
    [(== Boolean)
      (tag-checker boolean? tag "boolean?")]
    [(== NilValue)
      (tag-checker null? tag "null?")]
    [(== List)
    ; list? and all elements are EtaValue
      (tag-checker (lambda (x) 
                     (and (list? x) (andmap EtaValue? x)))
                    tag "list?")]
    [(== EtaExpr)
      (tag-checker Expr? tag "Expr?")]
    [(== EtaBuiltin)
      (tag-checker Builtin? tag "Builtin?")]
    [(== EtaClosure)
      (tag-checker Closure? tag "EtaClosure?")]
    [(== EtaStruct)
      (tag-checker StructInstance? tag "StructInstance?")]
    [(== Void)
      (tag-checker (lambda (x) 
                     (or (null? x) (equal? x '())))
                    tag "Void")]
    [(== Undefined)
      (tag-checker (lambda (x) 
                     (equal? x 'undefined))
                    tag "Undefined")]
    [else (error "Internal error: unknown tag ~a" tag)]
    )

    (EtaValue tag value))


(define (make-void) 
   (make-runtime-value Void '()))

;  runtime-value->string
;     Convert a runtime value into its string representation for REPL display
;  Arguments:
;      v - A runtime value (number, boolean, string, symbol, list, primitive-procedure, compound-procedure, etc.)
;  Returns:
;      A string suitable for printing in the REPL
;  Example:
;      (runtime-value->string 42)           ⇒ "42"
;      (runtime-value->string #t)           ⇒ "#t"
;      (runtime-value->string "hi")        ⇒ ""hi""
;      (runtime-value->string 'a)            ⇒ "a"
;      (runtime-value->string '(1 2 3))      ⇒ "(1 2 3)"
;      (runtime-value->string (make-primitive-procedure "+" + 'any))
;                                           ⇒ "#<native +>"
;      (runtime-value->string (make-compound-procedure params body env))
;                                           ⇒ "#<closure lambda>"
(define (runtime-value->string v)
  (match v
    [(EtaValue tag value)
      (match tag
       [(== Number) (number->string value)]
       [(== String) (format "\"~a\"" value)]
       [(== Boolean) (if value "#t" "#f")]
       [(== NilValue) "nil"]
       [(== List) (format "(~a)" (string-join (map runtime-value->string value) " "))]
       [(== EtaExpr) (format "<Expr: ~a>" value)]
       [(== EtaBuiltin) (format "<Builtin: ~a>" value)]
       [(== EtaClosure) (format "<Closure: ~a>" value)]
       [(== EtaStruct) (format "<StructInstance: ~a>" value)]
       [(== Undefined) "undefined"]
       [(== Void) "void"]
       [else (error "Internal error: unknown tag ~a" tag)])]
    [else (error (format "Internal error: expected EtaValue, but got ~a" v))]))