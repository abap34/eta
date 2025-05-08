#lang racket

(require  "../parser/ast.rkt")

(provide runtime-value->string
         EtaValue
         EtaValue?
         make-param-spec
         make-eta-closure
         make-builtin
         make-runtime-value
         ParamSpec-required
         ParamSpec-variadic
         Builtin-proc
         Closure-params-spec
         Closure-body
         Closure-captured-env
         Closure-loc
         EtaValue-tag
         EtaValue-value
         arity-check
)


(define (RuntimeValueTag? tag)
  (or (equal? tag 'IntTag)
      (equal? tag 'FloatTag)  
      (equal? tag 'StringTag)
      (equal? tag 'BooleanTag)
      (equal? tag 'NilValueTag)
      (equal? tag 'ListTag)
      (equal? tag 'EtaExprTag)
      (equal? tag 'EtaBuiltinTag)
      (equal? tag 'EtaClosureTag)
      (equal? tag 'EtaStructTag)
      (equal? tag 'VoidTag)
      (equal? tag 'UndefinedTag)))


(define (RuntimeValueTag->string tag)
  (cond
    [(equal? tag 'IntTag) "int"]
    [(equal? tag 'FloatTag) "float"]
    [(equal? tag 'StringTag) "string"]
    [(equal? tag 'BooleanTag) "boolean"]
    [(equal? tag 'NilValueTag) "nil"]
    [(equal? tag 'ListTag) "list"]
    [(equal? tag 'EtaExprTag) "Expr"]
    [(equal? tag 'EtaBuiltinTag) "Builtin"]
    [(equal? tag 'EtaClosureTag) "Closure"]
    [(equal? tag 'EtaStructTag) "StructInstance"]
    [(equal? tag 'VoidTag) "void"]
    [(equal? tag 'UndefinedTag) "undefined"]
    [else (error (format "Internal error: unknown tag ~a" tag))]))

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

(define (ParamSpec->string param-spec)
  (let ([required (ParamSpec-required param-spec)]
        [variadic (ParamSpec-variadic param-spec)])
    (if (null? variadic)
        (format "(~a)" (string-join required ", "))
        (format "(~a . ~a)" (string-join required ", ") variadic))))


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
      (EtaValue 'EtaBuiltinTag (Builtin proc))
      (error (format "Internal error: proc must be a procedure, but got ~a" proc))))

(define (pretty-print-Builtin builtin)
  (let ([proc (Builtin-proc builtin)])
    (if (procedure? proc)
        proc
        (error "Internal error: Builtin proc must be a procedure, but got ~a" proc))))  

(struct Closure (params-spec body captured-env loc) #:transparent)
(define (make-eta-closure param-spec body captured-env loc)
    (Closure param-spec body captured-env  loc))  ; TODO: validation

(define (pretty-print-Closure closure)
  (let ([params-spec (Closure-params-spec closure)]
        [body (Closure-body closure)]
        [captured-env (Closure-captured-env closure)]
        [loc (Closure-loc closure)])
    (format "~a -> ~a" (ParamSpec->string params-spec) (pretty-print-Expr body))))


(struct StructInstance (name fields) #:transparent)


(define (make-runtime-value tag value)
  (cond
    [(equal? tag 'IntTag) 
      (tag-checker number? tag "number?")]
    [(equal? tag 'FloatTag)
      (tag-checker number? tag "number?")]
    [(equal? tag 'StringTag)
      (tag-checker string? tag "string?")]
    [(equal? tag 'BooleanTag)
      (tag-checker boolean? tag "boolean?")]
    [(equal? tag 'NilValueTag)
      (tag-checker null? tag "null?")]
    [(equal? tag 'ListTag)
    ; list? and all elements are EtaValue
      (tag-checker (lambda (x) 
                     (and (list? x) (andmap EtaValue? x)))
                    tag "list?")]
    [(equal? tag 'EtaExprTag)
      (tag-checker Expr? tag "Expr?")]
    [(equal? tag 'EtaBuiltinTag)
      (tag-checker Builtin? tag "Builtin?")]
    [(equal? tag 'EtaClosureTag)
      (tag-checker Closure? tag "EtaClosure?")]
    [(equal? tag 'EtaStructTag)
      (tag-checker StructInstance? tag "StructInstance?")]
    [(equal? tag 'VoidTag)
      (tag-checker (lambda (x) 
                     (or (null? x) (equal? x '())))
                    tag "Void")]
    [(equal? tag 'UndefinedTag)
      (tag-checker (lambda (x) 
                     (equal? x 'undefined))
                    tag "Undefined")]
    [else (error "Internal error: unknown tag ~a" tag)]
    )

    (EtaValue tag value))


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
   (if (not (EtaValue? v))
    (error "Internal error: runtime-value->string expects an EtaValue, but got ~a" v)
    (let ([tag (EtaValue-tag v)]
          [value (EtaValue-value v)]) 
      (cond
      [(equal? tag 'IntTag) (format "~a" value)]
      [(equal? tag 'FloatTag) (format "~a" value)]
      [(equal? tag 'StringTag) (format "\"~a\"" value)]
      [(equal? tag 'BooleanTag) (if value "#t" "#f")]
      [(equal? tag 'NilValueTag) "'()"]
      [(equal? tag 'ListTag) (format "(~a)" (string-join (map runtime-value->string value) " "))]
      [(equal? tag 'EtaExprTag) (format "<expr: ~a>" (pretty-print-Expr value))]
      [(equal? tag 'EtaBuiltinTag) (format "<builtin: ~a>" (pretty-print-Builtin value))]
      [(equal? tag 'EtaClosureTag) (format "<closure: ~a>" (pretty-print-Closure value))]
      [(equal? tag 'EtaStructTag) (format "<StructInstance: ~a>" value)]
      [(equal? tag 'UndefinedTag) "undefined"]
      [(equal? tag 'VoidTag) "void"]
      [else (error "Internal error: unknown tag ~a" tag)]))))
