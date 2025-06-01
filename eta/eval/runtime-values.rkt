#lang racket

(require  "../parser/ast.rkt")

(provide runtime-value->string
         RuntimeValue
         RuntimeValue?
         make-param-spec
         make-eta-closure
         make-builtin
         make-runtime-value
         make-Pair
         list->Pair
         Pair?
         Pair-car
         Pair-cdr
         set-Pair-car!
         set-Pair-cdr!
         make-Vector
         Vector?
         Vector-elements
         set-Vector-elements!
         ParamSpec-required
         ParamSpec-variadic
         ParamSpec?
         has-rest?
         Builtin-proc
         Builtin-name
         Builtin?
         Closure-params-spec
         Closure-body
         Closure-captured-env
         Closure-loc
         Closure-name
         Closure?
         RuntimeValue-tag
         RuntimeValue-value
         arity-check
         
         ; Continuation-related exports
         make-continuation
         Continuation-k
         Continuation-stack
         Continuation?
)


(define (RuntimeValueTag? tag)
  (or (equal? tag 'IntTag)
      (equal? tag 'FloatTag)  
      (equal? tag 'StringTag)
      (equal? tag 'BooleanTag)
      (equal? tag 'NilValueTag)
      (equal? tag 'PairTag)
      (equal? tag 'SymbolTag)
      (equal? tag 'EtaBuiltinTag)
      (equal? tag 'EtaClosureTag)
      (equal? tag 'EtaStructTag)
      (equal? tag 'VectorTag)
      (equal? tag 'VoidTag)
      (equal? tag 'UndefinedTag)
      (equal? tag 'EtaContinuationTag)))


(define (RuntimeValueTag->string tag)
  (cond
    [(equal? tag 'IntTag)        "Int"]
    [(equal? tag 'FloatTag)      "Float"]
    [(equal? tag 'StringTag)     "String"]
    [(equal? tag 'BooleanTag)    "Boolean"]
    [(equal? tag 'NilValueTag)   "Nil"]
    [(equal? tag 'PairTag)       "Pair"]
    [(equal? tag 'SymbolTag)     "Symbol"]
    [(equal? tag 'EtaBuiltinTag) "Builtin"]
    [(equal? tag 'EtaClosureTag) "Closure"]
    [(equal? tag 'EtaStructTag)  "StructInstance"]
    [(equal? tag 'VectorTag)     "Vector"]
    [(equal? tag 'VoidTag)       "Void"]
    [(equal? tag 'UndefinedTag)  "Undefined"]
    [(equal? tag 'EtaContinuationTag) "Continuation"]
    [else                        (error (format "Internal error: unknown tag ~a" tag))]))

(define (tag-checker pred tag expect-pass-msg)
  (lambda (tag)
    (if (pred tag)
        tag
        (error "Internal error: tag ~a must be pass ~a." tag expect-pass-msg))))

; runtime-value definition
(struct RuntimeValue (tag value) #:transparent)

(define (make-runtime-value tag value)
  (cond
    [(equal? tag 'IntTag)        (tag-checker number? tag "number?")]
    [(equal? tag 'FloatTag)      (tag-checker number? tag "number?")]
    [(equal? tag 'StringTag)     (tag-checker string? tag "string?")]
    [(equal? tag 'BooleanTag)    (tag-checker boolean? tag "boolean?")]
    [(equal? tag 'NilValueTag)   (tag-checker null? tag "null?")]
    [(equal? tag 'PairTag)       (tag-checker Pair? tag "Pair?")]
    [(equal? tag 'SymbolTag)     (tag-checker symbol? tag "symbol?")]
    [(equal? tag 'EtaBuiltinTag) (tag-checker Builtin? tag "Builtin?")]
    [(equal? tag 'EtaClosureTag) (tag-checker Closure? tag "EtaClosure?")]
    [(equal? tag 'EtaStructTag)  (tag-checker StructInstance? tag "StructInstance?")]
    [(equal? tag 'VectorTag)     (tag-checker Vector? tag "Vector?")]
    [(equal? tag 'VoidTag)       (tag-checker (lambda (x) (or (null? x) (equal? x '()))) tag "Void")]
    [(equal? tag 'UndefinedTag)  (tag-checker (lambda (x) (equal? x 'undefined)) tag "Undefined")]
    [(equal? tag 'EtaContinuationTag) (tag-checker Continuation? tag "Continuation?")]
    [else (error "Internal error: unknown tag ~a" tag)]
    )
    
  (RuntimeValue tag value))

; Pair
;   A structure to represent a pair (cons cell) in the runtime
; Arguments:
;    car - The first element of the pair
;    cdr - The second element of the pair (can be another pair or nil)
(struct Pair (car cdr) #:transparent #:mutable)
(define (make-Pair car cdr)
    (Pair car cdr))

; Vector
;   A structure to represent a vector in the runtime
; Arguments:
;    elements - A mutable vector of runtime values
(struct Vector (elements) #:transparent #:mutable)
(define (make-Vector size init-value)
  (if (and (number? size) (>= size 0))
      (Vector (make-vector size init-value))
      (error (format "Internal error: make-Vector expects a non-negative number for size, but got ~a" size))))

; list->Pair
;   Convert a list of RuntimeValues into a Pair object
; Arguments:
;    args - A list of RuntimeValues
; Returns:
;    A Pair object representing the list
(define (list->Pair args)
  (if (null? args)
      (make-Pair (RuntimeValue 'NilValueTag '()) (RuntimeValue 'NilValueTag '()))
      (let loop ([lst args] [acc (RuntimeValue 'NilValueTag '())])
        (if (null? lst)
            acc
            (let ([car (car lst)]
                  [cdr (loop (cdr lst) acc)])
              (make-Pair car cdr))))))

; pretty-print-Pair
;   Convert a Pair into a string representation
; Arguments:
;    pair - A Pair object
; Returns:
;    A string representation of the pair
; Example:
;    (pretty-print-Pair (make-pair (RuntimeValue 'IntTag 1) (RuntimeValue 'IntTag 2))) ; => "(1 . 2)"
;    (pretty-print-Pair (make-pair (RuntimeValue 'IntTag 1) (make-pair (RuntimeValue 'IntTag 2) (RuntimeValue 'NilValueTag)))) ; => "(1 2)"
(define (pretty-print-Pair pair)
  (unless (Pair? pair)
    (error "Internal error: pretty-print-Pair expects a Pair, but got ~a" pair))
  
  (let ([car (Pair-car pair)]
        [cdr (Pair-cdr pair)])
    (if (equal? cdr (RuntimeValue 'NilValueTag '()))
        (format "(~a)" (runtime-value->string car))
        (format "(~a . ~a)" (runtime-value->string car) (runtime-value->string cdr)))))

; pretty-print-Vector
;   Convert a Vector into a string representation
; Arguments:
;    vector - A Vector object
; Returns:
;    A string representation of the vector
; Example:
;    (pretty-print-Vector (Vector #(1 2 3))) ; => "[1 2 3]"
(define (pretty-print-Vector vector)
  (unless (Vector? vector)
    (error "Internal error: pretty-print-Vector expects a Vector, but got ~a" vector))
  
  (let ([elements (Vector-elements vector)])
    (format "[~a]" 
            (string-join 
             (map runtime-value->string 
                  (vector->list elements)) 
             " "))))


; ParamSpec
;     A structure to represent the parameter specification of a function
; Arguments:
;    required - A list of required parameter names (strings)
;    variadic - A string representing the variadic parameter name. if no variadic parameter, it should be #f
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
  (not (null? (ParamSpec-variadic param-spec))))

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
;    name - The name of the builtin function (optional)
(struct Builtin (proc name) #:transparent)
(define (make-builtin proc [name #f])
  (if (procedure? proc)
      (RuntimeValue 'EtaBuiltinTag (Builtin proc name))
      (error (format "Internal error: proc must be a procedure, but got ~a" proc))))

(define (pretty-print-Builtin builtin)
  (unless (Builtin? builtin)
    (error "Internal error: pretty-print-Builtin expects a Builtin, but got ~a" builtin))

  (let ([proc (Builtin-proc builtin)])
    (if (procedure? proc)
        proc
        (error "Internal error: Builtin proc must be a procedure, but got ~a" proc))))  

(struct Closure (params-spec body captured-env loc name) #:transparent)
(define (make-eta-closure param-spec body captured-env loc name)
    (Closure param-spec body captured-env loc name))  ; TODO: validation

(define (pretty-print-Closure closure)
  (unless (Closure? closure)
    (error "Internal error: pretty-print-Closure expects a Closure, but got ~a" closure))

  (let ([params-spec (Closure-params-spec closure)]
        [name (Closure-name closure)])
    (format "<closure: ~a>" (ParamSpec->string params-spec))))

(struct StructInstance (name fields) #:transparent)

; Continuation
;    A structure to represent a captured continuation
; Arguments:
;    k - The captured continuation function
;    stack - The captured call stack
(struct Continuation (k stack) #:transparent)

; make-continuation
;    Creates a new continuation value
; Arguments:
;    k - The continuation function to capture
;    stack - The call stack at the point of capture
; Returns:
;    An RuntimeValue representing the continuation
(define (make-continuation k stack)
  (if (and (procedure? k) stack)
      (RuntimeValue 'EtaContinuationTag (Continuation k stack))
      (error (format "Internal error: Invalid continuation components: ~a, ~a" k stack))))


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
   (if (not (RuntimeValue? v))
    (error "Internal error: runtime-value->string expects an RuntimeValue, but got ~a" v)
    (let ([tag   (RuntimeValue-tag v)]
          [value (RuntimeValue-value v)]) 
      (cond
      [(equal? tag 'IntTag)        (format "~a" value)]
      [(equal? tag 'FloatTag)      (format "~a" value)]
      [(equal? tag 'StringTag)     (format "~a" value)]
      [(equal? tag 'BooleanTag)    (if value "#t" "#f")]
      [(equal? tag 'NilValueTag)   "'()"]
      [(equal? tag 'PairTag)       (format "~a" (pretty-print-Pair value))]
      [(equal? tag 'SymbolTag)     (format "~a" value)]
      [(equal? tag 'EtaBuiltinTag) (format "<builtin: ~a>" (pretty-print-Builtin value))]
      [(equal? tag 'EtaClosureTag) (pretty-print-Closure value)]
      [(equal? tag 'EtaStructTag)  (format "<StructInstance: ~a>" value)]
      [(equal? tag 'VectorTag)     (pretty-print-Vector value)]
      [(equal? tag 'UndefinedTag)  "undefined"]
      [(equal? tag 'VoidTag)       "void"]
      [(equal? tag 'EtaContinuationTag) (format "<continuation>")]
      [else                        (error "Internal error: unknown tag ~a" tag)]))))
