#lang racket

(require "runtime-values.rkt"
         "env.rkt"
         "../utils/error.rkt"
         "stack-frame.rkt"
         "../utils/location.rkt"
         "../desugar/desugar.rkt"
         "interp.rkt"
         "../parser/ast.rkt"
         "../parser/tokenizer.rkt"
         racket/file)


(provide 
  get-builtin-names
  int-value?
  float-value?
  string-value?
  boolean-value?
  nil-value?
  list-value?
  builtin-value?
  closure-value?
  struct-value?
  undefined-value?
  void-value?
  vector-value?
  add-builtins-to-env
  with-stack-trace
  runtime-value->expr
)


;  with-stack-trace
;     A utility function that adds stack trace information to runtime errors
;  Arguments:
;     result - The result of a computation that might be a RuntimeError
;     loc - Source location information for the error
;  Returns:
;     The RuntimeError with stack trace information if result is a RuntimeError, otherwise result
;  Example:
;     (with-stack-trace (some-operation) (make-location 10 5 10 6))
(define (with-stack-trace result loc)
  (if (RuntimeError? result)
      (if (not (EtaError-location result))
          (localize-error-location result loc)
          result)
      result))

;  with-error-handling
;     A utility function that handles runtime errors properly
;  Arguments:
;     result - The result of a computation that might be a RuntimeError
;     proc - A function to ;  get-builtin-names
;  Returns:
;     The RuntimeError if result is a RuntimeError, otherwise (proc result)
;  Example:
;     (with-error-handling nums (lambda (n) (apply + n)))
(define (with-error-handling result proc)
  (if (RuntimeError? result)
      result  ; Pass the error through
      (proc result)))

;  with-value-check
;     Checks if a value matches the expected type and extracts its value or returns an error
;  Arguments:
;     value - The value to check
;     type-pred - A predicate function that checks if the value is of the expected type
;     error-msg - A function that generates an error message if the check fails
;  Returns:
;     The extracted value or a RuntimeError
;  Example:
;     (with-value-check arg list-value? (lambda (v) (format "Expected a list, got: ~a" v)))
(define (with-value-check value type-pred error-msg)
  (if (type-pred value)
      (RuntimeValue-value value)
      (make-runtime-error (error-msg (runtime-value->string value)))))

;  Type checking functions
;     Check if an RuntimeValue has a specific tag
;  Arguments:
;     value - An RuntimeValue to check
;  Returns:
;     #t if the value has the specified tag, #f otherwise
;  Example:
;     (number-value? (make-runtime-value Number 42)) ; => #t
;     (number-value? (make-runtime-value String "hello")) ; => #f

(define (int-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'IntTag)))

(define (float-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'FloatTag)))

(define (string-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'StringTag)))

(define (boolean-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'BooleanTag)))

(define (nil-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'NilValueTag)))

(define (list-value? value)
  (and (RuntimeValue? value) (or (equal? (RuntimeValue-tag value) 'PairTag)
                                  (equal? (RuntimeValue-tag value) 'NilValueTag))))

(define (pair-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'PairTag)))

(define (symbol-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'SymbolTag)))                              

(define (builtin-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'EtaBuiltinTag)))

(define (closure-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'EtaClosureTag)))

(define (struct-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'EtaStructTag)))

(define (undefined-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'UndefinedTag)))

(define (void-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'Void)))

(define (vector-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'VectorTag)))

;  check-args-count
;     Validates that the number of arguments matches the expected count
;  Arguments:
;     func-name - Name of the function for error messages
;     args - The argument list to check
;  Returns:
;     void (raises an error if validation fails)
;  Example:
;     (check-args-count "+" args 2)    ; Exactly 2 arguments
;     (check-args-count "-" args '(1)) ; At least 1 argument
;     (check-args-count "cons" args '(2 2)) ; Between 1 and 2 arguments (i.e., exactly 2)
(define (check-args-count func-name args expected proc)
  (let ([count (length args)])
    (cond
      [(number? expected) 
       (if (= count expected)
         (proc args)
         (make-runtime-error 
                (format "~a expects ~a argument~a, got ~a" 
                       func-name expected (if (= expected 1) "" "s") count)))]
      [(list? expected)
       (let ([min (first expected)]
             [max (if (> (length expected) 1) (second expected) +inf.0)])
         (when (< count min)
           (make-runtime-error 
                  (format "~a requires at least ~a argument~a, got ~a" 
                         func-name min (if (= min 1) "" "s") count)))
         (if (> count max)
           (make-runtime-error 
                  (format "~a expects at most ~a argument~a, got ~a" 
                         func-name max (if (= max 1) "" "s") count))
            (proc args)
                  ))])))

;  determine-number-tag
;     Determines the appropriate tag for a numeric result
;  Arguments:
;     result - A numeric value
;     arg-values - Original numeric values used in the computation
;  Returns:
;     'IntTag if the result is an integer, 'FloatTag otherwise
;  Example:
;     (determine-number-tag 5 (list 2 3)) => 'IntTag
;     (determine-number-tag 2.5 (list 5 2)) => 'FloatTag
(define (determine-number-tag result arg-values)
  (cond
    [(or (inexact? result) (ormap inexact? arg-values)) 'FloatTag]
    [(integer? result) 'IntTag]
    [else 'FloatTag]))

;  ensure-numbers
;     Ensures all arguments are numbers and extracts their values as racket numbers
;  Arguments:
;     func-name - Name of the function for error messages
;     args - The argument list to check
;  Returns:
;     A list of numeric values or a RuntimeError if any argument is not a number
;  Example:
;     (ensure-numbers "+" args) => '(1 2 3)
;     (ensure-numbers "+" (list num-value string-value)) => RuntimeError
(define (ensure-numbers func-name args)
  (let loop ([remaining args]
             [result '()])
    (cond
      [(null? remaining) (reverse result)]
      [else
        (let ([val (car remaining)])
          (if (or (int-value? val) (float-value? val))
              (loop (cdr remaining) (cons (RuntimeValue-value val) result))
              (make-runtime-error 
                    (format "~a expects numbers, got: ~a" 
                           func-name (runtime-value->string val)))))])))

;  check-list-arg
;     Ensures an argument is a list and returns its value
;  Arguments:
;     func-name - Name of the function for error messages
;     arg - The argument to check
;  Returns:
;     The list value or a RuntimeError if the argument is not a list
;  Example:
;     (check-list-arg "car" (first args))
(define (check-list-arg func-name arg)
  (with-value-check arg list-value?
                   (lambda (v) (format "~a expects a list, got: ~a" func-name v))))

;  define-builtin!
;     Defines a built-in function in the given environment
;  Arguments:
;     env - The environment to define the function in
;     name - The name of the function
;     impl - The implementation function
;  Returns:
;     void
;  Example:
;     (define-builtin! env "+" numeric-add-impl)
(define (define-builtin! env name impl)
  (define-variable! env name (make-builtin impl name) #f))

;; ===== Built-in Function Implementations =====
;  builtins must be:
;  - receive `RuntimeValue` lists and `Env` as arguments
;  - return `RuntimeValue` or `RuntimeError`
;  - has responsible arity checking

;; I/O Functions
(define (display-impl args env)
  (check-args-count "display" args 1
    (lambda (checked-args)
      (printf "~a" (runtime-value->string (first checked-args)))
      (make-runtime-value 'VoidTag '()))))

(define (read-line-impl args env)
  (check-args-count "read-line" args 0
    (lambda (checked-args)
      (make-runtime-value 'StringTag (read-line)))))

;  flush-impl
;     Flushes the output buffer to ensure immediate display
;  Arguments:
;     args - No arguments expected
;     env - The environment (unused)
;  Returns:
;     Void
(define (flush-impl args env)
  (check-args-count "flush" args 0
    (lambda (checked-args)
      (flush-output)
      (make-runtime-value 'VoidTag '()))))

;  newline-impl
;     Outputs a newline character
;  Arguments:
;     args - No arguments expected
;     env - The environment (unused)
;  Returns:
;     Void
(define (newline-impl args env)
  (check-args-count "newline" args 0
    (lambda (checked-args)
      (newline)
      (make-runtime-value 'VoidTag '()))))


;  set-max-stack-depth!-impl
;     Sets the maximum call stack depth to prevent infinite recursion
;  Arguments:
;     args - One argument: depth (positive integer)
;     env - The environment (unused)
;  Returns:
;     Void
(define (set-max-stack-depth!-impl args env)
  (check-args-count "set-max-stack-depth!" args 1
    (lambda (checked-args)
      (let ([depth-arg (first checked-args)])
        (if (int-value? depth-arg)
            (let ([depth (RuntimeValue-value depth-arg)])
              (if (> depth 0)
                  (begin
                    (set-max-stack-depth! depth)
                    (make-runtime-value 'VoidTag '()))
                  (make-runtime-error (format "set-max-stack-depth! expects a positive integer, got: ~a" depth))))
            (make-runtime-error (format "set-max-stack-depth! expects an integer, got: ~a" 
                               (runtime-value->string depth-arg))))))))

;  get-max-stack-depth-impl
;     Gets the current maximum call stack depth
;  Arguments:
;     args - No arguments expected
;     env - The environment (unused)
;  Returns:
;     The current maximum stack depth as an integer
(define (get-max-stack-depth-impl args env)
  (check-args-count "get-max-stack-depth" args 0
    (lambda (checked-args)
      (make-runtime-value 'IntTag (get-max-stack-depth)))))

;; Arithmetic Operations
(define (add-impl args env)
  (with-error-handling (ensure-numbers "+" args)
    (lambda (nums)
      (let* ([result (apply + nums)]
             [tag (determine-number-tag result nums)])
        (make-runtime-value tag result)))))

; MEMO: support both 1 and 2 arguments
(define (subtract-impl args env)
  (check-args-count "-" args '(1 2)
    (lambda (checked-args)
      (cond
        [(= 1 (length checked-args))
         (with-value-check (first checked-args) 
                          (lambda (v) (or (int-value? v) (float-value? v)))
                          (lambda (v) (format "- expects numbers, got: ~a" v))
           (lambda (val)
             (let* ([result (- 0 val)]
                    [tag (determine-number-tag result (list val))])
               (make-runtime-value tag result))))]
        [(= 2 (length checked-args))
         (with-error-handling (ensure-numbers "-" checked-args)
           (lambda (vals)
             (let* ([x (first vals)]
                    [y (second vals)]
                    [result (- x y)]
                    [tag (determine-number-tag result vals)])
               (make-runtime-value tag result))))]))))

; MEMO: support any number of arguments
(define (multiply-impl args env)
  (with-error-handling (ensure-numbers "*" args)
    (lambda (nums)
      (let* ([result (apply * nums)]
             [tag (determine-number-tag result nums)])
        (make-runtime-value tag result)))))


; MEMO: support only 2 arguments. 
;       This function returns a float number. **NOT AN EXACT NUMBER**
(define (divide-impl args env)
  (check-args-count "/" args 2
    (lambda (checked-args)
      (with-error-handling (ensure-numbers "/" checked-args)
        (lambda (vals)
          (let ([x (first vals)]
                [y (second vals)])
            (if (= y 0)
                (make-runtime-error "Division by zero")
                (make-runtime-value 'FloatTag (exact->inexact (/ x y))))))))))

;; Comparison Operations
(define (make-comparison-impl op-name op)
  (lambda (args env)
    (check-args-count op-name args 2
      (lambda (checked-args)
        (with-error-handling (ensure-numbers op-name checked-args)
          (lambda (vals)
            (let ([result (op (first vals) (second vals))])
              (make-runtime-value 'BooleanTag result))))))))

;; List Operations
;  list-impl
;     Creates a list from the given arguments
;  Arguments:
;     args - Any number of arguments to be put into a list
;     env - The environment (unused)
;  Returns:
;     A new list containing the arguments
(define (list-impl args env)
  (if (null? args)
      (RuntimeValue 'NilValueTag '())
      (let loop ([remaining args])
        (if (null? remaining)
            (RuntimeValue 'NilValueTag '())
            (let* ([first-item (car remaining)]
                   [rest-items (cdr remaining)]
                   [rest-list (loop rest-items)]
                   [pair (make-Pair first-item rest-list)])
              (RuntimeValue 'PairTag pair))))))

;  cons-impl
;     Creates a new pair with the given car and cdr
;  Arguments:
;     args - Two arguments: the car and cdr for the new pair
;     env - The environment (unused)
;  Returns:
;     A new pair
(define (cons-impl args env)
  (check-args-count "cons" args 2
    (lambda (checked-args)
      (let* ([car-val (first checked-args)]
             [cdr-val (second checked-args)]
             [pair (make-Pair car-val cdr-val)])
        (RuntimeValue 'PairTag pair)))))

;  car-impl
;     Returns the car (first element) of a pair
;  Arguments:
;     args - One argument: a pair
;     env - The environment (unused)
;  Returns:
;     The car of the pair
(define (car-impl args env)
  (check-args-count "car" args 1
    (lambda (checked-args)
      (let ([arg (first checked-args)])
        (if (list-value? arg)
            (Pair-car (RuntimeValue-value arg))
            (make-runtime-error (format "car expects a pair, got: ~a" 
                                       (runtime-value->string arg))))))))

;  cdr-impl
;     Returns the cdr (second element) of a pair
;  Arguments:
;     args - One argument: a pair
;     env - The environment (unused)
;  Returns:
;     The cdr of the pair
(define (cdr-impl args env)
  (check-args-count "cdr" args 1
    (lambda (checked-args)
      (let ([arg (first checked-args)])
        (if (list-value? arg)
            (Pair-cdr (RuntimeValue-value arg))
            (make-runtime-error (format "cdr expects a pair, got: ~a" 
                                       (runtime-value->string arg))))))))

;  set-car-impl
;     Sets the car (first element) of a pair to a new value
;  Arguments:
;     args - Two arguments: a pair and a new value
;     env - The environment (unused)
;  Returns:
;     Void
(define (set-car-impl args env)
  (check-args-count "set-car!" args 2
    (lambda (checked-args)
      (let ([pair-arg (first checked-args)]
            [new-val (second checked-args)])
        (if (list-value? pair-arg)
            (begin 
                (set-Pair-car! (RuntimeValue-value pair-arg) new-val)
                (make-runtime-value 'VoidTag '()))
            (make-runtime-error (format "set-car! expects a pair, got: ~a" 
                                       (runtime-value->string pair-arg))))))))

;  set-cdr-impl
;     Sets the cdr (second element) of a pair to a new value
;  Arguments:
;     args - Two arguments: a pair and a new value
;     env - The environment (unused)
;  Returns:
;     Void
(define (set-cdr-impl args env)
  (check-args-count "set-cdr!" args 2
    (lambda (checked-args)
      (let ([pair-arg (first checked-args)]
            [new-val (second checked-args)])
        (if (list-value? pair-arg)
            (begin 
                (set-Pair-cdr! (RuntimeValue-value pair-arg) new-val)
                (make-runtime-value 'VoidTag '()))
            (make-runtime-error (format "set-cdr! expects a pair, got: ~a" 
                                       (runtime-value->string pair-arg))))))))

; null?-impl
;     Checks if the given argument is the empty list
;  Arguments:
;     args - One argument: the value to check
;     env - The environment (unused)
(define (null?-impl args env)
  (check-args-count "null?" args 1
    (lambda (checked-args)
      (let ([arg (first checked-args)])
        (if (list-value? arg)
            (make-runtime-value 'BooleanTag (null? (RuntimeValue-value arg)))
            (make-runtime-error (format "null? expects a list, got: ~a" (runtime-value->string arg))))))))

; string-length-impl
;     Returns the length of a string
;  Arguments:
;     args - One argument: a string
;     env - The environment (unused)
(define (string-length-impl args env)
  (check-args-count "string-length" args 1
    (lambda (checked-args)
      (let ([arg (first checked-args)])
        (if (string-value? arg)
            (make-runtime-value 'IntTag (string-length (RuntimeValue-value arg)))
            (make-runtime-error (format "string-length expects a string, got: ~a" 
                               (runtime-value->string arg))))))))

; substring-impl
;     Returns a substring of a string
;  Arguments:
;     args - Three arguments: a string, start index, and optional end index
;     env - The environment (unused)
(define (substring-impl args env)
  (check-args-count "substring" args '(2 3)
    (lambda (checked-args)
      (let ([arg (first checked-args)]
            [start (second checked-args)]
            [end (if (= (length checked-args) 3) (third checked-args) #f)])
        (if (string-value? arg)
            (let* ([str (RuntimeValue-value arg)]
                   [start-index (RuntimeValue-value start)]
                   [end-index (if end (RuntimeValue-value end) (string-length str))])
              (if (and (integer? start-index) 
                       (integer? end-index)
                       (< start-index end-index)
                       (< end-index (string-length str)))
                  (make-runtime-value 'StringTag 
                    (substring str start-index end-index))
                  (make-runtime-error "substring: invalid indices")))
            (make-runtime-error 
              (format "substring expects a string, got: ~a" 
                     (runtime-value->string arg))))))))

; string-ref-impl
;     Returns the character at a specified index in a string
(define (string-ref-impl args env)
  (check-args-count "string-ref" args 2
    (lambda (checked-args)
      (let ([arg (first checked-args)]
            [index (second checked-args)])
        (if (string-value? arg)
            (let* ([str (RuntimeValue-value arg)]
                   [idx (RuntimeValue-value index)])
              (if (and (integer? idx) 
                       (>= idx 0) 
                       (< idx (string-length str)))
                  (make-runtime-value 'StringTag 
                    (string-ref str idx))
                  (make-runtime-error "string-ref: index out of bounds")))
            (make-runtime-error 
              (format "string-ref expects a string, got: ~a" 
                     (runtime-value->string arg))))))))

; string-append-impl
;     Appends multiple strings together with any number of arguments
(define (string-append-impl args env)
   (let loop ([remaining args]
             [result ""])
     (if (null? remaining)
         (make-runtime-value 'StringTag result)
         (let ([arg (first remaining)])
           (if (string-value? arg)
               (loop (cdr remaining) 
                     (string-append result (RuntimeValue-value arg)))
               (make-runtime-error 
                 (format "string-append expects strings, got: ~a" 
                        (runtime-value->string arg))))))))


; string=?-impl
;     Checks if two strings are equal
(define (string=?-impl args env)
  (check-args-count "string=?" args 2
    (lambda (checked-args)
      (let ([arg1 (first checked-args)]
            [arg2 (second checked-args)])
        (if (and (string-value? arg1) (string-value? arg2))
            (make-runtime-value 'BooleanTag 
              (equal? (RuntimeValue-value arg1) 
                      (RuntimeValue-value arg2)))
            (make-runtime-error 
              (format "string=? expects two strings, got: ~a and ~a" 
                     (runtime-value->string arg1) 
                     (runtime-value->string arg2))))))))
   

;  error-impl
;     Creates a runtime error with a specified message
;  Arguments:
;     args - One or more arguments: an error message string followed by optional values to format
;     env - The environment (unused)
;  Returns:
;     A RuntimeError
(define (error-impl args env)
  (if (null? args)
      (make-runtime-error "Unknown error")
      (let ([msg (first args)]
            [rest (cdr args)])
        (if (string-value? msg)
            (let ([msg-str (RuntimeValue-value msg)])
              (if (null? rest)
                  (make-runtime-error msg-str)
                  (let ([formatted-args (map runtime-value->string rest)])
                    (make-runtime-error (apply format msg-str formatted-args)))))
            (make-runtime-error (format "error expects a string as first argument, got: ~a" 
                              (runtime-value->string msg)))))))

;   make-type-checker
;     Creates a type-checking function for a specific type
(define (make-type-checker name tag)
  (lambda (args env)
    (check-args-count name args 1
      (lambda (checked-args)
        (let ([arg (first checked-args)])
          (if (equal? (RuntimeValue-tag arg) tag)
              (make-runtime-value 'BooleanTag #t)
              (make-runtime-value 'BooleanTag #f)))))))


;  string->number-impl
;     Converts a string to a number
;  Arguments:
;     args - One argument: a string
;     env - The environment (unused)
;  Returns:
;     A number or #f if the string does not represent a valid number
(define (string->number-impl args env)
  (check-args-count "string->number" args 1
    (lambda (checked-args)
      (let ([arg (first checked-args)])
        (if (string-value? arg)
            (let* ([str (RuntimeValue-value arg)]
                   [num (string->number str)])
              (if num
                  (if (integer? num)
                      (make-runtime-value 'IntTag num)
                      (make-runtime-value 'FloatTag num))
                  (make-runtime-value 'BooleanTag #f)))
            (make-runtime-error (format "string->number expects a string, got: ~a" 
                               (runtime-value->string arg))))))))

;  number->string-impl
;     Converts a number to a string
;  Arguments:
;     args - One argument: a number
;     env - The environment (unused)
;  Returns:
;     A string representation of the number
(define (number->string-impl args env)
  (check-args-count "number->string" args 1
    (lambda (checked-args)
      (let ([arg (first checked-args)])
        (if (or (int-value? arg) (float-value? arg))
            (make-runtime-value 'StringTag (number->string (RuntimeValue-value arg)))
            (make-runtime-error (format "number->string expects a number, got: ~a" 
                               (runtime-value->string arg))))))))

;  make-vector-impl
;     Creates a new vector with the specified size and initial value
;  Arguments:
;     args - Two arguments: size (integer) and initial value
;     env - The environment (unused)
;  Returns:
;     A new vector filled with the initial value
(define (make-vector-impl args env)
  (check-args-count "make-vector" args 2
    (lambda (checked-args)
      (let ([size-arg (first checked-args)]
            [init-arg (second checked-args)])
        (if (int-value? size-arg)
            (let ([size (RuntimeValue-value size-arg)])
              (if (>= size 0)
                  (let ([vector (make-Vector size init-arg)])
                    (make-runtime-value 'VectorTag vector))
                  (make-runtime-error (format "make-vector expects a non-negative size, got: ~a" size))))
            (make-runtime-error (format "make-vector expects an integer size, got: ~a" 
                               (runtime-value->string size-arg))))))))

;  vector-ref-impl
;     Returns the element at the specified index in a vector
;  Arguments:
;     args - Two arguments: vector and index (integer)
;     env - The environment (unused)
;  Returns:
;     The element at the specified index
(define (vector-ref-impl args env)
  (check-args-count "vector-ref" args 2
    (lambda (checked-args)
      (let ([vector-arg (first checked-args)]
            [index-arg (second checked-args)])
        (if (vector-value? vector-arg)
            (if (int-value? index-arg)
                (let ([vector (RuntimeValue-value vector-arg)]
                      [index (RuntimeValue-value index-arg)])
                  (let ([elements (Vector-elements vector)])
                    (if (and (>= index 0) (< index (vector-length elements)))
                        (vector-ref elements index)
                        (make-runtime-error (format "vector-ref: index ~a out of bounds for vector of size ~a" 
                                           index (vector-length elements))))))
                (make-runtime-error (format "vector-ref expects an integer index, got: ~a" 
                                   (runtime-value->string index-arg))))
            (make-runtime-error (format "vector-ref expects a vector, got: ~a" 
                               (runtime-value->string vector-arg))))))))

;  vector-set!-impl
;     Sets the element at the specified index in a vector
;  Arguments:
;     args - Three arguments: vector, index (integer), and new value
;     env - The environment (unused)
;  Returns:
;     Void
(define (vector-set!-impl args env)
  (check-args-count "vector-set!" args 3
    (lambda (checked-args)
      (let ([vector-arg (first checked-args)]
            [index-arg (second checked-args)]
            [value-arg (third checked-args)])
        (if (vector-value? vector-arg)
            (if (int-value? index-arg)
                (let ([vector (RuntimeValue-value vector-arg)]
                      [index (RuntimeValue-value index-arg)])
                  (let ([elements (Vector-elements vector)])
                    (if (and (>= index 0) (< index (vector-length elements)))
                        (begin
                          (vector-set! elements index value-arg)
                          (make-runtime-value 'VoidTag '()))
                        (make-runtime-error (format "vector-set!: index ~a out of bounds for vector of size ~a" 
                                           index (vector-length elements))))))
                (make-runtime-error (format "vector-set! expects an integer index, got: ~a" 
                                   (runtime-value->string index-arg))))
            (make-runtime-error (format "vector-set! expects a vector, got: ~a" 
                               (runtime-value->string vector-arg))))))))

;  vector-length-impl
;     Returns the length of a vector
;  Arguments:
;     args - One argument: a vector
;     env - The environment (unused)
;  Returns:
;     The length of the vector as an integer
(define (vector-length-impl args env)
  (check-args-count "vector-length" args 1
    (lambda (checked-args)
      (let ([vector-arg (first checked-args)])
        (if (vector-value? vector-arg)
            (let ([vector (RuntimeValue-value vector-arg)])
              (let ([elements (Vector-elements vector)])
                (make-runtime-value 'IntTag (vector-length elements))))
            (make-runtime-error (format "vector-length expects a vector, got: ~a" 
                               (runtime-value->string vector-arg))))))))

;  vector-copy-impl
;     Creates a copy of a vector
;  Arguments:
;     args - One argument: a vector to copy
;     env - The environment (unused)
;  Returns:
;     A new vector with the same elements
(define (vector-copy-impl args env)
  (check-args-count "vector-copy" args 1
    (lambda (checked-args)
      (let ([vector-arg (first checked-args)])
        (if (vector-value? vector-arg)
            (let* ([vector (RuntimeValue-value vector-arg)]
                   [elements (Vector-elements vector)]
                   [size (vector-length elements)]
                   [new-vector (make-Vector size (make-runtime-value 'IntTag 0))])
              (let loop ([i 0])
                (if (< i size)
                    (begin
                      (vector-set! (Vector-elements new-vector) i (vector-ref elements i))
                      (loop (+ i 1)))
                    (make-runtime-value 'VectorTag new-vector))))
            (make-runtime-error (format "vector-copy expects a vector, got: ~a" 
                               (runtime-value->string vector-arg))))))))

;; ===== Type Information Functions =====

;  type-of-impl
;     Returns the type tag of a runtime value
;  Arguments:
;     args - One argument: the value to check
;     env - The environment (unused)
;  Returns:
;     A symbol representing the type tag
(define (type-of-impl args env)
  (check-args-count "type-of" args 1
    (lambda (checked-args)
      (let ([arg (first checked-args)])
        (make-runtime-value 'SymbolTag (RuntimeValue-tag arg))))))

;; ===== Eval Builtin Functions =====

;  pair->list
;     Converts a RuntimeValue with PairTag to a list
;  Arguments:
;     pair-value - A RuntimeValue with PairTag or NilValueTag
;  Returns:
;     A list containing the values in the pair chain
(define (pair->list pair-value)
  (cond
    [(nil-value? pair-value) '()]
    [(not (pair-value? pair-value))
     (error "Internal error: expected a pair or nil, got: ~a" pair-value)]
    [else
     (let* ([pair (RuntimeValue-value pair-value)]
            [car-value (Pair-car pair)]
            [cdr-value (Pair-cdr pair)])
       (cons car-value (pair->list cdr-value)))]))

;  constant-value?
;     Checks if a RuntimeValue is a constant value
;  Arguments:
;     value - A RuntimeValue to check
;  Returns:
;     #t if the value is a constant (Int, Float, Boolean, String, Nil), #f otherwise
(define (constant-value? value)
  (or (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'IntTag))
      (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'FloatTag))
      (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'BooleanTag))
      (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'StringTag))
      (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'NilValueTag))))

;  special-form?
;  special-form?
;     Checks if a symbol name represents a special form in eta language
;  Arguments:
;     symbol-name - A symbol (not a string) representing a potential special form
;                   Examples: 'if, 'lambda, 'define, etc.
;  Returns:
;     #t if the symbol is a special form, #f otherwise
;  Notes:
;     Uses eta-keyword? from tokenizer.rkt which checks against a predefined list
;     of special forms in eta language
(define (special-form? symbol-name)
  (eta-keyword? symbol-name))
  
;  runtime-value->expr
;     Convert a RuntimeValue to an Expr structure
;  Arguments:
;     value - A RuntimeValue to convert
;     location - Source location to use for created expressions
;     visited - (Optional) List of already visited objects for cycle detection
;  Returns:
;     An Expr representing the runtime value
(define (runtime-value->expr value location [visited '()])
  ;; Check for cycles
  (when (member value visited eq?)
    (error "Circular reference detected in expression"))
  
  (let ([new-visited (cons value visited)])
    (cond
      ;; Integer constant
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'IntTag))
       (make-const location 'IntConstNode (RuntimeValue-value value))]
      
      ;; Float constant
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'FloatTag))
       (make-const location 'FloatConstNode (RuntimeValue-value value))]
      
      ;; Boolean constant
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'BooleanTag))
       (make-const location 'BoolConstNode (RuntimeValue-value value))]
      
      ;; String constant
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'StringTag))
       (make-const location 'StringConstNode (RuntimeValue-value value))]
      
      ;; Nil constant
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'NilValueTag))
       (make-const location 'NilConstNode '())]
      
      ;; Symbol
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'SymbolTag))
       (make-var location (RuntimeValue-value value))]
      
      ;; Pair (S-expression)
      [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'PairTag))
       (let* ([pair (RuntimeValue-value value)]
              [car-value (Pair-car pair)]
              [cdr-value (Pair-cdr pair)])
         ;; Check if this is a special form
         (if (and (symbol-value? car-value)
                  (special-form? (RuntimeValue-value car-value)))
             ;; Handle special forms
             (let ([symbol-name (RuntimeValue-value car-value)])
               (convert-special-form symbol-name cdr-value location new-visited))
             ;; Regular function application
             (let* ([func-expr (runtime-value->expr car-value location new-visited)]
                    [args-list (pair->list cdr-value)]
                    [arg-exprs (map (lambda (arg) (runtime-value->expr arg location new-visited)) args-list)])
               (make-app location func-expr arg-exprs))))]
      
      ;; Other types or error cases
      [else (error "Cannot convert value to expression: ~a" value)])))

;  convert-special-form
;     Convert a special form pair to the appropriate Expr structure
;  Arguments:
;     symbol-name - The name of the special form (if, lambda, etc.) as a symbol or string
;     args - The arguments to the special form (as a RuntimeValue)
;     location - Source location to use for created expressions
;     visited - List of already visited objects for cycle detection
;  Returns:
;     An Expr representing the special form
(define (convert-special-form symbol-name args location visited)
  ;; Work directly with string representation of special form names
  (let ([arg-list (pair->list args)])
    (cond
      ;; if special form
      [(equal? symbol-name "if")
       (let ([arg-count (length arg-list)])
         (cond
           [(= arg-count 2) ;; (if test then)
            (let ([test-expr (runtime-value->expr (first arg-list) location visited)]
                  [then-expr (runtime-value->expr (second arg-list) location visited)])
              (make-ifthen location test-expr then-expr))]
           [(= arg-count 3) ;; (if test then else)
            (let ([test-expr (runtime-value->expr (first arg-list) location visited)]
                  [then-expr (runtime-value->expr (second arg-list) location visited)]
                  [else-expr (runtime-value->expr (third arg-list) location visited)])
              (make-ifthenelse location test-expr then-expr else-expr))]
           [else
            (error "Invalid if expression: expected 2 or 3 arguments, got ~a" arg-count)]))]
      
      ;; lambda special form
      [(equal? symbol-name "lambda")
       (if (>= (length arg-list) 1)
           (let ([params (first arg-list)]
                 [body-exprs (rest arg-list)])
             (let ([param-expr (convert-params-to-arg-expr params location visited)]
                   [body-expr (convert-body-exprs body-exprs location visited)])
               (make-lambda location param-expr body-expr)))
           (error "Invalid lambda expression: expected at least 1 argument"))]
      
      ;; define special form
      [(equal? symbol-name "define")
       (if (>= (length arg-list) 2)
           (let ([name (first arg-list)]
                 [value (second arg-list)])
             (if (symbol-value? name)
                 (let ([name-expr (runtime-value->expr name location visited)]
                       [value-expr (runtime-value->expr value location visited)])
                   (make-define location name-expr value-expr))
                 (error "First argument to define must be a symbol")))
           (error "Invalid define expression: expected at least 2 arguments"))]
      
      ;; set! special form
      [(equal? symbol-name "set!")
       (if (= (length arg-list) 2)
           (let ([name (first arg-list)]
                 [value (second arg-list)])
             (if (symbol-value? name)
                 (let ([name-expr (runtime-value->expr name location visited)]
                       [value-expr (runtime-value->expr value location visited)])
                   (make-setbang location name-expr value-expr))
                 (error "First argument to set! must be a symbol")))
           (error "Invalid set! expression: expected 2 arguments"))]
      
      ;; quote special form
      [(equal? symbol-name "quote")
       (if (= (length arg-list) 1)
           (let ([quoted-value (first arg-list)])
             (make-quote location (convert-to-sexpr quoted-value location visited)))
           (error "Invalid quote expression: expected 1 argument"))]
      
      ;; begin special form
      [(equal? symbol-name "begin")
       (let ([body-exprs (map (lambda (expr) (runtime-value->expr expr location visited)) arg-list)])
         (make-begin location body-exprs))]
      
      ;; and special form
      [(equal? symbol-name "and")
       (let ([and-exprs (map (lambda (expr) (runtime-value->expr expr location visited)) arg-list)])
         (make-and location and-exprs))]
      
      ;; or special form
      [(equal? symbol-name "or")
       (let ([or-exprs (map (lambda (expr) (runtime-value->expr expr location visited)) arg-list)])
         (make-or location or-exprs))]
      
      ;; let special form
      [(equal? symbol-name "let")
       (if (>= (length arg-list) 2)
           (let ([bindings (first arg-list)]
                 [body (rest arg-list)])
             ;; Check if this is a named let
             (if (and (pair-value? bindings)
                      (symbol-value? (let ([pair (RuntimeValue-value bindings)])
                                       (Pair-car pair))))
                 ;; Named let
                 (let ([name (Pair-car (RuntimeValue-value bindings))]
                       [actual-bindings (Pair-cdr (RuntimeValue-value bindings))])
                   (let ([name-expr (runtime-value->expr name location visited)]
                         [bindings-expr (convert-bindings-to-expr actual-bindings location visited)]
                         [body-expr (convert-body-exprs body location visited)])
                     (make-named-let location name-expr bindings-expr body-expr)))
                 ;; Regular let
                 (let ([bindings-expr (convert-bindings-to-expr bindings location visited)]
                       [body-expr (convert-body-exprs body location visited)])
                   (make-unnamed-let location bindings-expr body-expr))))
           (error "Invalid let expression: expected at least 2 arguments"))]
      
      ;; Other special forms - add more as needed
      ;; Example: let*, letrec, cond, etc.
      
      ;; Default case for other special forms (not implemented yet)
      [else (error "Special form not yet implemented: ~a" symbol-name)])))

;  convert-bindings-to-expr
;     Convert a list of bindings in RuntimeValue form to AST bindings
;  Arguments:
;     bindings - A RuntimeValue representing bindings list: ((var1 val1) (var2 val2) ...)
;     location - Source location
;     visited - List of already visited objects for cycle detection
;  Returns:
;     An Expr with BindingsHead
(define (convert-bindings-to-expr bindings location visited)
  (cond
    ;; Empty bindings list
    [(nil-value? bindings)
     (make-bindings location '())]
    
    ;; Process each binding in the list
    [(pair-value? bindings)
     (let* ([binding-list (pair->list bindings)]
            ;; Convert each binding to an AST Bind node
            [bind-exprs 
              (map 
                (lambda (binding)
                  (if (pair-value? binding)
                      (let* ([binding-pair (RuntimeValue-value binding)]
                             [var (Pair-car binding-pair)]
                             [val-pair (Pair-cdr binding-pair)])
                        ;; Ensure variable is a symbol
                        (if (symbol-value? var)
                            (let* ([var-expr (runtime-value->expr var location visited)]
                                   ;; Extract the actual value from the cdr
                                   [val (if (pair-value? val-pair)
                                            (Pair-car (RuntimeValue-value val-pair))
                                            val-pair)]
                                   [val-expr (runtime-value->expr val location visited)])
                              (make-bind location var-expr val-expr))
                            (error "Binding variable must be a symbol")))
                      (error "Invalid binding format: ~a" binding)))
                binding-list)])
       (make-bindings location bind-exprs))]
    
    ;; Invalid bindings format
    [else
     (error "Invalid bindings list: ~a" bindings)]))

;  convert-params-to-arg-expr
;     Convert a parameter specification to an Arg expression
;  Arguments:
;     params - A RuntimeValue representing parameter list
;     location - Source location
;     visited - List of already visited objects for cycle detection
;  Returns:
;     An Expr with ArgHead
(define (convert-params-to-arg-expr params location visited)
  (cond
    ;; Symbol parameter (lambda x ...)
    [(symbol-value? params)
     (let ([param-val (RuntimeValue-value params)])
       (make-single-arg location 
                       (if (string? param-val) 
                           param-val 
                           (symbol->string param-val))))]
    
    ;; List of parameters (lambda (x y z) ...)
    [(or (pair-value? params) (nil-value? params))
     (let ([param-list (pair->list params)])
       ;; Process parameter list and detect dotted params
       (let process-params ([remaining param-list]
                           [processed '()]
                           [found-dotted? #f])
         (cond
           [(null? remaining)
            (make-list-arg location (reverse processed) '())]
           [(and (= (length remaining) 1) (symbol-value? (car remaining)) found-dotted?)
            (let ([param-val (RuntimeValue-value (car remaining))])
              (make-list-arg location (reverse processed) 
                             (if (string? param-val)
                                 param-val
                                 (symbol->string param-val))))]
           [(symbol-value? (car remaining))
            (let ([param-val (RuntimeValue-value (car remaining))])
              (process-params (cdr remaining)
                           (cons (if (string? param-val)
                                     param-val
                                     (symbol->string param-val)) processed)
                           found-dotted?))]
           [else
            (error "Invalid parameter specification")])))]
    
    ;; Invalid parameter
    [else
     (error "Invalid parameter specification")]))

;  convert-body-exprs
;     Convert a list of body expressions to a Body expression
;  Arguments:
;     body-exprs - A list of RuntimeValues for body
;     location - Source location
;     visited - List of already visited objects for cycle detection
;  Returns:
;     An Expr with BodyHead
(define (convert-body-exprs body-exprs location visited)
  ;; Separate defines from other expressions using a functional approach
  (let-values ([(define-exprs other-exprs) 
                (partition (lambda (expr) 
                            (and (pair-value? expr)
                                 (let ([pair (RuntimeValue-value expr)])
                                   (and (symbol-value? (Pair-car pair))
                                        (equal? (RuntimeValue-value (Pair-car pair)) 'define)))))
                          body-exprs)])
    ;; Convert all expressions
    (let ([define-ast-exprs (map (lambda (expr) (runtime-value->expr expr location visited)) define-exprs)]
          [other-ast-exprs (map (lambda (expr) (runtime-value->expr expr location visited)) other-exprs)])
      (make-body location define-ast-exprs other-ast-exprs))))

;  convert-to-sexpr
;     Convert a RuntimeValue to an S-expression Expr
;  Arguments:
;     value - A RuntimeValue to convert
;     location - Source location
;     visited - List of already visited objects for cycle detection
;  Returns:
;     An Expr suitable for a quoted expression
(define (convert-to-sexpr value location visited)
  (cond
    ;; Simple types (integers, strings, etc.)
    [(or (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'IntTag))
         (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'FloatTag))
         (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'BooleanTag))
         (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'StringTag))
         (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'NilValueTag)))
     (runtime-value->expr value location visited)]
    
    ;; Symbol
    [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'SymbolTag))
     (make-var location (symbol->string (RuntimeValue-value value)))]
    
    ;; Pair (for lists and dotted pairs)
    [(and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'PairTag))
     ;; Extract the proper list and potential dotted tail using a helper function
     (let-values ([(items tail) (extract-list-and-tail value '())])
       ;; Convert all items to Expr
       (let ([item-exprs (map (lambda (item) (convert-to-sexpr item location visited)) items)]
             [tail-expr (if (null? tail) 
                            '() 
                            (convert-to-sexpr tail location visited))])
         (make-sexpr location item-exprs tail-expr)))]
    
    ;; Other types or error cases
    [else
     (error "Cannot convert value to s-expression: ~a" value)]))

;  extract-list-and-tail
;     Helper function to extract a proper list and its potential dotted tail
;  Arguments:
;     current - The current pair being processed
;     acc - Accumulated items in the list so far
;  Returns:
;     Two values: the list of items and the tail (if dotted pair)
(define (extract-list-and-tail current acc)
  (cond
    [(nil-value? current)
     (values (reverse acc) '())]
    [(pair-value? current)
     (let* ([pair (RuntimeValue-value current)]
            [car-val (Pair-car pair)]
            [cdr-val (Pair-cdr pair)])
       (if (or (nil-value? cdr-val) (pair-value? cdr-val))
           (extract-list-and-tail cdr-val (cons car-val acc))
           (values (reverse (cons car-val acc)) cdr-val)))]
    [else
     (error "Invalid value in s-expression")]))

;  eval-impl
;     Evaluates a quoted expression provided as a RuntimeValue
;  Arguments:
;     args - List of RuntimeValue objects, expected to contain a single value:
;            - For special forms: a PairTag RuntimeValue representing e.g. (if ...), (lambda ...), etc.
;            - For symbols: a SymbolTag RuntimeValue
;            - For self-evaluating values: IntTag, FloatTag, BooleanTag, or StringTag RuntimeValue
;     env - The Environment object in which to evaluate the expression
;  Returns:
;     Result of evaluating the expression as a RuntimeValue, or RuntimeError in case of errors
;  Example:
;     (eval-impl (list (quote-to-runtime-value '(+ 1 2)))) => RuntimeValue with IntTag 3
;     (eval-impl (list (quote-to-runtime-value 'x)) env-with-x) => value of x from environment
;  Notes:
;     Special forms like if, lambda, etc. are properly handled via runtime-value->expr conversion
;  Returns:
;     Result of evaluating the expression or RuntimeError
(define (eval-impl args env)
  (check-args-count "eval" args 1
    (lambda (checked-args)
      (let ([arg (first checked-args)])
        ;; Ensure argument is a valid value to evaluate
        (cond
          ;; Handle expressions that need to be evaluated (symbols, pairs, nil)
          [(or (symbol-value? arg) (pair-value? arg) (nil-value? arg))
           (let* ([dummy-location (make-dummy-location)]
                  [expr (runtime-value->expr arg dummy-location)])
             ;; Special handling for special forms (if, lambda, etc.)
             (if (and (pair-value? arg)
                      (symbol-value? (let ([pair (RuntimeValue-value arg)])
                                      (Pair-car pair)))
                      ;; Check if this is a special form using the symbol name
                      (let ([symbol (Pair-car (RuntimeValue-value arg))])
                        (and (RuntimeValue? symbol)
                             (equal? (RuntimeValue-tag symbol) 'SymbolTag)
                             (special-form? (RuntimeValue-value symbol)))))
                 ;; Special form - already handled by runtime-value->expr
                 (let ([desugared-expr (desugar (list expr))])
                   (if (EtaError? desugared-expr)
                       desugared-expr
                       (let ([result (eval-toplevel-exprs desugared-expr env)])
                         (if (list? result)
                             (if (null? result)
                                 (make-runtime-value 'VoidTag '())
                                 (first result))
                             result))))
                 ;; Regular expression
                 (let ([desugared-expr (desugar (list expr))])
                   (if (EtaError? desugared-expr)
                       desugared-expr
                       (let ([result (eval-toplevel-exprs desugared-expr env)])
                         (if (list? result)
                             (if (null? result)
                                 (make-runtime-value 'VoidTag '())
                                 (first result))
                             result))))))]
          
          ;; Handle self-evaluating values (numbers, strings, etc.)
          [(or (int-value? arg) 
               (float-value? arg)
               (boolean-value? arg)
               (string-value? arg))
           arg]
          
          ;; Error for other types
          [else
           (make-runtime-error 
            (format "eval cannot evaluate value of type: ~a" 
                    (if (RuntimeValue? arg) 
                        (RuntimeValue-tag arg)
                        (format "~a" arg))))])))))

;  add-builtins-to-env
;     Adds built-in functions to the environment
;  Arguments:
;     env - The environment to add built-ins to
;  Returns:
;     The environment with built-ins added
(define (add-builtins-to-env env)
      ;; Register all built-in functions

    ;; I/O Functions
    (define-builtin! env "display" display-impl)
    (define-builtin! env "read-line" read-line-impl)
    (define-builtin! env "flush" flush-impl)
    (define-builtin! env "newline" newline-impl)
    (define-builtin! env "error" error-impl)
    (define-builtin! env "flush" flush-impl)
    (define-builtin! env "newline" newline-impl)

    ;; Arithmetic Operations
    (define-builtin! env "+" add-impl)
    (define-builtin! env "-" subtract-impl)
    (define-builtin! env "*" multiply-impl)
    (define-builtin! env "/" divide-impl)

    ;; Comparison Operations
    (define-builtin! env "=" (make-comparison-impl "=" =))
    (define-builtin! env "<" (make-comparison-impl "<" <))
    (define-builtin! env ">" (make-comparison-impl ">" >))

    ;; List Operations
    (define-builtin! env "list" list-impl)
    (define-builtin! env "cons" cons-impl)
    (define-builtin! env "car" car-impl)
    (define-builtin! env "cdr" cdr-impl)
    (define-builtin! env "set-car!" set-car-impl)
    (define-builtin! env "set-cdr!" set-cdr-impl)
    (define-builtin! env "null?" null?-impl)
    
    ;; Vector Operations
    (define-builtin! env "make-vector" make-vector-impl)
    (define-builtin! env "vector-ref" vector-ref-impl)
    (define-builtin! env "vector-set!" vector-set!-impl)
    (define-builtin! env "vector-length" vector-length-impl)
    (define-builtin! env "vector-copy" vector-copy-impl)
    
    ;; String Operations
    (define-builtin! env "string-length" string-length-impl)
    (define-builtin! env "substring" substring-impl)
    (define-builtin! env "string-ref" string-ref-impl)
    (define-builtin! env "string-append" string-append-impl)
    (define-builtin! env "string=?" string=?-impl)
    
    ;; Type Checking and Conversion
    (define-builtin! env "int?" (make-type-checker "int?" 'IntTag))
    (define-builtin! env "float?" (make-type-checker "float?" 'FloatTag))
    (define-builtin! env "string?" (make-type-checker "string?" 'StringTag))
    (define-builtin! env "boolean?" (make-type-checker "boolean?" 'BooleanTag))
    (define-builtin! env "nil?" (make-type-checker "nil?" 'NilValueTag))
    (define-builtin! env "pair?" (make-type-checker "pair?" 'PairTag))
    (define-builtin! env "vector?" (make-type-checker "vector?" 'VectorTag))
    (define-builtin! env "builtin?" (make-type-checker "builtin?" 'EtaBuiltinTag))
    (define-builtin! env "closure?" (make-type-checker "closure?" 'EtaClosureTag))
    (define-builtin! env "struct?" (make-type-checker "struct?" 'EtaStructTag))
    (define-builtin! env "undefined?" (make-type-checker "undefined?" 'UndefinedTag))
    (define-builtin! env "void?" (make-type-checker "void?" 'Void))
    
    ;; Type Information Functions
    (define-builtin! env "type-of" type-of-impl)
    
    (define-builtin! env "string->number" string->number-impl)
    (define-builtin! env "number->string" number->string-impl)

    ;; Evaluation
    (define-builtin! env "eval" eval-impl)

    ;; Stack Depth Management
    (define-builtin! env "set-max-stack-depth!" set-max-stack-depth!-impl)
    (define-builtin! env "get-max-stack-depth" get-max-stack-depth-impl)

    env)

;  get-builtin-names
;     Returns a list of all available built-in function names
;  Arguments:
;     None
;  Returns:
;     A list of strings representing the names of all built-in functions
;  Example:
;     (get-builtin-names) => (list "+" "-" "*" "/" "=" "<" ">" "list" "cons" "car" "cdr")
(define (get-builtin-names)
  (list "display" "read-line" "flush" "newline" "load" "error"
        "+" "-" "*" "/" 
        "=" "<" ">"
        "list" "cons" "car" "cdr" "set-car!" "set-cdr!" "null?" "pair?"
        "make-vector" "vector-ref" "vector-set!" "vector-length" "vector?" "vector-copy"
        "string-length" "substring" "string-ref" "string-append" "string=?"
        "apply" "string->number" "number->string"
        "int?" "float?" "string?" "boolean?" "nil?" "vector?" "builtin?" "closure?" "struct?" "undefined?" "void?"
        "type-of" 
        "set-max-stack-depth!" "get-max-stack-depth"
        "eval"))
