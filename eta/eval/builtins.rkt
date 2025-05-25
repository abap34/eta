#lang racket

(require "runtime-values.rkt"
         "env.rkt"
         "../utils/error.rkt"
)


(provide 
  get-builtin-names
  int-value?
  float-value?
  string-value?
  boolean-value?
  nil-value?
  list-value?
  expr-value?
  builtin-value?
  closure-value?
  struct-value?
  undefined-value?
  void-value?
  add-builtins-to-env
)


;  with-error-handling
;     A utility function that handles runtime errors properly
;  Arguments:
;     result - The result of a computation that might be a RuntimeError
;     proc - A function to apply to the result if it's not an error
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

(define (expr-value? value)
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'EtaExprTag)))

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
  (define-variable! env name (make-builtin impl) #f))

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
  (list "display" "read-line"
        "+" "-" "*" "/" 
        "=" "<" ">"
        "list" "cons" "car" "cdr" "set-car!" "set-cdr!"))
