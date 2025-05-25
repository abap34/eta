#lang racket

(require "runtime-values.rkt"
         "env.rkt"
         "../utils/error.rkt"
         rnrs/mutable-pairs-6)

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
  (and (RuntimeValue? value) (equal? (RuntimeValue-tag value) 'PairTag)))

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
(define (list-impl args env)
  (make-runtime-value 'ListTag args))

(define (cons-impl args env)
  (check-args-count "cons" args 2
    (lambda (checked-args)
      (let ([item (first checked-args)]
            [lst (second checked-args)])
        (with-error-handling (check-list-arg "cons" lst)
          (lambda (lst-val)
            (make-runtime-value 'ListTag (cons item lst-val))))))))

(define (car-impl args env)
  (check-args-count "car" args 1
    (lambda (checked-args)
      (with-error-handling (check-list-arg "car" (first checked-args))
        (lambda (lst-val)
          (if (null? lst-val)
              (make-runtime-error "car called on empty list")
              (first lst-val)))))))

(define (cdr-impl args env)
  (check-args-count "cdr" args 1
    (lambda (checked-args)
      (with-error-handling (check-list-arg "cdr" (first checked-args))
        (lambda (lst-val)
          (if (null? lst-val)
             (make-runtime-error "cdr called on empty list")
              (make-runtime-value 'ListTag (rest lst-val))))))))

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
        "list" "cons" "car" "cdr"))
