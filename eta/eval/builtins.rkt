#lang racket

(require "runtime-values.rkt"
         "env.rkt"
         "../utils/error.rkt")

(provide 
  get-builtin-names
  number-value?
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

;  Type checking functions
;     Check if an EtaValue has a specific tag
;  Arguments:
;     value - An EtaValue to check
;  Returns:
;     #t if the value has the specified tag, #f otherwise
;  Example:
;     (number-value? (make-runtime-value Number 42)) ; => #t
;     (number-value? (make-runtime-value String "hello")) ; => #f

(define (number-value? value)
  (and (EtaValue? value) (eq? (EtaValue-tag value) Number)))

(define (string-value? value)
  (and (EtaValue? value) (eq? (EtaValue-tag value) String)))

(define (boolean-value? value)
  (and (EtaValue? value) (eq? (EtaValue-tag value) Boolean)))

(define (nil-value? value)
  (and (EtaValue? value) (eq? (EtaValue-tag value) NilValue)))

(define (list-value? value)
  (and (EtaValue? value) (eq? (EtaValue-tag value) List)))

(define (expr-value? value)
  (and (EtaValue? value) (eq? (EtaValue-tag value) EtaExpr)))

(define (builtin-value? value)
  (and (EtaValue? value) (eq? (EtaValue-tag value) EtaBuiltin)))

(define (closure-value? value)
  (and (EtaValue? value) (eq? (EtaValue-tag value) EtaClosure)))

(define (struct-value? value)
  (and (EtaValue? value) (eq? (EtaValue-tag value) EtaStruct)))

(define (undefined-value? value)
  (and (EtaValue? value) (eq? (EtaValue-tag value) Undefined)))

(define (void-value? value)
  (and (EtaValue? value) (eq? (EtaValue-tag value) Void)))

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
(define (check-args-count func-name args expected)
  (let ([count (length args)])
    (cond
      [(number? expected) 
       (unless (= count expected)
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
         (when (> count max)
           (make-runtime-error 
                  (format "~a expects at most ~a argument~a, got ~a" 
                         func-name max (if (= max 1) "" "s") count))))])))

;  ensure-numbers
;     Ensures all arguments are numbers and extracts their values
;  Arguments:
;     func-name - Name of the function for error messages
;     args - The argument list to check
;  Returns:
;     A list of numeric values
;  Example:
;     (ensure-numbers "+" args) => '(1 2 3)
(define (ensure-numbers func-name args)
  (map (lambda (arg) 
         (if (eq? (EtaValue-tag arg) Number)
             (EtaValue-value arg)
              (make-runtime-error 
                    (format "~a expects numbers, got: ~a" 
                           func-name (runtime-value->string arg)))))
       args))

;  check-list-arg
;     Ensures an argument is a list and returns its value
;  Arguments:
;     func-name - Name of the function for error messages
;     arg - The argument to check
;  Returns:
;     The list value or raises an error
;  Example:
;     (check-list-arg "car" (first args))
(define (check-list-arg func-name arg)
  (if (eq? (EtaValue-tag arg) List)
      (EtaValue-value arg)
    (make-runtime-error 
             (format "~a expects a list, got: ~a" 
                    func-name (runtime-value->string arg)))))

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
(define (print-impl args env)
  (check-args-count "print" args 1)
  (printf "~a\n" (EtaValue-value (first args)))
  (make-runtime-value Void '()))

(define (display-impl args env)
  (check-args-count "display" args 1)
  (printf "~a" (EtaValue-value (first args)))
  (make-runtime-value Void '()))

(define (newline-impl args env)
  (check-args-count "newline" args 0)
  (newline)
  (make-runtime-value Void '()))

(define (read-line-impl args env)
  (check-args-count "read-line" args 0)
  (make-runtime-value String (read-line)))

;; Arithmetic Operations
(define (add-impl args env)
  (let ([nums (ensure-numbers "+" args)])
    (make-runtime-value Number (apply + nums))))

(define (subtract-impl args env)
  (check-args-count "-" args '(1))
  (cond
    [(= 1 (length args))
     (let ([val (EtaValue-value (first args))])
       (if (number? val)
           (make-runtime-value Number (- val))
           (make-runtime-error 
                  (format "- expects numbers, got: ~a" 
                         (runtime-value->string (first args))))))]
    [else
     (let* ([vals (ensure-numbers "-" args)]
            [first (first vals)]
            [rest (rest vals)])
       (make-runtime-value Number (apply - first rest)))]))

(define (multiply-impl args env)
  (let ([nums (ensure-numbers "*" args)])
    (make-runtime-value Number (apply * nums))))

(define (divide-impl args env)
  (check-args-count "/" args '(1))
  (cond
    [(= 1 (length args))
     (let ([val (EtaValue-value (first args))])
       (if (number? val)
           (if (= val 0)
              (make-runtime-error "Division by zero")
               (make-runtime-value Number (/ 1 val)))
            (make-runtime-error 
                  (format "/ expects numbers, got: ~a" 
                         (runtime-value->string (first args))))))]
    [else
     (let* ([vals (ensure-numbers "/" args)]
            [first (first vals)]
            [rest (rest vals)])
       (if (member 0 rest)
           (make-runtime-error "Division by zero")
           (make-runtime-value Number (apply / first rest))))]))

;; Comparison Operations
(define (make-comparison-impl op-name op)
  (lambda (args env)
    (check-args-count op-name args '(2))
    (let* ([vals (ensure-numbers op-name args)]
           [result (for/and ([i (in-range (sub1 (length vals)))])
                     (op (list-ref vals i) (list-ref vals (add1 i))))])
      (make-runtime-value Boolean result))))

;; List Operations
(define (list-impl args env)
  (make-runtime-value List args))

(define (cons-impl args env)
  (check-args-count "cons" args 2)
  (let ([item (first args)]
        [lst (second args)])
    (let ([lst-val (check-list-arg "cons" lst)])
      (make-runtime-value List (cons item lst-val)))))

(define (car-impl args env)
  (check-args-count "car" args 1)
  (let ([lst-val (check-list-arg "car" (first args))])
    (if (null? lst-val)
        (make-runtime-error "car called on empty list")
        (first lst-val))))

(define (cdr-impl args env)
  (check-args-count "cdr" args 1)
  (let ([lst-val (check-list-arg "cdr" (first args))])
    (if (null? lst-val)
       (make-runtime-error "cdr called on empty list")
        (make-runtime-value List (rest lst-val)))))

;  add-builtins-to-env
;     Adds built-in functions to the environment
;  Arguments:
;     env - The environment to add built-ins to
;  Returns:
;     The environment with built-ins added
(define (add-builtins-to-env env)
      ;; Register all built-in functions

    ;; I/O Functions
    (define-builtin! env "print" print-impl)
    (define-builtin! env "display" display-impl)
    (define-builtin! env "newline" newline-impl)
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
  (list "print" "display" "newline" "read-line"
        "+" "-" "*" "/" 
        "=" "<" ">"
        "list" "cons" "car" "cdr"))