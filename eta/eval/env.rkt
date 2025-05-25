#lang racket

(require "../utils/error.rkt"
        "../utils/console.rkt"
         "runtime-values.rkt"
         racket/list
        )

(provide 
  init-toplevel-env
  copy-env
  env-lookup
  make-child-env
  define-variable!
  define-variable
  is-defined?
  assign-params
  defined?
  pretty-print-Env
)

;  Env
;     Environment frame holding variable bindings, link to parent
;  Fields:
;     frame      - hash table mapping name(string) to value(RuntimeValue)
;     parent     - enclosing Env or #f
;  Note:
;     Env only contains the newly defined variables.
;     For example, if a variable is defined in the top-level
;     environment, it will not be present in the child environments.
(struct Env (frame parent) #:transparent)

;  init-toplevel-env
;     Create a new top-level environment
;  Arguments:
;     None
;  Returns:
;     An Env with empty frame, no parent, and empty 
(define (init-toplevel-env)
  (Env (make-hash) #f))

; copy-env
;     Create a copy of an existing environment
;  Arguments:
;     env - the Env to copy
;  Returns:
;     a new Env with the same frame and parent
(define (copy-env env)
  (Env (hash-copy (Env-frame env)) (Env-parent env)))

;  env-lookup
;     Lookup a variable in the environment chain
;  Arguments:
;     env  - the starting Env
;     name - variable name (symbol or string)
;  Returns:
;     the bound value or raises RuntimeError
(define (env-lookup env name)
  (let loop ([e env])
    (cond
      [(not e)  (make-runtime-error (format "Undefined variable: ~a" name))]
      [(hash-has-key? (Env-frame e) name) (hash-ref (Env-frame e) name)]
      [else (loop (Env-parent e))])))

  
;  defined?
;     Check if a variable is defined in the environment
;  Arguments:
;     env  - the Env to inspect
;     name - variable name (symbol or string)
;  Returns:
;     #t if defined, #f otherwise
(define (defined? env name)
  (let loop ([e env])
    (cond
      [(not e) #f]
      [(hash-has-key? (Env-frame e) name) #t]
      [else (loop (Env-parent e))])))


;  make-child-env
;     Create a new child environment from the parent
;  Arguments:
;     parent - the parent Env
;  Returns:
;     a new Env with an empty frame and the parent as its parent
(define (make-child-env parent)
  (Env (make-hash) parent))


; define-variable!
;     Define or set a variable in the environment 
;  Arguments:
;     env   - the starting Env
;     name  - variable name
;     value - value
;     breaking? - #t if this is for set! (finds variable in parent scopes)
;                 #f if this is for define (only uses current env)
;  Returns:
;     #t if successful, RuntimeError if:
;     - For define (breaking? = #f): name already exists in current env
;     - For set! (breaking? = #t): variable not found in any scope
;     - For both: value is not an RuntimeValue
(define (define-variable! env name value breaking?)
  (unless (RuntimeValue? value)
      (make-runtime-error (format "Failed to define variable. Invalid value type for variable: ~a. Expected RuntimeValue." name)))
  
  (if breaking?
      (let loop ([e env])
        (cond
          [(not e) (make-runtime-error (format "Cannot set undefined variable: ~a" name))]
          [(hash-has-key? (Env-frame e) name) 
           (begin
             (hash-set! (Env-frame e) name value)
             #t)]
          [else (loop (Env-parent e))]))
      
      ; define behavior: only check in **current** env
      (if (hash-has-key? (Env-frame env) name)
          (make-runtime-error (format "Variable already defined: ~a" name))
          (begin
            (hash-set! (Env-frame env) name value)
            #t))))

; define-variable
;    Define a new variable in the environment (non-breaking)
;  Arguments:
;     env   - the starting Env
;     name  - variable name
;     value - value
;     breaking? - #f
;  Returns:
;     New env with the new variable added if successful, RuntimeError if name already exists and breaking? is #f
(define (define-variable env name value breaking?)
  (let ([new-env (copy-env env)])
        (define-variable! new-env name value breaking?)))

;  is-defined?
;     Check if a variable is defined in env or ancestors
;  Arguments:
;     env  - the Env to inspect
;     name - variable name
;  Returns:
;     #t if defined, #f otherwise
(define (is-defined? env name)
  (let loop ([e env])
    (cond
      [(not e) #f]
      [(hash-has-key? (Env-frame e) name) #t]
      [else (loop (Env-parent e))])))


; assign-params
;     Assign values to parameters based on the parameter specification
; Arguments:
;    param-spec - A ParamSpec object
;    args - A list of arguments
;    env - The environment to assign the parameters in
; Returns:
;    A list of assigned values
; Example:
;    (assign-params (make-param-spec (list "x" "y") "z") (list 1 2 3 4) env)
;    ; => "x" => 1, "y" => 2, "z" => (list 3 4) are assigned in `env`
;    (assign-params (make-param-spec (list "x" "y") #f) (list 1 2) env)
;    ; => "x" => 1, "y" => 2 are assigned in `env`
(define (assign-params param-spec args env)
  (unless (arity-check param-spec args)
    (error "Internal error: arity mismatch in assign-params"))

 (define (assign-each param-names args env)
    (if (= (length param-names) 1)
        (let ([last-param-name (car param-names)])
          (define-variable! env last-param-name args))
        (let ([param-name (car param-names)]
              [arg-value (car args)])
          (define-variable! env param-name arg-value)
          (assign-each (cdr param-names) (cdr args) env))))

  (assign-each (ParamSpec-required param-spec) args (make-child-env env))
  )


;  pretty-print-Env
;     Pretty-print the environment structure showing all defined variables
;  Arguments:
;     env - Environment to print
;     show-builtins? - Whether to show built-in functions (default: #f)
;  Returns:
;     void (prints to stdout)
(define (pretty-print-Env env [show-builtins? #f])
  (displayln (colorize "Current Environment:" 'magenta))
  (define (print-env-frame frame prefix is-toplevel?)
    (define bindings (sort (hash->list frame) string<? #:key car))
    (cond
      [(null? bindings)
       (displayln (format "~a(empty)" prefix))]
      [else
       (for ([binding bindings])
         (define name (car binding))
         (define value (cdr binding))
         (when (or show-builtins? 
                   (not (and (RuntimeValue? value)
                             (equal? (RuntimeValue-tag value) 'EtaBuiltinTag))))
           (displayln (format "~a~a: ~a" 
                             prefix
                             (colorize name 'green)
                             (colorize (runtime-value->string value) 'cyan)))))]))
  
  (define (traverse-env env level)
    (when env
      (let ([frame (Env-frame env)]
            [parent (Env-parent env)]
            [prefix (make-string (* level 2) #\space)])
        (if (= level 0)
            (displayln (format "~a└─ Current scope:" prefix))
            (displayln (format "~a├─ Parent scope ~a:" prefix level)))
        (print-env-frame frame (string-append prefix "   ") (= level 0))
        (when parent
          (traverse-env parent (+ level 1))))))
  
  (traverse-env env 0)
  (newline))