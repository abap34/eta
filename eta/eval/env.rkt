#lang racket

(require "../utils/error.rkt"
         "runtime-values.rkt"
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
)

;  Env
;     Environment frame holding variable bindings, link to parent
;  Fields:
;     frame      - hash table mapping name(string) to value(EtaValue)
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
;     Define a new variable in the environment 
;  Arguments:
;     env   - the starting Env
;     name  - variable name
;     value - value
;     breaking? - #t if this is a breaking definition
;  Returns:
;     #t if successful, RuntimeError if name already exists and breaking? is #f
(define (define-variable! env name value breaking?)
(unless (EtaValue? value)
    (make-runtime-error (format "Failed to define variable. Invalid value type for variable: ~a. Expected EtaValue." name)))
  (if (hash-has-key? (Env-frame env) name)
      (if (not breaking?)
          (make-runtime-error (format "Variable already defined: ~a" name))
          (begin
            (hash-set! (Env-frame env) name value)
            value))
      (hash-set! (Env-frame env) name value))
  #t)

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
