#lang racket

(provide make-env env-extend env-lookup env-set! NotFound? make-not-found NotFound-name)

(require "../utils/error.rkt")

;  NotFound
;     Represents a variable not found in the environment.
;  Arguments:
;      name - The name of the variable that was not found.
;  Returns:
;      A NotFound struct representing the missing variable.
;  Example:
;      (make-not-found 'x) ; => (NotFound 'x)
(struct NotFound (name) #:transparent)
(define (make-not-found name) (NotFound name))

;  make-env
;     Creates a new environment.
;  Arguments:
;      parent - The parent environment (or #f for the global environment).
;  Returns:
;      A new environment object.
;  Example:
;      (define global-env (make-env #f))
;  Notes:
;      Environments are implemented as hash tables with optional parent chaining.
(define (make-env parent)
  (make-hash (list (cons 'parent parent))))

;  env-extend
;     Extends an environment with new bindings.
;  Arguments:
;      env - The environment to extend.
;      bindings - A list of (key . value) pairs to add.
;  Returns:
;      A new extended environment.
;  Example:
;      (define extended-env (env-extend global-env '((x . 10) (y . 20))))
;  Notes:
;      This creates a new environment with the given bindings.
(define (env-extend env bindings)
  (let ([new-env (make-env env)])
    (for-each (lambda (binding)
                (hash-set! new-env (car binding) (cdr binding)))
              bindings)
    new-env))

;  env-lookup
;     Looks up a value in the environment.
;  Arguments:
;      env - The environment to search.
;      key - The key to look up.
;  Returns:
;      The value associated with the key, or a NotFound struct if not found.
;  Example:
;      (env-lookup global-env 'x) ; => 10 or (NotFound 'x)
;  Notes:
;      Searches parent environments if the key is not found in the current one.
(define (env-lookup env key)
  (cond
    [(hash-has-key? env key) (hash-ref env key)]
    [(hash-ref env 'parent #f) (env-lookup (hash-ref env 'parent) key)]
    [else (make-not-found key)]))

;  env-set!
;     Sets a value in the environment.
;  Arguments:
;      env - The environment to modify.
;      key - The key to set.
;      value - The value to associate with the key.
;  Returns:
;      None.
;  Example:
;      (env-set! global-env 'x 42)
;  Notes:
;      Modifies the current environment only (does not affect parent environments).
(define (env-set! env key value)
  (hash-set! env key value))