#lang racket

(provide run-eval-tests)
(require "test-utils.rkt"
         "../eta/parser/ast.rkt"
         "../eta/parser/tokenizer.rkt"
         "../eta/parser/parser.rkt"
         "../eta/eval/env.rkt"
         "../eta/eval/eval.rkt")

;  test-constant-eval
;     Tests evaluation of constants (numbers, booleans, strings).
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-constant-eval state output-fn)
  (output-fn "Running test-constant-eval...")
  
  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))
  
  (let* ([env (make-env #f)]
         [num-expr (make-expr Const (list 42) (Location 1 1 1 3))]
         [bool-expr (make-expr Const (list #t) (Location 1 1 1 3))]
         [str-expr (make-expr Const (list "hello") (Location 1 1 1 8))])
    
    (set! state (assert (eta-eval num-expr env) 42 "Number constant evaluation"))
    (set! state (assert (eta-eval bool-expr env) #t "Boolean constant evaluation"))
    (set! state (assert (eta-eval str-expr env) "hello" "String constant evaluation")))
  
  state)

;  test-variable-lookup
;     Tests variable lookup in environments.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-variable-lookup state output-fn)
  (output-fn "Running test-variable-lookup...")
  
  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))
  
  (let* ([env (make-env #f)]
         [var-x (make-expr Var (list "x") (Location 1 1 1 2))]
         [var-y (make-expr Var (list "y") (Location 1 1 1 2))]
         [var-z (make-expr Var (list "z") (Location 1 1 1 2))])
    
    ;; Set up environment
    (env-set! env "x" 10)
    (env-set! env "y" "hello")
    
    ;; Test lookups
    (set! state (assert (eta-eval var-x env) 10 "Integer variable lookup"))
    (set! state (assert (eta-eval var-y env) "hello" "String variable lookup"))
    
    ;; Test nested environments
    (let* ([nested-env (make-env env)]
           [shadowed-env (make-env env)])
      
      (env-set! nested-env "z" #t)
      (env-set! shadowed-env "x" 20)
      
      (set! state (assert (eta-eval var-z nested-env) #t "Nested environment lookup"))
      (set! state (assert (eta-eval var-x nested-env) 10 "Parent environment lookup"))
      (set! state (assert (eta-eval var-x shadowed-env) 20 "Shadowed variable lookup"))))
  
  state)

;  test-arithmetic-ops
;     Tests basic arithmetic operations (+, -, *, /).
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-arithmetic-ops state output-fn)
  (output-fn "Running test-arithmetic-ops...")
  
  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))
  
  (let* ([env (make-env #f)]
         [parse-eval (lambda (expr)
                       (eta-eval (parse (tokenize expr)) env))])
    
    ;; Set up environment
    (env-set! env "+" '+)
    (env-set! env "-" '-)
    (env-set! env "*" '*)
    (env-set! env "/" '/)
    (env-set! env "x" 10)
    (env-set! env "y" 5)
    
    ;; Test simple arithmetic
    (set! state (assert (parse-eval "(+ 1 2)") 3 "Addition test"))
    (set! state (assert (parse-eval "(- 5 3)") 2 "Subtraction test"))
    (set! state (assert (parse-eval "(* 4 5)") 20 "Multiplication test"))
    (set! state (assert (parse-eval "(/ 10 2)") 5 "Division test"))
    
    ;; Test with variables
    (set! state (assert (parse-eval "(+ x y)") 15 "Addition with variables"))
    (set! state (assert (parse-eval "(- x y)") 5 "Subtraction with variables"))
    (set! state (assert (parse-eval "(* x y)") 50 "Multiplication with variables"))
    (set! state (assert (parse-eval "(/ x y)") 2 "Division with variables"))
    
    ;; Test nested operations
    (set! state (assert (parse-eval "(+ 1 (* 2 3))") 7 "Nested arithmetic 1"))
    (set! state (assert (parse-eval "(* (+ 2 3) (- 8 3))") 25 "Nested arithmetic 2"))
    (set! state (assert (parse-eval "(/ (+ x y) 5)") 3 "Nested arithmetic with variables")))
  
  state)

;  test-quote
;     Tests quote expressions.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-quote state output-fn)
  (output-fn "Running test-quote...")
  
  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))
  
  (let* ([env (make-env #f)]
         [quoted-var (make-expr Quote 
                               (list (make-expr Var (list "x") (Location 1 2 1 3)))
                               (Location 1 1 1 3))]
         [quoted-num (make-expr Quote 
                               (list (make-expr Const (list 42) (Location 1 2 1 4)))
                               (Location 1 1 1 4))])
    
    ;; Set up environment
    (env-set! env "x" 10)
    
    ;; Test quote - should return the expression itself, not evaluate it
    (set! state (assert (eta-eval quoted-var env) 
                        (make-expr Var (list "x") (Location 1 2 1 3)) 
                        "Quoted variable"))
    
    (set! state (assert (eta-eval quoted-num env) 
                        (make-expr Const (list 42) (Location 1 2 1 4)) 
                        "Quoted number")))
  
  state)

;  run-eval-tests
;     Runs all evaluator tests.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (run-eval-tests state output-fn)
  (output-fn "Running evaluator tests...")
  (let ([out (make-indented-output-fn output-fn 1)])
    (for/fold ([s state])
              ([f (list 
                   test-constant-eval
                   test-variable-lookup
                   test-arithmetic-ops
                   test-quote)])
      (with-error-handling (lambda () (f s out))
        (symbol->string (object-name f)) s out))))