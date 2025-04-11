#lang racket

(provide run-repl-tests)
(require "test-utils.rkt"
         "../eta/parser/ast.rkt"
         "../eta/parser/tokenizer.rkt"
         "../eta/parser/parser.rkt"
         "../eta/eval/env.rkt"
         "../eta/eval/eval.rkt"
         "../eta/repl/repl.rkt")

;  test-init-environment
;     Tests that the REPL initialization properly sets up the global environment.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-init-environment state output-fn)
  (output-fn "Running test-init-environment...")
  
  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))
  
  ;; Create a testing mock of initialize-env
  (let* ([test-env (make-env #f)]
         [initialized-env (initialize-env test-env)])
    
    ;; Verify environment was initialized (not returned as #f)
    (set! state (assert (not (false? initialized-env)) #t "Environment initialization returns non-false value"))
    
    ;; Test that arithmetic operators are defined
    (set! state (assert (env-lookup initialized-env "+") '+ "Addition operator defined"))
    (set! state (assert (env-lookup initialized-env "-") '- "Subtraction operator defined"))
    (set! state (assert (env-lookup initialized-env "*") '* "Multiplication operator defined"))
    (set! state (assert (env-lookup initialized-env "/") '/ "Division operator defined")))
  
  state)

;  test-parse-input
;     Tests that parsing input works correctly in REPL.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
;  Notes:
;      This test requires a custom version of parse-input that takes 
;      a string and returns the AST without any IO side effects.
(define (parse-input-for-test input)
  (parse (tokenize input)))

(define (test-parse-input state output-fn)
  (output-fn "Running test-parse-input...")
  
  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))
  
  ;; Test basic expressions
  (set! state (assert (Expr-head (parse-input-for-test "42")) Const 
                      "Parsing constant in REPL"))
  
  (set! state (assert (Expr-head (parse-input-for-test "x")) Var 
                      "Parsing variable in REPL"))
  
  (set! state (assert (Expr-head (parse-input-for-test "(+ 1 2)")) App 
                      "Parsing application in REPL"))
  
  (set! state (assert (Expr-head (parse-input-for-test "'x")) Quote 
                      "Parsing quote in REPL"))
  
  state)

;  test-simple-eval-loop
;     Tests basic evaluation loop functionality.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-simple-eval-loop state output-fn)
  (output-fn "Running test-simple-eval-loop...")
  
  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))
  
  (let* ([env (make-env #f)]
         [initialized-env (initialize-env env)]
         
         ;; A simplified version of the REPL evaluation loop for testing
         [eval-input (lambda (input)
                       (eta-eval (parse-input-for-test input) initialized-env))])
    
    ;; Test basic arithmetic expressions
    (set! state (assert (eval-input "10") 10 "Simple constant evaluation in REPL"))
    (set! state (assert (eval-input "(+ 1 2)") 3 "Simple addition in REPL"))
    (set! state (assert (eval-input "(- 5 3)") 2 "Simple subtraction in REPL"))
    (set! state (assert (eval-input "(* 4 5)") 20 "Simple multiplication in REPL"))
    (set! state (assert (eval-input "(/ 10 2)") 5 "Simple division in REPL"))
    
    ;; Set a variable and test
    (env-set! initialized-env "x" 42)
    (set! state (assert (eval-input "x") 42 "Variable lookup in REPL"))
    (set! state (assert (eval-input "(+ x 8)") 50 "Variable usage in REPL")))
  
  state)

;  run-repl-tests
;     Runs all REPL-related tests.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (run-repl-tests state output-fn)
  (output-fn "Running REPL tests...")
  (let ([out (make-indented-output-fn output-fn 1)])
    (for/fold ([s state])
              ([f (list 
                   test-init-environment
                   test-parse-input
                   test-simple-eval-loop)])
      (with-error-handling (lambda () (f s out))
        (symbol->string (object-name f)) s out))))