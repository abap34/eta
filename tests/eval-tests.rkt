#lang racket

(provide run-eval-tests)
(require "test-utils.rkt"
         "../eta/eval/interp.rkt"
         "../eta/eval/env.rkt"
         "../eta/eval/runtime-values.rkt"
         "../eta/eval/interp-interface.rkt"
         "../eta/parser/ast.rkt"
         "../eta/parser/tokenizer.rkt"
         "../eta/parser/parser.rkt"
         "../eta/desugar/desugar.rkt"
         "../eta/utils/error.rkt"
         "../eta/utils/location.rkt")

;; Helper function to evaluate eta code string and get result
(define (eval-eta-string input)
  (let* ([env (init-basic-env)]
         [result (eta-eval env input)])
    result))

;; Helper function for extracting value from successful eval result
(define (get-value-from-eval result [getter first])
  (if (EvalResult-success? result)
      (getter (EvalResult-value result))
      (error "Evaluation failed")))

;; Helper function to check if evaluation failed with error
(define (check-eval-error result)
  (not (EvalResult-success? result)))

;; test-simple-expressions
;;    Tests for evaluating basic expressions like constants and simple arithmetic
(define (test-simple-expressions state output-fn)
  (output-fn "Running test-simple-expressions...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Integer constant
  (let* ([input "42"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Integer constant evaluation tag test"))
    (set! state (assert (RuntimeValue-value value) 42 "Integer constant evaluation value test")))

  ;; Float constant
  (let* ([input "3.14"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'FloatTag "Float constant evaluation tag test"))
    (set! state (assert (RuntimeValue-value value) 3.14 "Float constant evaluation value test")))

  ;; String constant
  (let* ([input "\"hello\""]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'StringTag "String constant evaluation tag test"))
    (set! state (assert (RuntimeValue-value value) "hello" "String constant evaluation value test")))

  ;; Boolean constants
  (let* ([input "#t"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'BooleanTag "Boolean true evaluation tag test"))
    (set! state (assert (RuntimeValue-value value) #t "Boolean true evaluation value test")))

  (let* ([input "#f"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'BooleanTag "Boolean false evaluation tag test"))
    (set! state (assert (RuntimeValue-value value) #f "Boolean false evaluation value test")))

  ;; Simple addition
  (let* ([input "(+ 1 2)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Addition evaluation tag test"))
    (set! state (assert (RuntimeValue-value value) 3 "Addition evaluation value test")))

  ;; Simple subtraction
  (let* ([input "(- 5 2)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Subtraction evaluation tag test"))
    (set! state (assert (RuntimeValue-value value) 3 "Subtraction evaluation value test")))

  ;; Simple multiplication
  (let* ([input "(* 2 3)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Multiplication evaluation tag test"))
    (set! state (assert (RuntimeValue-value value) 6 "Multiplication evaluation value test")))

  ;; Simple division
  (let* ([input "(/ 6 2)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'FloatTag "Division evaluation tag test"))
    (set! state (assert (RuntimeValue-value value) 3.0 "Division evaluation value test")))

  ;; Division by zero (should fail)
  (let* ([input "(/ 6 0)"]
         [result (eval-eta-string input)])
    (set! state (assert (check-eval-error result) #t "Division by zero error test")))

  state)

;; test-variables-and-bindings
;;    Tests for variable definition, binding, and access
(define (test-variables-and-bindings state output-fn)
  (output-fn "Running test-variables-and-bindings...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Set! undefined variable (should fail)
  (let* ([input "(set! undefined-var 42)"]
         [result (eval-eta-string input)])
    (set! state (assert (check-eval-error result) #t "Set! undefined variable error test")))

  ;; Let binding
  (let* ([input "(let ((x 1) (y 2)) (+ x y))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) 3 "Let binding test")))

  ;; Let with shadowing
  (let* ([input "(let ((x 5)) (let ((x 10)) x))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) 10 "Let shadowing test")))

  ;; Advanced let shadowing test - nested inner values
  (let* ([input "(let ((x 5)) (let ((x 10)) (let ((x 15)) x)))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) 15 "Deeply nested let shadowing test")))

  ;; Advanced let shadowing test - access outer values
  (let* ([input "(let ((x 5)) (let ((y 10)) x))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) 5 "Let accessing outer scope test")))

  ;; Advanced let shadowing with set! - modify inner value
  (let* ([input "(let ((x 5)) (let ((x 10)) (set! x 20) x))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) 20 "Let with set! inner value test")))

  ;; Advanced let shadowing with set! - inner set! doesn't affect outer value
  (let* ([input "(let ((x 5)) (let ((x 10)) (set! x 20)) x)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) 5 "Let inner set! isolation test")))

  ;; Let* sequentiality
  (let* ([input "(let* ((x 1) (y (+ x 1))) (+ x y))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) 3 "Let* sequential binding test")))

  state)

;; test-conditionals
;;    Tests for conditional expressions (if, cond, and, or)
(define (test-conditionals state output-fn)
  (output-fn "Running test-conditionals...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple if with true condition
  (let* ([input "(if #t 1 2)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) 1 "If with true condition test")))

  ;; Simple if with false condition
  (let* ([input "(if #f 1 2)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) 2 "If with false condition test")))

  ;; If with computed condition
  (let* ([input "(if (> 5 3) \"yes\" \"no\")"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) "yes" "If with computed true condition test")))

  ;; If with non-boolean condition (should fail)
  (let* ([input "(if 42 1 2)"]
         [result (eval-eta-string input)])
    (set! state (assert (check-eval-error result) #t "If with non-boolean condition error test")))

  ;; cond with multiple clauses
  (let* ([input "(cond ((< 3 2) \"case 1\") ((> 5 1) \"case 2\") (else \"default\"))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) "case 2" "Cond with multiple clauses test")))

  ;; cond with else clause
  (let* ([input "(cond ((< 3 2) \"case 1\") (#f \"case 2\") (else \"default\"))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) "default" "Cond with else clause test")))

  ;; and with all true values
  (let* ([input "(and #t #t #t)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) #t "And with all true values test")))

  ;; and with false value
  (let* ([input "(and #t #f #t)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) #f "And with false value test")))

  ;; or with true value
  (let* ([input "(or #f #t #f)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) #t "Or with true value test")))

  ;; or with all false values
  (let* ([input "(or #f #f #f)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) #f "Or with all false values test")))

  state)

;; test-functions
;;    Tests for function definition, application, and closures
(define (test-functions state output-fn)
  (output-fn "Running test-functions...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple lambda definition and application
  (let* ([input "((lambda (x) (* x x)) 5)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Lambda application tag test"))
    (set! state (assert (RuntimeValue-value value) 25 "Lambda application value test")))

  ;; Lambda with multiple parameters
  (let* ([input "((lambda (x y) (+ x y)) 3 4)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) 7 "Lambda with multiple parameters test")))

  ;; Lambda with too few arguments (should fail)
  (let* ([input "((lambda (x y) (+ x y)) 3)"]
         [result (eval-eta-string input)])
    (set! state (assert (check-eval-error result) 
                       #t "Lambda with too few arguments error test")))

  ;; Lambda with too many arguments (should fail)
  (let* ([input "((lambda (x) (* x x)) 5 6)"]
         [result (eval-eta-string input)])
    (set! state (assert (check-eval-error result) 
                       #t "Lambda with too many arguments error test")))

  ;; Function with lexical closure
  (let* ([input "(let ((x 10)) (let ((f (lambda (y) (+ x y)))) (f 5)))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) 15 "Function with lexical closure test")))

  ;; Named function with recursion
  (let* ([input "
  (define fact 
    (lambda (n) 
      (if (= n 0) 
          1 
          (* n (fact (- n 1))))))
  (fact 5)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result second)])
    (set! state (assert (RuntimeValue-value value) 120 "Named recursive function test")))

  ;; Mutual recursion with letrec
  (let* ([input "
(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
         (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
  (even? 10))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-value value) #t "Mutual recursion with letrec test")))

  ;; Function with global variable access
  (let* ([input "
(define x 10)
(define get-x (lambda () x))
(get-x)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result third)])
    (set! state (assert (RuntimeValue-value value) 10 "Function accessing global variable test")))

  ;; Function with global variable set!
  (let* ([input "
(define x 10)
(define set-x (lambda (val) (set! x val)))
(set-x 20)
x"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result fourth)])
    (set! state (assert (RuntimeValue-value value) 20 "Function modifying global variable test")))

  ;; Nested function with variable shadowing
  (let* ([input "
(define x 10)
(define nested-fn 
  (lambda () 
    (let ((x 20))
      (lambda () x))))
((nested-fn))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result third)])
    (set! state (assert (RuntimeValue-value value) 20 "Nested function with shadowing test")))

  ;; Variable shadowing and set! in nested functions
  (let* ([input "
(define x 10)
(define nested-set 
  (lambda () 
    (let ((x 20))
      (set! x 30)
      x)))
(nested-set)
x"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result fourth)])
    (set! state (assert (RuntimeValue-value value) 10 "set! in nested function doesn't affect outer scope test")))

  ;; Complex variable interaction with multiple levels
  (let* ([input "
(define x 10)
(define y 20)
(define complex-fn
  (lambda ()
    (let ((x 30))
      (let ((get-both (lambda () (+ x y))))
        (set! y 40)
        (get-both)))))
(complex-fn)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result fourth)])
    (set! state (assert (RuntimeValue-value value) 70 "Complex variable interaction test")))

  state)

;; test-tail-call-optimization
;;    Tests for proper tail call optimization
(define (test-tail-call-optimization state output-fn)
  (output-fn "Running test-tail-call-optimization...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Deep recursion that would stack overflow without TCO
  (let* ([input "
  (define count-down
    (lambda (n)
      (if (= n 0)
          0
          (count-down (- n 1)))))
  (count-down 5000)"]  ; This would overflow without TCO
         [result (eval-eta-string input)]
         [value (get-value-from-eval result second)])
    (set! state (assert (RuntimeValue-value value) 0 "Deep recursion with TCO test")))

  ;; Mutual recursion with TCO
  (let* ([input "
  (define is-even?
    (lambda (n)
      (if (= n 0)
          #t
          (is-odd? (- n 1)))))
  (define is-odd?
    (lambda (n)
      (if (= n 0)
          #f
          (is-even? (- n 1)))))
  (is-even? 5000)"]  ; This would overflow without TCO
         [result (eval-eta-string input)]
         [value (get-value-from-eval result third)])
    (set! state (assert (RuntimeValue-value value) #t "Mutual recursion with TCO test")))

  state)

;; test-error-handling
;;    Tests for appropriate error handling in the interpreter
(define (test-error-handling state output-fn)
  (output-fn "Running test-error-handling...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Reference to undefined variable
  (let* ([input "undefined-variable"]
         [result (eval-eta-string input)])
    (set! state (assert (check-eval-error result) #t "Undefined variable error test")))

  ;; Type error in operation
  (let* ([input "(+ \"string\" 5)"]
         [result (eval-eta-string input)])
    (set! state (assert (check-eval-error result) #t "Type error in operation test")))

  ;; Application of non-function
  (let* ([input "(5 1 2 3)"]
         [result (eval-eta-string input)])
    (set! state (assert (check-eval-error result) #t "Application of non-function error test")))

  ;; Syntax error
  (let* ([input "(let ((x 5) (y)) y)"]
         [result (eval-eta-string input)])
    (set! state (assert (not (EvalResult-success? result)) #t "Syntax error test")))

  state)

;; run-eval-tests
;;    Main function to run all evaluation tests
(define (run-eval-tests state output-fn)
  (output-fn "Running eta evaluation tests...")
  
  ;; Individual test sections
  (set! state (test-simple-expressions state (make-indented-output-fn output-fn 1)))
  (set! state (test-variables-and-bindings state (make-indented-output-fn output-fn 1)))
  (set! state (test-conditionals state (make-indented-output-fn output-fn 1)))
  (set! state (test-functions state (make-indented-output-fn output-fn 1)))
  (set! state (test-tail-call-optimization state (make-indented-output-fn output-fn 1)))
  (set! state (test-error-handling state (make-indented-output-fn output-fn 1)))

  state)