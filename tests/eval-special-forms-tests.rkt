#lang racket

(provide run-eval-special-forms-tests)
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

;; test-eval-function
;;    Tests for the eval function - tests our latest implementation
(define (test-eval-function state output-fn)
  (output-fn "Running test-eval-function...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple eval of constant
  (let* ([input "(eval (quote 42))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Eval constant tag test"))
    (set! state (assert (RuntimeValue-value value) 42 "Eval constant value test")))

  ;; Eval of arithmetic expression
  (let* ([input "(eval (quote (+ 3 4)))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Eval arithmetic expression tag test"))
    (set! state (assert (RuntimeValue-value value) 7 "Eval arithmetic expression value test")))

  ;; Eval of a special form (if)
  (let* ([input "(eval (quote (if #t 1 2)))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Eval if special form tag test"))
    (set! state (assert (RuntimeValue-value value) 1 "Eval if special form value test")))

  ;; Eval of a lambda expression
  (let* ([input "(eval (quote ((lambda (x) (+ x 1)) 5)))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Eval lambda application tag test"))
    (set! state (assert (RuntimeValue-value value) 6 "Eval lambda application value test")))

  ;; Eval of a nested special form
  (let* ([input "(eval (quote (let ((x 10)) (+ x 5))))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Eval let special form tag test"))
    (set! state (assert (RuntimeValue-value value) 15 "Eval let special form value test")))

  ;; Eval with environment awareness
  (let* ([input "(define y 42) (eval (quote y))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Eval environment awareness tag test"))
    (set! state (assert (RuntimeValue-value value) 42 "Eval environment awareness value test")))

  state)

;; test-let-special-form
;;    Tests for the let special form - tests our latest implementation
(define (test-let-special-form state output-fn)
  (output-fn "Running test-let-special-form...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple let with one binding
  (let* ([input "(let ((x 5)) x)"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Simple let tag test"))
    (set! state (assert (RuntimeValue-value value) 5 "Simple let value test")))

  ;; Let with multiple bindings
  (let* ([input "(let ((x 3) (y 4)) (+ x y))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Multiple bindings let tag test"))
    (set! state (assert (RuntimeValue-value value) 7 "Multiple bindings let value test")))

  ;; Let with sequential expressions in body
  (let* ([input "(let ((x 10)) (define y 20) (+ x y))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Let with sequential expressions tag test"))
    (set! state (assert (RuntimeValue-value value) 30 "Let with sequential expressions value test")))

  ;; Nested let expressions
  (let* ([input "(let ((x 5)) (let ((y (* x 2))) (+ x y)))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Nested let tag test"))
    (set! state (assert (RuntimeValue-value value) 15 "Nested let value test")))

  ;; Named let (recursive let)
  (let* ([input "
(let fac ((n 5))
  (if (= n 0)
      1
      (* n (fac (- n 1)))))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Named let tag test"))
    (set! state (assert (RuntimeValue-value value) 120 "Named let value test")))

  ;; Let with complex expressions in bindings
  (let* ([input "(let ((x (+ 1 2)) (y (* 3 4))) (- y x))"]
         [result (eval-eta-string input)]
         [value (get-value-from-eval result)])
    (set! state (assert (RuntimeValue-tag value) 'IntTag "Let with complex bindings tag test"))
    (set! state (assert (RuntimeValue-value value) 9 "Let with complex bindings value test")))

  state)

;; run-eval-special-forms-tests
;;    Main function to run eval and let special form tests
(define (run-eval-special-forms-tests state output-fn)
  (output-fn "Running eta special forms tests...")
  
  ;; Individual test sections
  (set! state (test-eval-function state (make-indented-output-fn output-fn 1)))
  (set! state (test-let-special-form state (make-indented-output-fn output-fn 1)))

  state)
