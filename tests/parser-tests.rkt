#lang racket

(provide run-parser-tests)
(require "test-utils.rkt"
         "../eta/parser/ast.rkt"
         "../eta/parser/tokenizer.rkt"
         "../eta/parser/parser.rkt")



;; test-constant-parsing
;;     Tests for constant parsing (number, string, boolean)
(define (test-constant-parsing state output-fn)
  (output-fn "Running test-constant-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  (let* ([input "42"]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (make-expr Const (list "42") (Location 1 1 1 3))])
    (set! state (assert expr expected "Number constant parsing test")))

  (let* ([input "\"hello\""]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (make-expr Const (list "hello") (Location 1 1 1 8))])
    (set! state (assert expr expected "String constant parsing test")))

  (let* ([input "#t"]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (make-expr Const (list "#t") (Location 1 1 1 3))])
    (set! state (assert expr expected "Boolean true constant parsing test")))

  (let* ([input "#f"]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (make-expr Const (list "#f") (Location 1 1 1 3))])
    (set! state (assert expr expected "Boolean false constant parsing test")))

  state)

;; test-variable-parsing
;;     Tests for parsing variables (simple, complex, symbolic)
(define (test-variable-parsing state output-fn)
  (output-fn "Running test-variable-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))
  
  (set! state (assert (parse (tokenize "x"))
                      (make-expr Var (list "x") (Location 1 1 1 2))
                      "Simple variable parsing test"))

  (set! state (assert (parse (tokenize "hello-world!"))
                      (make-expr Var (list "hello-world!") (Location 1 1 1 13))
                      "Complex variable name parsing test"))

  (set! state (assert (parse (tokenize "+"))
                      (make-expr Var (list "+") (Location 1 1 1 2))
                      "Symbolic variable parsing test"))

  state)

;; test-quote-parsing
;;     Tests for parsing quoted expressions
(define (test-quote-parsing state output-fn)
  (output-fn "Running test-quote-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  (let* ([input "'x"]
         [expr (parse (tokenize input))]
         [expected (make-expr Quote
                         (list (make-expr Var (list "x") (Location 1 2 1 3)))
                         (Location 1 1 1 2))])
    (set! state (assert expr expected "Quoted variable parsing test")))

  (let* ([input "'42"]
         [expr (parse (tokenize input))]
         [expected (make-expr Quote
                         (list (make-expr Const (list "42") (Location 1 2 1 4)))
                         (Location 1 1 1 2))])
    (set! state (assert expr expected "Quoted number parsing test")))

  (let* ([input "'(1 2 3)"]
         [expr (parse (tokenize input))]
         [expected
          (make-expr Quote
                (list (make-expr App
                            (list (make-expr Const (list "1") (Location 1 3 1 4))
                                  (make-expr Const (list "2") (Location 1 5 1 6))
                                  (make-expr Const (list "3") (Location 1 7 1 8)))
                            (Location 1 2 1 9)))
                (Location 1 1 1 2))])
    (set! state (assert expr expected "Quoted list parsing test")))

  state)

;; test-application-parsing
;;     Tests for function application
(define (test-application-parsing state output-fn)
  (output-fn "Running test-application-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  (let* ([input "(+ 1 2)"]
         [expr (parse (tokenize input))]
         [expected
          (make-expr App
                (list (make-expr Var (list "+") (Location 1 2 1 3))
                      (make-expr Const (list "1") (Location 1 4 1 5))
                      (make-expr Const (list "2") (Location 1 6 1 7)))
                (Location 1 1 1 8))])
    (set! state (assert expr expected "Simple function application parsing test")))

  ;; (foo)
  (set! state (assert (parse (tokenize "(foo)"))
                      (make-expr App
                            (list (make-expr Var (list "foo") (Location 1 2 1 5)))
                            (Location 1 1 1 6))
                      "No-argument function application parsing test"))

  ;; (foo 1 "bar" #t)
  (set! state (assert (parse (tokenize "(foo 1 \"bar\" #t)"))
                      (make-expr App
                            (list (make-expr Var (list "foo") (Location 1 2 1 5))
                                  (make-expr Const (list "1") (Location 1 6 1 7))
                                  (make-expr Const (list "bar") (Location 1 8 1 13))
                                  (make-expr Const (list "#t") (Location 1 14 1 16)))
                            (Location 1 1 1 17))
                      "Multi-argument function application parsing test"))

  state)

;; test-complex-expressions
;;     Tests for nested and mixed expressions
(define (test-complex-expressions state output-fn)
  (output-fn "Running test-complex-expressions...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; (+ 1 (* 2 3))
  (let* ([expr (parse (tokenize "(+ 1 (* 2 3))"))]
         [expected
          (make-expr App
                (list (make-expr Var (list "+") (Location 1 2 1 3))
                      (make-expr Const (list "1") (Location 1 4 1 5))
                      (make-expr App
                            (list (make-expr Var (list "*") (Location 1 7 1 8))
                                  (make-expr Const (list "2") (Location 1 9 1 10))
                                  (make-expr Const (list "3") (Location 1 11 1 12)))
                            (Location 1 6 1 13)))
                (Location 1 1 1 14))])
    (set! state (assert expr expected "Nested function application parsing test")))

  ;; (cons 'a '(b c))
  (let* ([expr (parse (tokenize "(cons 'a '(b c))"))]
         [expected
          (make-expr App
                (list (make-expr Var (list "cons") (Location 1 2 1 6))
                      (make-expr Quote
                            (list (make-expr Var (list "a") (Location 1 8 1 9)))
                            (Location 1 7 1 8))
                      (make-expr Quote
                            (list (make-expr App
                                        (list (make-expr Var (list "b") (Location 1 12 1 13))
                                              (make-expr Var (list "c") (Location 1 14 1 15)))
                                        (Location 1 11 1 16)))
                            (Location 1 10 1 11)))
                (Location 1 1 1 17))])
    (set! state (assert expr expected "Quote and application combination test")))

  state)

;; run-parser-tests
;;     Runs all parser-related tests.
(define (run-parser-tests state output-fn)
  (output-fn "Running parser tests...")
  (let ([out (make-indented-output-fn output-fn 1)])
    (for/fold ([s state])
              ([f (list 
                        test-constant-parsing
                        test-variable-parsing
                        test-quote-parsing
                        test-application-parsing
                        test-complex-expressions)])
      (with-error-handling (lambda () (f s out))
        (symbol->string (object-name f)) s out))))
