#lang racket

(provide run-parser-tests)
(require "test-utils.rkt"
         "../eta/parser/ast.rkt"
         "../eta/parser/tokenizer.rkt"
         "../eta/parser/parser.rkt")

(define (assert-expr-equal actual expected msg state output-fn)
  (assert-equal actual expected msg state output-fn #:cmp expr-equal?))

;; test-expr-equal
;;     Tests expr-equal? function itself.
(define (test-expr-equal state output-fn)
  (output-fn "Running test-expr-equal...")

  (define assert (lambda (actual expected msg)
                   (assert-equal actual expected msg state (make-indented-output-fn output-fn 1))))

  ;; Shared locations
  (define loc1 (Location 1 1 1 2))
  (define loc2 (Location 1 1 1 2)) ;; equal to loc1
  (define loc3 (Location 2 1 2 2)) ;; different

  ;; 1. same Expr (identical contents)
  (define e1 (Expr 'const (list "42") loc1))
  (define e2 (Expr 'const (list "42") loc2))
  (set! state (assert (expr-equal? e1 e2) #t "Same const exprs should be equal"))

  ;; 2. value differs
  (define e3 (Expr 'const (list "43") loc1))
  (set! state (assert (expr-equal? e1 e3) #f "Exprs with different const values should not be equal"))

  ;; 3. location differs
  (define e4 (Expr 'const (list "42") loc3))
  (set! state (assert (expr-equal? e1 e4) #f "Exprs with different locations should not be equal"))

  ;; 4. head differs
  (define e5 (Expr 'var (list "42") loc1))
  (set! state (assert (expr-equal? e1 e5) #f "Exprs with different heads should not be equal"))

  ;; 5. multiple args, same order
  (define e6 (Expr 'app (list (Expr 'var (list "+") loc1)
                              (Expr 'const (list "1") loc1)
                              (Expr 'const (list "2") loc1))
                   loc1))
  (define e7 (Expr 'app (list (Expr 'var (list "+") loc2)
                              (Expr 'const (list "1") loc2)
                              (Expr 'const (list "2") loc2))
                   loc2))
  (set! state (assert (expr-equal? e6 e7) #t "Function applications with same structure should be equal"))

  ;; 6. multiple args, different order
  (define e8 (Expr 'app (list (Expr 'var (list "+") loc1)
                              (Expr 'const (list "2") loc1)
                              (Expr 'const (list "1") loc1))
                   loc1))
  (set! state (assert (expr-equal? e6 e8) #f "Function applications with different argument order should not be equal"))

  ;; 7. nested exprs, deep equality
  (define e9 (Expr 'quote (list (Expr 'var (list "x") loc1)) loc1))
  (define e10 (Expr 'quote (list (Expr 'var (list "x") loc2)) loc2))
  (set! state (assert (expr-equal? e9 e10) #t "Nested exprs with same structure should be equal"))

  ;; 8. nested exprs, inner value differs
  (define e11 (Expr 'quote (list (Expr 'var (list "y") loc1)) loc1))
  (set! state (assert (expr-equal? e9 e11) #f "Nested exprs with different inner var should not be equal"))

  state)


;; test-constant-parsing
;;     Tests for constant parsing (number, string, boolean)
(define (test-constant-parsing state output-fn)
  (output-fn "Running test-constant-parsing...")

  (define assert (lambda (a e m)
                   (assert-expr-equal a e m state (make-indented-output-fn output-fn 1))))

  (let* ([input "42"]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (Expr 'const (list "42") (Location 1 1 1 3))])
    (set! state (assert expr expected "Number constant parsing test")))

  (let* ([input "\"hello\""]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (Expr 'const (list "hello") (Location 1 1 1 8))])
    (set! state (assert expr expected "String constant parsing test")))

  (let* ([input "#t"]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (Expr 'const (list "#t") (Location 1 1 1 3))])
    (set! state (assert expr expected "Boolean true constant parsing test")))

  (let* ([input "#f"]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (Expr 'const (list "#f") (Location 1 1 1 3))])
    (set! state (assert expr expected "Boolean false constant parsing test")))

  state)

;; test-variable-parsing
;;     Tests for parsing variables (simple, complex, symbolic)
(define (test-variable-parsing state output-fn)
  (output-fn "Running test-variable-parsing...")

  (define assert (lambda (a e m)
                   (assert-expr-equal a e m state (make-indented-output-fn output-fn 1))))

  (set! state (assert (parse (tokenize "x"))
                      (Expr 'var (list "x") (Location 1 1 1 2))
                      "Simple variable parsing test"))

  (set! state (assert (parse (tokenize "hello-world!"))
                      (Expr 'var (list "hello-world!") (Location 1 1 1 13))
                      "Complex variable name parsing test"))

  (set! state (assert (parse (tokenize "+"))
                      (Expr 'var (list "+") (Location 1 1 1 2))
                      "Symbolic variable parsing test"))

  state)

;; test-quote-parsing
;;     Tests for parsing quoted expressions
(define (test-quote-parsing state output-fn)
  (output-fn "Running test-quote-parsing...")

  (define assert (lambda (a e m)
                   (assert-expr-equal a e m state (make-indented-output-fn output-fn 1))))

  (let* ([input "'x"]
         [expr (parse (tokenize input))]
         [expected (Expr 'quote
                         (list (Expr 'var (list "x") (Location 1 2 1 3)))
                         (Location 1 1 1 2))])
    (set! state (assert expr expected "Quoted variable parsing test")))

  (let* ([input "'42"]
         [expr (parse (tokenize input))]
         [expected (Expr 'quote
                         (list (Expr 'const (list "42") (Location 1 2 1 4)))
                         (Location 1 1 1 2))])
    (set! state (assert expr expected "Quoted number parsing test")))

  (let* ([input "'(1 2 3)"]
         [expr (parse (tokenize input))]
         [expected
          (Expr 'quote
                (list (Expr 'app
                            (list (Expr 'const (list "1") (Location 1 3 1 4))
                                  (Expr 'const (list "2") (Location 1 5 1 6))
                                  (Expr 'const (list "3") (Location 1 7 1 8)))
                            (Location 1 2 1 9)))
                (Location 1 1 1 2))])
    (set! state (assert expr expected "Quoted list parsing test")))

  state)

;; test-application-parsing
;;     Tests for function application
(define (test-application-parsing state output-fn)
  (output-fn "Running test-application-parsing...")

  (define assert (lambda (a e m)
                   (assert-expr-equal a e m state (make-indented-output-fn output-fn 1))))

  (let* ([input "(+ 1 2)"]
         [expr (parse (tokenize input))]
         [expected
          (Expr 'app
                (list (Expr 'var (list "+") (Location 1 2 1 3))
                      (Expr 'const (list "1") (Location 1 4 1 5))
                      (Expr 'const (list "2") (Location 1 6 1 7)))
                (Location 1 1 1 8))])
    (set! state (assert expr expected "Simple function application parsing test")))

  ;; (foo)
  (set! state (assert (parse (tokenize "(foo)"))
                      (Expr 'app
                            (list (Expr 'var (list "foo") (Location 1 2 1 5)))
                            (Location 1 1 1 6))
                      "No-argument function application parsing test"))

  ;; (foo 1 "bar" #t)
  (set! state (assert (parse (tokenize "(foo 1 \"bar\" #t)"))
                      (Expr 'app
                            (list (Expr 'var (list "foo") (Location 1 2 1 5))
                                  (Expr 'const (list "1") (Location 1 6 1 7))
                                  (Expr 'const (list "bar") (Location 1 8 1 13))
                                  (Expr 'const (list "#t") (Location 1 14 1 16)))
                            (Location 1 1 1 17))
                      "Multi-argument function application parsing test"))

  state)

;; test-complex-expressions
;;     Tests for nested and mixed expressions
(define (test-complex-expressions state output-fn)
  (output-fn "Running test-complex-expressions...")

  (define assert (lambda (a e m)
                   (assert-expr-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; (+ 1 (* 2 3))
  (let* ([expr (parse (tokenize "(+ 1 (* 2 3))"))]
         [expected
          (Expr 'app
                (list (Expr 'var (list "+") (Location 1 2 1 3))
                      (Expr 'const (list "1") (Location 1 4 1 5))
                      (Expr 'app
                            (list (Expr 'var (list "*") (Location 1 7 1 8))
                                  (Expr 'const (list "2") (Location 1 9 1 10))
                                  (Expr 'const (list "3") (Location 1 11 1 12)))
                            (Location 1 6 1 13)))
                (Location 1 1 1 14))])
    (set! state (assert expr expected "Nested function application parsing test")))

  ;; (cons 'a '(b c))
  (let* ([expr (parse (tokenize "(cons 'a '(b c))"))]
         [expected
          (Expr 'app
                (list (Expr 'var (list "cons") (Location 1 2 1 6))
                      (Expr 'quote
                            (list (Expr 'var (list "a") (Location 1 8 1 9)))
                            (Location 1 7 1 8))
                      (Expr 'quote
                            (list (Expr 'app
                                        (list (Expr 'var (list "b") (Location 1 12 1 13))
                                              (Expr 'var (list "c") (Location 1 14 1 15)))
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
              ([f (list test-expr-equal  ; 👈 ここに追加！
                        test-constant-parsing
                        test-variable-parsing
                        test-quote-parsing
                        test-application-parsing
                        test-complex-expressions)])
      (with-error-handling (lambda () (f s out))
        (symbol->string (object-name f)) s out))))
