#lang racket

(provide run-parser-tests)
(require "test-utils.rkt"
         "../eta/parser/ast.rkt"
         "../eta/parser/parser.rkt")

(define (test-const state output-fn)
  (output-fn "Running test-const...")

  (let* ([input "#t"]
         [parsed (parse input)]
         [expected (Expr 'const (list #t))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Const test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "#f"]
         [parsed (parse input)]
         [expected (Expr 'const (list #f))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Const test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "123"]
         [parsed (parse input)]
         [expected (Expr 'const (list 123))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Const test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "\"hello\""]
         [parsed (parse input)]
         [expected (Expr 'const (list "hello"))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Const test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "()"]
         [parsed (parse input)]
         [expected (Expr 'const '(()))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Const test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))
  (let* ([input "'x"]
         [parsed (parse input)]
         [expected (Expr 'quote (list (Expr 'id (list 'x))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Const test for " input " => 'x")
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "-42"]
         [parsed (parse input)]
         [expected (Expr 'const (list -42))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Const test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "3.14"]
         [parsed (parse input)]
         [expected (Expr 'const (list 3.14))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Const test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "'()"]
         [parsed (parse input)]
         [expected (Expr 'quote (list (Expr 'const '(()))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Const test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "'(1 2 3)"]
         [parsed (parse input)]
         [expected (Expr 'quote
                         (list (Expr 'application
                                     (list (Expr 'const (list 1))
                                           (Expr 'const (list 2))
                                           (Expr 'const (list 3))))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Const test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "#\\a"]
         [parsed (parse input)]
         [expected (Expr 'const (list #\a))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Const test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  state)

(define (test-id state output-fn)
  (output-fn "Running test-id...")

  (let* ([input "x"]
         [parsed (parse input)]
         [expected (Expr 'id (list 'x))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Id test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "hello-world!"]
         [parsed (parse input)]
         [expected (Expr 'id (list 'hello-world!))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Id test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "x2"]
         [parsed (parse input)]
         [expected (Expr 'id (list 'x2))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Id test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "+"]
         [parsed (parse input)]
         [expected (Expr 'id (list '+))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Id test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "set!"]
         [parsed (parse input)]
         [expected (Expr 'id (list 'set!))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Id test for special form name " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "->string"]
         [parsed (parse input)]
         [expected (Expr 'id (list '->string))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Id test for arrow symbol " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "*multiply*"]
         [parsed (parse input)]
         [expected (Expr 'id (list '*multiply*))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Id test for symbol with asterisks " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  state)

(define (test-lambda state output-fn)
  (output-fn "Running test-lambda...")

  (let* ([input "(lambda (x) x)"]
         [parsed (parse input)]
         [expected (Expr 'lambda (list (Expr 'args (list 'x)) (Expr 'id (list 'x))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Lambda test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(lambda (x y) (+ x y))"]
         [parsed (parse input)]
         [expected
          (Expr 'lambda
                (list (Expr 'args (list 'x 'y))
                      (Expr 'application
                            (list (Expr 'id (list '+)) (Expr 'id (list 'x)) (Expr 'id (list 'y))))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Lambda test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(lambda () 42)"]
         [parsed (parse input)]
         [expected (Expr 'lambda (list (Expr 'args '()) (Expr 'const (list 42))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Lambda test with no args for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(lambda (x y) (+ x y) (* x y))"]
         [parsed (parse input)]
         [expected
          (Expr 'lambda
                (list (Expr 'args (list 'x 'y))
                      (Expr 'application
                            (list (Expr 'id (list '+)) (Expr 'id (list 'x)) (Expr 'id (list 'y))))
                      (Expr 'application
                            (list (Expr 'id (list '*)) (Expr 'id (list 'x)) (Expr 'id (list 'y))))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Lambda test with multiple body expressions for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(lambda (x . rest) rest)"]
         [parsed (parse input)]
         [expected
          (Expr 'lambda (list (Expr 'args (list 'x)) (Expr 'varargs 'rest) (Expr 'id (list 'rest))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Lambda test with dotted pair args for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  state)

(define (test-application state output-fn)
  (output-fn "Running test-application...")

  (let* ([input "(f x)"]
         [parsed (parse input)]
         [expected (Expr 'application (list (Expr 'id (list 'f)) (Expr 'id (list 'x))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Application test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(f x y)"]
         [parsed (parse input)]
         [expected (Expr 'application
                         (list (Expr 'id (list 'f)) (Expr 'id (list 'x)) (Expr 'id (list 'y))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Application test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "((lambda (x) x) 42)"]
         [parsed (parse input)]
         [expected (Expr 'application
                         (list (Expr 'lambda (list (Expr 'args (list 'x)) (Expr 'id (list 'x))))
                               (Expr 'const (list 42))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Application test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(+ 1 (- 3 2))"]
         [parsed (parse input)]
         [expected (Expr 'application
                         (list (Expr 'id (list '+))
                               (Expr 'const (list 1))
                               (Expr 'application
                                     (list (Expr 'id (list '-))
                                           (Expr 'const (list 3))
                                           (Expr 'const (list 2))))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Nested application test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(apply + '(1 2 3))"]
         [parsed (parse input)]
         [expected (Expr 'application
                         (list (Expr 'id (list 'apply))
                               (Expr 'id (list '+))
                               (Expr 'quote
                                     (list (Expr 'application
                                                 (list (Expr 'const (list 1))
                                                       (Expr 'const (list 2))
                                                       (Expr 'const (list 3))))))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Application with quoted list for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "((if #t + -) 3 4)"]
         [parsed (parse input)]
         [expected
          (Expr 'application
                (list (Expr 'if
                            (list (Expr 'const (list #t)) (Expr 'id (list '+)) (Expr 'id (list '-))))
                      (Expr 'const (list 3))
                      (Expr 'const (list 4))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Application with conditional operator for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  state)

(define (test-define state output-fn)
  (output-fn "Running test-define...")

  (let* ([input "(define x 42)"]
         [parsed (parse input)]
         [expected (Expr 'define (list (Expr 'id (list 'x)) (Expr 'const (list 42))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Define test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(define (f x) (+ x 1))"]
         [parsed (parse input)]
         [expected (Expr 'define
                         (list (Expr 'id (list 'f))
                               (Expr 'args (list 'x))
                               (Expr 'application
                                     (list (Expr 'id (list '+))
                                           (Expr 'id (list 'x))
                                           (Expr 'const (list 1))))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Define test for function form " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(define x (lambda (y) (+ y 1)))"]
         [parsed (parse input)]
         [expected (Expr 'define
                         (list (Expr 'id (list 'x))
                               (Expr 'lambda
                                     (list (Expr 'args (list 'y))
                                           (Expr 'application
                                                 (list (Expr 'id (list '+))
                                                       (Expr 'id (list 'y))
                                                       (Expr 'const (list 1))))))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Define test with lambda " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(define (f x . args) (apply + x args))"]
         [parsed (parse input)]
         [expected (Expr 'define
                         (list (Expr 'id (list 'f))
                               (Expr 'args (list 'x))
                               (Expr 'varargs 'args)
                               (Expr 'application
                                     (list (Expr 'id (list 'apply))
                                           (Expr 'id (list '+))
                                           (Expr 'id (list 'x))
                                           (Expr 'id (list 'args))))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Define test with variadic args " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  state)

(define (test-if state output-fn)
  (output-fn "Running test-if...")

  (let* ([input "(if #t 1 2)"]
         [parsed (parse input)]
         [expected
          (Expr 'if (list (Expr 'const (list #t)) (Expr 'const (list 1)) (Expr 'const (list 2))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Basic if test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(if (> x 0) (+ x 1) (- x 1))"]
         [parsed (parse input)]
         [expected
          (Expr
           'if
           (list (Expr 'application
                       (list (Expr 'id (list '>)) (Expr 'id (list 'x)) (Expr 'const (list 0))))
                 (Expr 'application
                       (list (Expr 'id (list '+)) (Expr 'id (list 'x)) (Expr 'const (list 1))))
                 (Expr 'application
                       (list (Expr 'id (list '-)) (Expr 'id (list 'x)) (Expr 'const (list 1))))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Complex if test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(if #f 1)"]
         [parsed (parse input)]
         [expected (Expr 'if (list (Expr 'const (list #f)) (Expr 'const (list 1))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "If without else for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  state)

(define (test-let state output-fn)
  (output-fn "Running test-let...")

  (let* ([input "(let ((x 1) (y 2)) (+ x y))"]
         [parsed (parse input)]
         [expected
          (Expr 'let
                (list (Expr 'bindings
                            (list (Expr 'binding (list 'x (Expr 'const (list 1))))
                                  (Expr 'binding (list 'y (Expr 'const (list 2))))))
                      (Expr 'application
                            (list (Expr 'id (list '+)) (Expr 'id (list 'x)) (Expr 'id (list 'y))))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Basic let test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(let ((x 1)) (let ((y (+ x 1))) (* x y)))"]
         [parsed (parse input)]
         [expected
          (Expr 'let
                (list (Expr 'bindings (list (Expr 'binding (list 'x (Expr 'const (list 1))))))
                      (Expr 'let
                            (list (Expr 'bindings
                                        (list (Expr 'binding
                                                    (list 'y
                                                          (Expr 'application
                                                                (list (Expr 'id (list '+))
                                                                      (Expr 'id (list 'x))
                                                                      (Expr 'const (list 1))))))))
                                  (Expr 'application
                                        (list (Expr 'id (list '*))
                                              (Expr 'id (list 'x))
                                              (Expr 'id (list 'y))))))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Nested let test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(let () 42)"]
         [parsed (parse input)]
         [expected (Expr 'let (list (Expr 'bindings '()) (Expr 'const (list 42))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Let with no bindings for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(let ((x 1)) (set! x 2) x)"]
         [parsed (parse input)]
         [expected (Expr 'let
                         (list (Expr 'bindings
                                     (list (Expr 'binding (list 'x (Expr 'const (list 1))))))
                               (Expr 'set! (list (Expr 'id (list 'x)) (Expr 'const (list 2))))
                               (Expr 'id (list 'x))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Let with multiple expressions for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  state)

(define (test-begin-and-set state output-fn)
  (output-fn "Running test-begin-and-set...")

  (let* ([input "(begin (define x 1) (set! x 2) x)"]
         [parsed (parse input)]
         [expected (Expr 'begin
                         (list (Expr 'define (list (Expr 'id (list 'x)) (Expr 'const (list 1))))
                               (Expr 'set! (list (Expr 'id (list 'x)) (Expr 'const (list 2))))
                               (Expr 'id (list 'x))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Basic begin test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(begin)"]
         [parsed (parse input)]
         [expected (Expr 'begin '())])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Empty begin test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(begin 1)"]
         [parsed (parse input)]
         [expected (Expr 'begin (list (Expr 'const (list 1))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Begin with single expr for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(set! x 42)"]
         [parsed (parse input)]
         [expected (Expr 'set! (list (Expr 'id (list 'x)) (Expr 'const (list 42))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Basic set! test for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  (let* ([input "(set! result (f x y))"]
         [parsed (parse input)]
         [expected
          (Expr 'set!
                (list (Expr 'id (list 'result))
                      (Expr 'application
                            (list (Expr 'id (list 'f)) (Expr 'id (list 'x)) (Expr 'id (list 'y))))))])
    (set! state
          (assert-equal parsed
                        expected
                        (string-append "Set! with complex expr for " input)
                        state
                        (make-indented-output-fn output-fn 1))))

  state)

(define (run-parser-tests state output-fn)
  (output-fn "Running parser tests...")
  (let ([child-output-fn (make-indented-output-fn output-fn 1)])
    (let ([state (with-error-handling (lambda () (test-const state child-output-fn))
                                      "test-const"
                                      state
                                      child-output-fn)])
      (let ([state (with-error-handling (lambda () (test-id state child-output-fn))
                                        "test-id"
                                        state
                                        child-output-fn)])
        (let ([state (with-error-handling (lambda () (test-lambda state child-output-fn))
                                          "test-lambda"
                                          state
                                          child-output-fn)])
          (let ([state (with-error-handling (lambda () (test-application state child-output-fn))
                                            "test-application"
                                            state
                                            child-output-fn)])
            (let ([state (with-error-handling (lambda () (test-define state child-output-fn))
                                              "test-define"
                                              state
                                              child-output-fn)])
              (let ([state (with-error-handling (lambda () (test-if state child-output-fn))
                                                "test-if"
                                                state
                                                child-output-fn)])
                (let ([state (with-error-handling (lambda () (test-let state child-output-fn))
                                                  "test-let"
                                                  state
                                                  child-output-fn)])
                  (with-error-handling (lambda () (test-begin-and-set state child-output-fn))
                                       "test-begin-and-set"
                                       state
                                       child-output-fn))))))))))
