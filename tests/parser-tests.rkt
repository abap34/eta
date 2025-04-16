#lang racket

(provide run-parser-tests)
(require "test-utils.rkt"
         "../eta/parser/ast.rkt"
         "../eta/parser/tokenizer.rkt"
         "../eta/parser/parser.rkt"
         "../eta/utils/location.rkt"
         "../eta/utils/error.rkt")

;; Helper function to simplify test creation
(define (test-parse input expected-ast message state output-fn)
  (let* ([tokens (tokenize input)]
         [actual-ast (parse tokens)])
    (assert-equal actual-ast expected-ast message state 
                 (make-indented-output-fn output-fn 1))))

;; test-constant-parsing
;;     Tests for constant parsing (number, string, boolean, nil)
(define (test-constant-parsing state output-fn)
  (output-fn "Running test-constant-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Number constant
  (let* ([input "42"]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (make-expr Const (list 'Num 42) (Location 1 1 1 3))])
    (set! state (assert expr expected "Number constant parsing test")))

  ;; String constant
  (let* ([input "\"hello\""]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (make-expr Const (list 'String "hello") (Location 1 1 1 8))])
    (set! state (assert expr expected "String constant parsing test")))

  ;; Boolean true constant
  (let* ([input "#t"]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (make-expr Const (list 'Bool #t) (Location 1 1 1 3))])
    (set! state (assert expr expected "Boolean true constant parsing test")))

  ;; Boolean false constant
  (let* ([input "#f"]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (make-expr Const (list 'Bool #f) (Location 1 1 1 3))])
    (set! state (assert expr expected "Boolean false constant parsing test")))

  ;; Empty list / nil constant
  (let* ([input "()"]
         [tokens (tokenize input)]
         [expr (parse tokens)]
         [expected (make-expr Nil '() (Location 1 1 1 3))])
    (set! state (assert expr expected "Nil/empty list parsing test")))

  state)

;; test-variable-parsing
;;     Tests for parsing variables (simple, complex, symbolic)
(define (test-variable-parsing state output-fn)
  (output-fn "Running test-variable-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))
  
  ;; Simple variable
  (set! state (assert (parse (tokenize "x"))
                      (make-expr Var (list "x") (Location 1 1 1 2))
                      "Simple variable parsing test"))

  ;; Complex variable name
  (set! state (assert (parse (tokenize "hello-world!"))
                      (make-expr Var (list "hello-world!") (Location 1 1 1 13))
                      "Complex variable name parsing test"))

  ;; Symbolic variable
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

  ;; Quoted variable
  (let* ([input "'x"]
         [expr (parse (tokenize input))]
         [expected (make-expr Quote
                         (list (make-expr Var (list "x") (Location 1 2 1 3)))
                         (Location 1 1 1 3))])
    (set! state (assert expr expected "Quoted variable parsing test")))

  ;; Quoted number
  (let* ([input "'42"]
         [expr (parse (tokenize input))]
         [expected (make-expr Quote
                         (list (make-expr Const (list 'Num 42) (Location 1 2 1 4)))
                         (Location 1 1 1 4))])
    (set! state (assert expr expected "Quoted number parsing test")))

  ;; Quoted list
  (let* ([input "'(1 2 3)"]
         [expr (parse (tokenize input))]
         [expected (make-expr Quote
                         (list 
                          (make-expr S-Expr
                             (list (make-expr Const (list 'Num 1) (Location 1 3 1 4))
                                   (make-expr Const (list 'Num 2) (Location 1 5 1 6))
                                   (make-expr Const (list 'Num 3) (Location 1 7 1 8)))
                             (Location 1 2 1 9))
                         )
         (Location 1 1 1 9))])
    (set! state (assert expr expected "Quoted list parsing test")))


  ;; Quote with explicit quote syntax
  (let* ([input "(quote hello)"]
         [expr (parse (tokenize input))]
         [expected (make-expr Quote
                         (list (make-expr Var (list "hello") (Location 1 8 1 13)))
                         (Location 1 1 1 14))])
    (set! state (assert expr expected "Explicit quote syntax parsing test")))

  state)

;; test-application-parsing
;;     Tests for function application
(define (test-application-parsing state output-fn)
  (output-fn "Running test-application-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple function application
  (let* ([input "(+ 1 2)"]
         [expr (parse (tokenize input))]
         [expected (make-expr App 
                         (list (make-expr Var (list "+") (Location 1 2 1 3))
                               (list 
                                (make-expr Const (list 'Num 1) (Location 1 4 1 5))
                                (make-expr Const (list 'Num 2) (Location 1 6 1 7))))
                         (Location 1 1 1 8))])
    (set! state (assert expr expected "Simple function application parsing test")))

  ;; No-argument function call
  (let* ([input "(foo)"]
         [expr (parse (tokenize input))]
         [expected (make-expr App
                         (list (make-expr Var (list "foo") (Location 1 2 1 5))
                               '())
                         (Location 1 1 1 6))])
    (set! state (assert expr expected "No-argument function application parsing test")))

  ;; Multiple argument types
  (let* ([input "(foo 1 \"bar\" #t)"]
         [expr (parse (tokenize input))]
         [expected (make-expr App
                         (list (make-expr Var (list "foo") (Location 1 2 1 5))
                               (list
                                (make-expr Const (list 'Num 1) (Location 1 6 1 7))
                                (make-expr Const (list 'String "bar") (Location 1 8 1 13))
                                (make-expr Const (list 'Bool #t) (Location 1 14 1 16))))
                         (Location 1 1 1 17))])
    (set! state (assert expr expected "Multi-argument function application parsing test")))

  ;; Nested function calls
  (let* ([input "(+ 1 (* 2 3))"]
         [expr (parse (tokenize input))]
         [expected (make-expr App
                         (list (make-expr Var (list "+") (Location 1 2 1 3))
                               (list
                                (make-expr Const (list 'Num 1) (Location 1 4 1 5))
                                (make-expr App
                                     (list (make-expr Var (list "*") (Location 1 7 1 8))
                                           (list
                                            (make-expr Const (list 'Num 2) (Location 1 9 1 10))
                                            (make-expr Const (list 'Num 3) (Location 1 11 1 12))))
                                     (Location 1 6 1 13))))
                         (Location 1 1 1 14))])
    (set! state (assert expr expected "Nested function application parsing test")))

  state)

;; test-lambda-parsing
;;     Tests for lambda expression parsing
(define (test-lambda-parsing state output-fn)
  (output-fn "Running test-lambda-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple lambda with single argument
  (let* ([input "(lambda (x) x)"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Lambda "Lambda expression head test")))

  ;; Lambda with multiple arguments
  (let* ([input "(lambda (x y z) (+ x (* y z)))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Lambda "Lambda with multiple args test")))

  ;; Lambda with rest arguments
  (let* ([input "(lambda (x . rest) (cons x rest))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Lambda "Lambda with rest args test")))

  ;; Lambda with single variable as argument (no parens)
  (let* ([input "(lambda x x)"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Lambda "Lambda with single variable arg test")))

  ;; Lambda with empty argument list
  (let* ([input "(lambda () 42)"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Lambda "Lambda with empty arg list test")))

  ;; Lambda with internal define
  (let* ([input "(lambda () (define x 1) (+ x 2))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Lambda "Lambda with internal define test")))

  state)

;; test-define-parsing
;;     Tests for define expression parsing
(define (test-define-parsing state output-fn)
  (output-fn "Running test-define-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple variable definition
  (let* ([input "(define x 42)"]
         [expr (parse (tokenize input))]
         [expected (make-expr Define 
                         (list (make-expr Var (list "x") (Location 1 9 1 10))
                               (make-expr Const (list 'Num 42) (Location 1 11 1 13)))
                         (Location 1 1 1 14))])
    (set! state (assert expr expected "Simple variable definition parsing test")))

  ;; Function definition - traditional syntax
  (let* ([input "(define (add x y) (+ x y))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Define "Function definition parsing test")))

  ;; Function definition with rest arguments
  (let* ([input "(define (foo x . rest) (cons x rest))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Define "Function with rest args definition test")))

  ;; Lambda function definition
  (let* ([input "(define adder (lambda (x) (+ x 1)))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Define "Lambda function definition test")))

  state)

;; test-if-parsing
;;     Tests for if expression parsing
(define (test-if-parsing state output-fn)
  (output-fn "Running test-if-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; If with else branch
  (let* ([input "(if (> x 0) \"positive\" \"non-positive\")"]
         [expr (parse (tokenize input))]
         [expected (make-expr If
                         (list #t
                               (make-expr App
                                    (list (make-expr Var (list ">") (Location 1 6 1 7))
                                          (list 
                                           (make-expr Var (list "x") (Location 1 8 1 9))
                                           (make-expr Const (list 'Num 0) (Location 1 10 1 11))))
                                    (Location 1 5 1 12))
                               (make-expr Const (list 'String "positive") (Location 1 13 1 23))
                               (make-expr Const (list 'String "non-positive") (Location 1 24 1 38)))
                         (Location 1 1 1 39))])
    (set! state (assert expr expected "If-then-else parsing test")))

  ;; If without else branch
  (let* ([input "(if (> x 0) \"positive\")"]
         [expr (parse (tokenize input))]
         [expected (make-expr If
                         (list #f
                               (make-expr App
                                    (list (make-expr Var (list ">") (Location 1 6 1 7))
                                          (list 
                                           (make-expr Var (list "x") (Location 1 8 1 9))
                                           (make-expr Const (list 'Num 0) (Location 1 10 1 11))))
                                    (Location 1 5 1 12))
                               (make-expr Const (list 'String "positive") (Location 1 13 1 23)))
                         (Location 1 1 1 24))])
    (set! state (assert expr expected "If-then (no else) parsing test")))

  ;; Nested if expressions
  (let* ([input "(if (> x 0) (if (> x 10) \"big\" \"medium\") \"non-positive\")"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) If "Nested if expressions parsing test")))

  state)

;; test-let-parsing
;;     Tests for let expression parsing
(define (test-let-parsing state output-fn)
  (output-fn "Running test-let-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple let
  (let* ([input "(let ((x 1) (y 2)) (+ x y))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) UnNamedLet "Simple let parsing test")))

  ;; Named let (for recursion)
  (let* ([input "(let loop ((i 0)) (if (< i 10) (loop (+ i 1)) i))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) NamedLet "Named let parsing test")))

  ;; Let with multiple bindings
  (let* ([input "(let ((a 1) (b 2) (c 3)) (+ a b c))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) UnNamedLet "Let with multiple bindings test")))

  ;; Let with internal definitions
  (let* ([input "(let ((x 1)) (define y 2) (+ x y))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) UnNamedLet "Let with internal definitions test")))

  ;; Empty bindings
  (let* ([input "(let () (display \"hello\"))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) UnNamedLet "Let with empty bindings test")))

  state)

;; test-let-variants-parsing
;;     Tests for let* and letrec expression parsing
(define (test-let-variants-parsing state output-fn)
  (output-fn "Running test-let-variants-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple let*
  (let* ([input "(let* ((x 1) (y (+ x 1))) (+ x y))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) LetStar "Simple let* parsing test")))

  ;; Let* with multiple sequential bindings
  (let* ([input "(let* ((a 1) (b (+ a 1)) (c (+ b 1))) (+ a b c))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) LetStar "Let* with sequential bindings test")))

  ;; Simple letrec
  (let* ([input "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) 
                          (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) 
                   (even? 10))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) LetRec "Simple letrec parsing test")))

  ;; Letrec with internal definitions
  (let* ([input "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) 
                   (define x 5) 
                   (fact x))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) LetRec "Letrec with internal definitions test")))

  state)

;; test-begin-parsing
;;     Tests for begin expression parsing
(define (test-begin-parsing state output-fn)
  (output-fn "Running test-begin-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple begin
  (let* ([input "(begin (display \"hello\") (display \"world\") (+ 1 2))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Begin "Simple begin parsing test")))

  ;; Empty begin
  (let* ([input "(begin)"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Begin "Empty begin parsing test")))

  ;; Begin with various expression types
  (let* ([input "(begin (define x 1) (set! x 2) (if (> x 0) x 0))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Begin "Begin with various expressions test")))

  state)

;; test-set-parsing
;;     Tests for set! expression parsing
(define (test-set-parsing state output-fn)
  (output-fn "Running test-set-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple set!
  (let* ([input "(set! x 42)"]
         [expr (parse (tokenize input))]
         [expected (make-expr Set!
                         (list (make-expr Var (list "x") (Location 1 7 1 8))
                               (make-expr Const (list 'Num 42) (Location 1 9 1 11)))
                         (Location 1 1 1 12))])
    (set! state (assert expr expected "Simple set! parsing test")))

  ;; Set! with complex expression
  (let* ([input "(set! counter (+ counter 1))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Set! "Set! with complex expression test")))

  state)

;; test-cond-parsing
;;     Tests for cond expression parsing
(define (test-cond-parsing state output-fn)
  (output-fn "Running test-cond-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple cond
  (let* ([input "(cond ((< x 0) \"negative\") 
                       ((> x 0) \"positive\") 
                       (else \"zero\"))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Cond "Simple cond parsing test")))

  ;; Cond without else
  (let* ([input "(cond ((< x 0) \"negative\") ((> x 0) \"positive\"))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Cond "Cond without else parsing test")))

  ;; Cond with multiple expressions in clause
  (let* ([input "(cond ((= n 0) (display \"zero\") 0) 
                       (else (display \"nonzero\") n))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Cond "Cond with multiple expressions parsing test")))

  state)

;; test-and-or-parsing
;;     Tests for and/or expression parsing
(define (test-and-or-parsing state output-fn)
  (output-fn "Running test-and-or-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple and
  (let* ([input "(and (> x 0) (< x 10))"]
         [expr (parse (tokenize input))]
         [expected (make-expr And
                         (list
                          (make-expr App
                               (list (make-expr Var (list ">") (Location 1 7 1 8))
                                     (list
                                      (make-expr Var (list "x") (Location 1 9 1 10))
                                      (make-expr Const (list 'Num 0) (Location 1 11 1 12))))
                               (Location 1 6 1 13))
                          (make-expr App
                               (list (make-expr Var (list "<") (Location 1 15 1 16))
                                     (list
                                      (make-expr Var (list "x") (Location 1 17 1 18))
                                      (make-expr Const (list 'Num 10) (Location 1 19 1 21))))
                               (Location 1 14 1 22)))
                         (Location 1 1 1 23))])
    (set! state (assert expr expected "Simple and parsing test")))

  ;; Empty and
  (let* ([input "(and)"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) And "Empty and parsing test")))

  ;; Simple or
  (let* ([input "(or (< x 0) (> x 10))"]
         [expr (parse (tokenize input))]
         [expected (make-expr Or
                         (list
                          (make-expr App
                               (list (make-expr Var (list "<") (Location 1 6 1 7))
                                     (list
                                      (make-expr Var (list "x") (Location 1 8 1 9))
                                      (make-expr Const (list 'Num 0) (Location 1 10 1 11))))
                               (Location 1 5 1 12))
                          (make-expr App
                               (list (make-expr Var (list ">") (Location 1 14 1 15))
                                     (list
                                      (make-expr Var (list "x") (Location 1 16 1 17))
                                      (make-expr Const (list 'Num 10) (Location 1 18 1 20))))
                               (Location 1 13 1 21)))
                         (Location 1 1 1 22))])
    (set! state (assert expr expected "Simple or parsing test")))

  ;; Empty or
  (let* ([input "(or)"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Or "Empty or parsing test")))

  ;; Multiple expressions
  (let* ([input "(and (> x 0) (< x 10) (even? x) #t)"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) And "And with multiple expressions test")))

  ;; Multiple expressions
  (let* ([input "(or (< x 0) (> x 10) (= x 5) #f)"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Or "Or with multiple expressions test")))

  state)

;; test-do-parsing
;;     Tests for do expression parsing
(define (test-do-parsing state output-fn)
  (output-fn "Running test-do-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple do
  (let* ([input "(do ((i 0 (+ i 1)) (sum 0 (+ sum i)))
                     ((= i 10) sum)
                   (display i))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Do "Simple do parsing test")))

  ;; Do with empty body
  (let* ([input "(do ((i 0 (+ i 1)))
                     ((> i 10) i))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Do "Do with empty body parsing test")))

  ;; Do with multiple test result expressions
  (let* ([input "(do ((i 0 (+ i 1)))
                     ((> i 10) (display i) i))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Do "Do with multiple test results test")))

  ;; Do with multiple body expressions
  (let* ([input "(do ((i 0 (+ i 1)))
                     ((> i 10) i)
                   (display i)
                   (display \"iteration\"))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Do "Do with multiple body expressions test")))

  state)

;; test-load-parsing
;;     Tests for load expression parsing
(define (test-load-parsing state output-fn)
  (output-fn "Running test-load-parsing...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple load
  (let* ([input "(load \"filename.scm\")"]
         [expr (parse (tokenize input))]
         [expected (make-expr Load
                         (list (make-expr Const (list 'String "filename.scm") (Location 1 7 1 21)))
                         (Location 1 1 1 22))])
    (set! state (assert expr expected "Simple load parsing test")))

  ;; Load with path
  (let* ([input "(load \"path/to/file.scm\")"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Load "Load with path parsing test")))

  state)

;; test-error-handling
;;     Tests for error handling in the parser
(define (test-error-handling state output-fn)
  (output-fn "Running test-error-handling...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))
  
  (define (test-parse-error input)
    (let ([result (parse (tokenize input))])
      (EtaError? result)))

  ;; Unmatched parenthesis
  (set! state (assert (test-parse-error "(+ 1 2")
                      #t
                      "Unmatched parenthesis error detection test"))

  state)

;; test-complex-expressions
;;     Tests for nested and mixed expressions
(define (test-complex-expressions state output-fn)
  (output-fn "Running test-complex-expressions...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Nested function application
  (let* ([input "(+ 1 (* 2 3))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) App "Nested function application test")))

  ;; Quote and application combination
  (let* ([input "(cons 'a '(b c))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) App "Quote and application combination test")))

  ;; Complex nested expression
  (let* ([input "(let ((x 1)) 
                   (if (> x 0) 
                       (begin 
                         (display \"positive\") 
                         (+ x 1)) 
                       0))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) UnNamedLet "Complex nested expression test")))

  ;; Factorial function definition
  (let* ([input "(define (factorial n)
                   (if (= n 0)
                       1
                       (* n (factorial (- n 1)))))"]
         [expr (parse (tokenize input))])
    (set! state (assert (Expr-head expr) Define "Factorial function definition test")))

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
                    test-lambda-parsing
                    test-define-parsing
                    test-if-parsing
                    test-let-parsing
                    test-let-variants-parsing
                    test-begin-parsing
                    test-set-parsing
                    test-cond-parsing
                    test-and-or-parsing
                    test-do-parsing
                    test-load-parsing
                    test-error-handling
                    test-complex-expressions)])
      (with-error-handling (lambda () (f s out))
        (symbol->string (object-name f)) s out))))
