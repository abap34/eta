#lang racket

(provide run-desugar-tests)
(require "test-utils.rkt"
         "../eta/parser/ast.rkt"
         "../eta/parser/tokenizer.rkt"
         "../eta/parser/parser.rkt"
         "../eta/desugar/desugar.rkt"
         "../eta/utils/location.rkt"
         "../eta/utils/error.rkt")

;; Helper function to simplify test creation
(define (parse-and-desugar-string input)
  (first (desugar (parse (tokenize input)))))

;; Helper function to create a dummy location for expected expressions
(define dummy-loc (Location 0 0 0 0))

;; test-and-desugaring
;;     Tests for 'and' expression desugaring into nested if expressions
(define (test-and-desugaring state output-fn)
  (output-fn "Running test-and-desugaring...")

  (define assert (lambda (a e m)
                   (assert-expr-struct-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Empty and - should desugar to #t
  (let* ([input "(and)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'ConstHead (list 'BoolConstNode #t) dummy-loc)])
    (set! state (assert expr expected "Empty 'and' desugaring test")))

  ;; Single argument 'and' - should remain as is
  (let* ([input "(and x)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'IdHead (list "x") dummy-loc)])
    (set! state (assert expr expected "Single argument 'and' desugaring test")))

  ;; Two argument 'and' - should become if
  (let* ([input "(and a b)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'IfHead 
                             (list 
                              (make-expr 'IdHead (list "a") dummy-loc)
                              (make-expr 'IdHead (list "b") dummy-loc)
                              (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc))
                             dummy-loc)])
    (set! state (assert expr expected "Two argument 'and' desugaring test")))

  ;; Three argument 'and' - should become nested if
  (let* ([input "(and a b c)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'IfHead 
                             (list 
                              (make-expr 'IdHead (list "a") dummy-loc)
                              (make-expr 'IfHead 
                                        (list 
                                         (make-expr 'IdHead (list "b") dummy-loc)
                                         (make-expr 'IdHead (list "c") dummy-loc)
                                         (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc))
                                        dummy-loc)
                              (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc))
                             dummy-loc)])
    (set! state (assert expr expected "Three argument 'and' desugaring test")))
  
  ;; 'and' with constant values
  (let* ([input "(and #t 42 \"hello\")"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'IfHead 
                             (list 
                              (make-expr 'ConstHead (list 'BoolConstNode #t) dummy-loc)
                              (make-expr 'IfHead 
                                        (list 
                                         (make-expr 'ConstHead (list 'IntConstNode 42) dummy-loc)
                                         (make-expr 'ConstHead (list 'StringConstNode "hello") dummy-loc)
                                         (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc))
                                        dummy-loc)
                              (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc))
                             dummy-loc)])
    (set! state (assert expr expected "Constants 'and' desugaring test")))
  
  ;; 'and' with complex expressions
  (let* ([input "(and (+ 1 2) (- 3 4) (* 5 6))"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'IfHead 
                             (list 
                              (make-expr 'AppHead 
                                        (list
                                         (make-expr 'IdHead (list "+") dummy-loc)
                                         (list 
                                          (make-expr 'ConstHead (list 'IntConstNode 1) dummy-loc)
                                          (make-expr 'ConstHead (list 'IntConstNode 2) dummy-loc)))
                                        dummy-loc)
                              (make-expr 'IfHead 
                                        (list 
                                         (make-expr 'AppHead 
                                                   (list
                                                    (make-expr 'IdHead (list "-") dummy-loc)
                                                    (list 
                                                     (make-expr 'ConstHead (list 'IntConstNode 3) dummy-loc)
                                                     (make-expr 'ConstHead (list 'IntConstNode 4) dummy-loc)))
                                                   dummy-loc)
                                         (make-expr 'AppHead 
                                                   (list
                                                    (make-expr 'IdHead (list "*") dummy-loc)
                                                    (list 
                                                     (make-expr 'ConstHead (list 'IntConstNode 5) dummy-loc)
                                                     (make-expr 'ConstHead (list 'IntConstNode 6) dummy-loc)))
                                                   dummy-loc)
                                         (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc))
                                        dummy-loc)
                              (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc))
                             dummy-loc)])
    (set! state (assert expr expected "Complex expressions 'and' desugaring test")))
  
  ;; Nested 'and' expressions
  (let* ([input "(and a (and b c) d)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'IfHead 
                             (list 
                              (make-expr 'IdHead (list "a") dummy-loc)
                              (make-expr 'IfHead 
                                        (list 
                                         ;; Inner and has been desugared to if
                                         (make-expr 'IfHead 
                                                   (list 
                                                    (make-expr 'IdHead (list "b") dummy-loc)
                                                    (make-expr 'IdHead (list "c") dummy-loc)
                                                    (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc))
                                                   dummy-loc)
                                         (make-expr 'IdHead (list "d") dummy-loc)
                                         (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc))
                                        dummy-loc)
                              (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc))
                             dummy-loc)])
    (set! state (assert expr expected "Nested 'and' desugaring test")))
  
  ;; 'and' with inline conditional - checks how conditionals inside and behave
  (let* ([input "(and (if x 1 2) (if y 3 4))"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'IfHead 
                             (list 
                              (make-expr 'IfHead 
                                        (list 
                                         (make-expr 'IdHead (list "x") dummy-loc)
                                         (make-expr 'ConstHead (list 'IntConstNode 1) dummy-loc)
                                         (make-expr 'ConstHead (list 'IntConstNode 2) dummy-loc))
                                        dummy-loc)
                              (make-expr 'IfHead 
                                        (list 
                                         (make-expr 'IdHead (list "y") dummy-loc)
                                         (make-expr 'ConstHead (list 'IntConstNode 3) dummy-loc)
                                         (make-expr 'ConstHead (list 'IntConstNode 4) dummy-loc))
                                        dummy-loc)
                              (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc))
                             dummy-loc)])
    (set! state (assert expr expected "Conditional 'and' desugaring test")))

  state)

;; test-or-desugaring
;;     Tests for 'or' expression desugaring into nested if/let expressions
(define (test-or-desugaring state output-fn)
  (output-fn "Running test-or-desugaring...")

  (define assert (lambda (a e m)
                   (assert-expr-struct-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Empty or - should desugar to #f
  (let* ([input "(or)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc)])
    (set! state (assert expr expected "Empty 'or' desugaring test")))

  ;; Single argument 'or' - should remain as is
  (let* ([input "(or x)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'IdHead (list "x") dummy-loc)])
    (set! state (assert expr expected "Single argument 'or' desugaring test")))

  ;; Two argument 'or' with constants - we can check actual structure without
  ;; worrying about gensym because we know the exact expected structure of constants
  (let* ([input "(or 5 #f)"]
         [expr (parse-and-desugar-string input)]
         ;; For constant values, we can predict the full AST structure
         ;; We expect: (let ([tmp 5]) (if tmp tmp #f))
         [expected-let-body (make-expr 'IfHead
                                     (list
                                      (make-expr 'IdHead (list "tmp") dummy-loc) ; Condition
                                      (make-expr 'IdHead (list "tmp") dummy-loc) ; Then branch
                                      (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc)) ; Else branch
                                     dummy-loc)]
         [expected-bindings (make-expr 'BindingsHead
                                     (list
                                      (list (make-expr 'BindHead
                                                     (list
                                                      (make-expr 'IdHead (list "tmp") dummy-loc)
                                                      (make-expr 'ConstHead (list 'IntConstNode 5) dummy-loc))
                                                     dummy-loc)))
                                     dummy-loc)]
         [expected (make-expr 'UnNamedLetHead
                            (list expected-bindings expected-let-body)
                            dummy-loc)])
    ;; We'll build expected structure with a generic variable name (tmp) but need to extract 
    ;; the actual variable name from expr to do proper comparison
    (let* ([bindings (first (Expr-args expr))]
           [binding (first (Expr-args bindings))]
           [var-expr (first (Expr-args binding))]
           [var-name (first (Expr-args var-expr))])
      ;; Check structure pattern by parts since we don't know the exact variable name
      (set! state (assert (Expr-head expr) 'UnNamedLetHead "Two argument constants 'or' produces let"))
      (set! state (assert (length (Expr-args expr)) 2 "Two argument constants 'or' has bindings and body"))
      ;; Check binding value is the first constant
      (let* ([bind-value (second (Expr-args binding))])
        (set! state (assert (Expr-head bind-value) 'ConstHead "Binding value is constant"))
        (set! state (assert (first (Expr-args bind-value)) 'IntConstNode "Binding value is an int")))
      
      ;; Check if structure in body uses variables correctly
      (let* ([body (second (Expr-args expr))]
             [if-args (Expr-args body)]
             [condition (first if-args)]
             [then-branch (second if-args)]
             [else-branch (third if-args)])
        (set! state (assert (Expr-head body) 'IfHead "Body contains if expression"))
        (set! state (assert (Expr-head condition) 'IdHead "Condition is variable"))
        (set! state (assert (Expr-head then-branch) 'IdHead "Then branch is variable"))
        ;; Both condition and then branch should use the same variable
        (set! state (assert (equal? (first (Expr-args condition)) (first (Expr-args then-branch))) 
                           "Condition and then branch use same variable"))
        ;; Else branch should be the second or argument: #f
        (set! state (assert (expr-struct-equal? else-branch
                                             (make-expr 'ConstHead (list 'BoolConstNode #f) dummy-loc))
                          "Else branch is #f")))))

  ;; Three argument 'or' - complex case with constants for easier verification
  (let* ([input "(or 10 20 30)"]
         [expr (parse-and-desugar-string input)])
    ;; Verify that we get a let expression with nested 'or'
    (set! state (assert (Expr-head expr) 'UnNamedLetHead "Three constant 'or' produces let expression"))
    
    ;; Get the actual structure parts
    (let* ([bindings (first (Expr-args expr))]
           [body (second (Expr-args expr))]
           [binding (first (Expr-args bindings))]
           [bind-value (second (Expr-args binding))])
      
      ;; Check that first binding is to the first constant
      (set! state (assert (expr-struct-equal? bind-value 
                                           (make-expr 'ConstHead (list 'IntConstNode 10) dummy-loc))
                        "First binding is to first constant"))
      
      ;; If structure should have another or expression in else branch
      (let* ([if-args (Expr-args body)]
             [else-branch (third if-args)])
        
        ;; Else branch should be a nested or (let) for (or 20 30)
        (set! state (assert (Expr-head else-branch) 'UnNamedLetHead "Else branch is nested or"))
        
        ;; Check the nested or structure
        (let* ([nested-bindings (first (Expr-args else-branch))]
               [nested-binding (first (Expr-args nested-bindings))]
               [nested-value (second (Expr-args nested-binding))])
          
          ;; Nested binding should be to the second constant
          (set! state (assert (expr-struct-equal? nested-value
                                               (make-expr 'ConstHead (list 'IntConstNode 20) dummy-loc))
                            "Nested binding is to second constant"))
          
          ;; Final else branch in the nested if should be the last constant
          (let* ([nested-body (second (Expr-args else-branch))]
                 [nested-if-args (Expr-args nested-body)]
                 [final-else (third nested-if-args)])
            (set! state (assert (expr-struct-equal? final-else
                                                 (make-expr 'ConstHead (list 'IntConstNode 30) dummy-loc))
                              "Final else branch is last constant")))))))

  ;; Or with complex expressions
  (let* ([input "(or (+ 1 2) (- 3 4) (* 5 6))"]
         [expr (parse-and-desugar-string input)])
    ;; The outer structure should be a let binding the first expression
    (set! state (assert (Expr-head expr) 'UnNamedLetHead "Complex or produces let"))
    
    (let* ([bindings (first (Expr-args expr))]
           [binding (first (Expr-args bindings))]
           [bind-value (second (Expr-args binding))])
      
      ;; Verify first binding is to (+ 1 2)
      (set! state (assert (Expr-head bind-value) 'AppHead "First binding is to an application"))
      (let* ([func (first (Expr-args bind-value))]
             [args (second (Expr-args bind-value))])
        (set! state (assert (expr-struct-equal? func (make-expr 'IdHead (list "+") dummy-loc))
                          "First binding is to addition"))
        (set! state (assert (= (length args) 2) "Addition has two arguments"))
        (set! state (assert (expr-struct-equal? (first args)
                                             (make-expr 'ConstHead (list 'IntConstNode 1) dummy-loc))
                          "First arg is 1"))
        (set! state (assert (expr-struct-equal? (second args)
                                             (make-expr 'ConstHead (list 'IntConstNode 2) dummy-loc))
                          "Second arg is 2"))))
    
    ;; Check nested or structure has the remaining expressions
    (let* ([body (second (Expr-args expr))]
           [if-args (Expr-args body)]
           [else-branch (third if-args)])
      
      (set! state (assert (Expr-head else-branch) 'UnNamedLetHead "Else branch is another or"))
      
      (let* ([nested-bindings (first (Expr-args else-branch))]
             [nested-binding (first (Expr-args nested-bindings))]
             [nested-value (second (Expr-args nested-binding))])
        
        ;; Second binding should be to (- 3 4)
        (set! state (assert (Expr-head nested-value) 'AppHead "Second binding is to an application"))
        (let* ([func (first (Expr-args nested-value))]
               [args (second (Expr-args nested-value))])
          (set! state (assert (expr-struct-equal? func (make-expr 'IdHead (list "-") dummy-loc))
                            "Second binding is to subtraction"))))))

  state)

;; test-cond-desugaring
;;     Tests for 'cond' expression desugaring into nested if expressions
(define (test-cond-desugaring state output-fn)
  (output-fn "Running test-cond-desugaring...")

  (define assert (lambda (a e m)
                   (assert-expr-struct-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Empty cond - should become void
  (let* ([input "(cond)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'ConstHead (list 'VoidConstNode '()) dummy-loc)])
    (set! state (assert expr expected "Empty cond desugaring test")))

  ;; Simple cond with one clause
  (let* ([input "(cond ((> x 0) 42))"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'IfHead
                             (list
                              (make-expr 'AppHead
                                        (list
                                         (make-expr 'IdHead (list ">") dummy-loc)
                                         (list
                                          (make-expr 'IdHead (list "x") dummy-loc)
                                          (make-expr 'ConstHead (list 'IntConstNode 0) dummy-loc)))
                                        dummy-loc)
                              (make-expr 'ConstHead (list 'IntConstNode 42) dummy-loc)
                              (make-expr 'ConstHead (list 'VoidConstNode '()) dummy-loc))
                             dummy-loc)])
    (set! state (assert expr expected "Single clause cond desugaring test")))

  ;; Simple cond with else
  (let* ([input "(cond ((> x 0) \"positive\") (else \"non-positive\"))"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'IfHead
                             (list
                              (make-expr 'AppHead
                                        (list
                                         (make-expr 'IdHead (list ">") dummy-loc)
                                         (list
                                          (make-expr 'IdHead (list "x") dummy-loc)
                                          (make-expr 'ConstHead (list 'IntConstNode 0) dummy-loc)))
                                        dummy-loc)
                              (make-expr 'ConstHead (list 'StringConstNode "positive") dummy-loc)
                              (make-expr 'ConstHead (list 'StringConstNode "non-positive") dummy-loc))
                             dummy-loc)])
    (set! state (assert expr expected "Cond with else desugaring test")))

  ;; Multiple clauses
  (let* ([input "(cond ((= x 0) \"zero\") ((> x 0) \"positive\"))"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'IfHead
                             (list
                              (make-expr 'AppHead
                                        (list
                                         (make-expr 'IdHead (list "=") dummy-loc)
                                         (list
                                          (make-expr 'IdHead (list "x") dummy-loc)
                                          (make-expr 'ConstHead (list 'IntConstNode 0) dummy-loc)))
                                        dummy-loc)
                              (make-expr 'ConstHead (list 'StringConstNode "zero") dummy-loc)
                              (make-expr 'IfHead
                                        (list
                                         (make-expr 'AppHead
                                                   (list
                                                    (make-expr 'IdHead (list ">") dummy-loc)
                                                    (list
                                                     (make-expr 'IdHead (list "x") dummy-loc)
                                                     (make-expr 'ConstHead (list 'IntConstNode 0) dummy-loc)))
                                                   dummy-loc)
                                         (make-expr 'ConstHead (list 'StringConstNode "positive") dummy-loc)
                                         (make-expr 'ConstHead (list 'VoidConstNode '()) dummy-loc))
                                        dummy-loc))
                             dummy-loc)])
    (set! state (assert expr expected "Multiple clauses cond desugaring test")))

  ;; Multiple clauses with else
  (let* ([input "(cond ((= x 0) \"zero\") ((< x 0) \"negative\") (else \"positive\"))"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'IfHead
                             (list
                              (make-expr 'AppHead
                                        (list
                                         (make-expr 'IdHead (list "=") dummy-loc)
                                         (list
                                          (make-expr 'IdHead (list "x") dummy-loc)
                                          (make-expr 'ConstHead (list 'IntConstNode 0) dummy-loc)))
                                        dummy-loc)
                              (make-expr 'ConstHead (list 'StringConstNode "zero") dummy-loc)
                              (make-expr 'IfHead
                                        (list
                                         (make-expr 'AppHead
                                                   (list
                                                    (make-expr 'IdHead (list "<") dummy-loc)
                                                    (list
                                                     (make-expr 'IdHead (list "x") dummy-loc)
                                                     (make-expr 'ConstHead (list 'IntConstNode 0) dummy-loc)))
                                                   dummy-loc)
                                         (make-expr 'ConstHead (list 'StringConstNode "negative") dummy-loc)
                                         (make-expr 'ConstHead (list 'StringConstNode "positive") dummy-loc))
                                        dummy-loc))
                             dummy-loc)])
    (set! state (assert expr expected "Multiple clauses with else cond desugaring test")))
  
  ;; Nested cond expressions
  (let* ([input "(cond ((= x 0) \"zero\") 
                      ((> x 0) (cond ((< x 10) \"small positive\")
                                     ((< x 100) \"medium positive\")
                                     (else \"large positive\")))
                      (else \"negative\"))"]
         [expr (parse-and-desugar-string input)])
    
    ;; Outer structure should be if from cond
    (set! state (assert (Expr-head expr) 'IfHead "Nested cond produces if"))
    
    ;; Then check the nested structure
    (let* ([if-args (Expr-args expr)]
           [condition (first if-args)]
           [then-branch (second if-args)]
           [else-branch (third if-args)])
      
      ;; First clause checks x = 0
      (set! state (assert (Expr-head condition) 'AppHead "First condition is application"))
      (set! state (assert (expr-struct-equal? (first (Expr-args condition))
                                           (make-expr 'IdHead (list "=") dummy-loc))
                        "First condition uses ="))
      (set! state (assert (expr-struct-equal? then-branch
                                           (make-expr 'ConstHead (list 'StringConstNode "zero") dummy-loc))
                        "First then branch returns \"zero\""))
      
      ;; The else branch should contain another if for the second cond clause 
      (set! state (assert (Expr-head else-branch) 'IfHead "Else branch contains another if"))
      (let* ([nested-if-args (Expr-args else-branch)]
             [nested-condition (first nested-if-args)]
             [nested-then (second nested-if-args)]
             [nested-else (third nested-if-args)])
        
        ;; Second clause checks x > 0
        (set! state (assert (Expr-head nested-condition) 'AppHead "Second condition is application"))
        (set! state (assert (expr-struct-equal? (first (Expr-args nested-condition))
                                             (make-expr 'IdHead (list ">") dummy-loc))
                          "Second condition uses >"))
        
        ;; The nested 'then' branch should be another if from the inner cond
        (set! state (assert (Expr-head nested-then) 'IfHead "Nested then branch is another if"))
        
        ;; The inner cond should have three clauses: two conditions and an else
        (let* ([inner-if-args (Expr-args nested-then)]
               [inner-condition (first inner-if-args)]
               [inner-then (second inner-if-args)]
               [inner-else (third inner-if-args)])
          
          ;; First inner condition checks x < 10
          (set! state (assert (Expr-head inner-condition) 'AppHead "Inner condition is application"))
          (set! state (assert (expr-struct-equal? (first (Expr-args inner-condition))
                                               (make-expr 'IdHead (list "<") dummy-loc))
                            "Inner condition uses <"))
          
          ;; First inner then branch should return "small positive"
          (set! state (assert (expr-struct-equal? inner-then
                                               (make-expr 'ConstHead 
                                                        (list 'StringConstNode "small positive")
                                                        dummy-loc))
                            "Inner then returns \"small positive\""))
          
          ;; Inner else should be another if for second inner clause
          (set! state (assert (Expr-head inner-else) 'IfHead "Inner else is another if"))
          
          ;; Check final else clause returns "large positive"
          (let* ([final-if-args (Expr-args inner-else)]
                 [final-else (third final-if-args)])
            (set! state (assert (expr-struct-equal? final-else
                                                 (make-expr 'ConstHead 
                                                          (list 'StringConstNode "large positive")
                                                          dummy-loc))
                              "Final inner clause returns \"large positive\"")))))
      
      ;; The outer final else should return "negative"
      (let* ([outer-else-branch (third (Expr-args else-branch))])
        (set! state (assert (expr-struct-equal? outer-else-branch
                                             (make-expr 'ConstHead 
                                                      (list 'StringConstNode "negative")
                                                      dummy-loc))
                          "Outer final else returns \"negative\"")))))
  
  ;; Cond with and/or in the conditions
  (let* ([input "(cond ((and (> x 0) (< x 10)) \"small\")
                      ((or (= x 0) (>= x 10)) \"not small\"))"]
         [expr (parse-and-desugar-string input)])
    
    ;; Should be an if expression
    (set! state (assert (Expr-head expr) 'IfHead "Cond with and/or produces if"))
    
    ;; First condition should be an if from 'and'
    (let* ([if-args (Expr-args expr)]
           [condition (first if-args)]
           [then-branch (second if-args)]
           [else-branch (third if-args)])
      
      ;; Condition is from and expression desugaring
      (set! state (assert (Expr-head condition) 'IfHead "First condition is if from and"))
      
      ;; Then branch should be "small"
      (set! state (assert (expr-struct-equal? then-branch
                                           (make-expr 'ConstHead (list 'StringConstNode "small") dummy-loc))
                        "First then branch is \"small\""))
      
      ;; Else branch should be another if for second condition
      (set! state (assert (Expr-head else-branch) 'IfHead "Else branch is another if"))
      
      ;; Second condition is from or expression desugaring
      (let* ([nested-if-args (Expr-args else-branch)]
             [nested-condition (first nested-if-args)])
        
        ;; Condition should be from or desugaring (which is a let)
        (set! state (assert (Expr-head nested-condition) 'UnNamedLetHead "Second condition is from or")))))
  
  ;; Cond with expressions in the consequent position
  (let* ([input "(cond ((> x 0) (+ x 1)) ((< x 0) (- x 1)) (else x))"]
         [expr (parse-and-desugar-string input)])
    
    ;; Should be an if expression
    (set! state (assert (Expr-head expr) 'IfHead "Cond with expressions produces if"))
    
    ;; Check that then and else branches have the correct expressions
    (let* ([if-args (Expr-args expr)]
           [then-branch (second if-args)]
           [else-branch (third if-args)])
      
      ;; First then branch should be (+ x 1)
      (set! state (assert (Expr-head then-branch) 'AppHead "First then branch is addition"))
      (set! state (assert (expr-struct-equal? (first (Expr-args then-branch))
                                           (make-expr 'IdHead (list "+") dummy-loc))
                        "First then uses +"))
      
      ;; Second if should have second expression as then branch
      (let* ([nested-if-args (Expr-args else-branch)]
             [nested-then (second nested-if-args)]
             [nested-else (third nested-if-args)])
        
        ;; Second then branch should be (- x 1)
        (set! state (assert (Expr-head nested-then) 'AppHead "Second then branch is subtraction"))
        (set! state (assert (expr-struct-equal? (first (Expr-args nested-then))
                                             (make-expr 'IdHead (list "-") dummy-loc))
                          "Second then uses -"))
        
        ;; Final else should be just x
        (set! state (assert (expr-struct-equal? nested-else
                                             (make-expr 'IdHead (list "x") dummy-loc))
                          "Final else is x")))))
  
  state)

;; test-unnamed-let-desugaring
;;     Tests for 'let' expression desugaring into lambda applications
(define (test-unnamed-let-desugaring state output-fn)
  (output-fn "Running test-unnamed-let-desugaring...")

  (define assert (lambda (a e m)
                   (assert-expr-struct-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Simple let
  (let* ([input "(let ((x 1)) x)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'AppHead
                             (list
                              (make-expr 'LambdaHead
                                        (list
                                         (make-expr 'ArgHead (list (list "x") '()) dummy-loc)
                                         (make-expr 'BodyHead
                                                   (list 
                                                    '()
                                                    (list (make-expr 'IdHead (list "x") dummy-loc)))
                                                   dummy-loc))
                                        dummy-loc)
                              (list (make-expr 'ConstHead (list 'IntConstNode 1) dummy-loc)))
                             dummy-loc)])
    (set! state (assert expr expected "Simple let desugaring test")))

  ;; Empty bindings
  (let* ([input "(let () 42)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'AppHead
                             (list
                              (make-expr 'LambdaHead
                                        (list
                                         (make-expr 'ArgHead (list '() '()) dummy-loc)
                                         (make-expr 'BodyHead
                                                   (list 
                                                    '()
                                                    (list (make-expr 'ConstHead 
                                                                    (list 'IntConstNode 42) 
                                                                    dummy-loc)))
                                                   dummy-loc))
                                        dummy-loc)
                              '())
                             dummy-loc)])
    (set! state (assert expr expected "Empty bindings let desugaring test")))

  ;; Multiple bindings
  (let* ([input "(let ((a 1) (b 2)) (+ a b))"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'AppHead
                             (list
                              (make-expr 'LambdaHead
                                        (list
                                         (make-expr 'ArgHead (list (list "a" "b") '()) dummy-loc)
                                         (make-expr 'BodyHead
                                                   (list 
                                                    '()
                                                    (list (make-expr 'AppHead
                                                                    (list
                                                                     (make-expr 'IdHead (list "+") dummy-loc)
                                                                     (list
                                                                      (make-expr 'IdHead (list "a") dummy-loc)
                                                                      (make-expr 'IdHead (list "b") dummy-loc)))
                                                                    dummy-loc)))
                                                   dummy-loc))
                                        dummy-loc)
                              (list 
                               (make-expr 'ConstHead (list 'IntConstNode 1) dummy-loc)
                               (make-expr 'ConstHead (list 'IntConstNode 2) dummy-loc)))
                             dummy-loc)])
    (set! state (assert expr expected "Multiple bindings let desugaring test")))

  state)

;; test-letstar-desugaring
;;     Tests for 'let*' expression desugaring into nested let expressions
(define (test-letstar-desugaring state output-fn)
  (output-fn "Running test-letstar-desugaring...")

  (define assert (lambda (a e m)
                   (assert-expr-struct-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Empty bindings
  (let* ([input "(let* () 42)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'ConstHead (list 'IntConstNode 42) dummy-loc)])
    (set! state (assert expr expected "Empty bindings let* desugaring test")))

  ;; Simple let* (one binding - same as let)
  (let* ([input "(let* ((x 1)) x)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'AppHead
                             (list
                              (make-expr 'LambdaHead
                                        (list
                                         (make-expr 'ArgHead (list (list "x") '()) dummy-loc)
                                         (make-expr 'BodyHead
                                                   (list 
                                                    '()
                                                    (list (make-expr 'IdHead (list "x") dummy-loc)))
                                                   dummy-loc))
                                        dummy-loc)
                              (list (make-expr 'ConstHead (list 'IntConstNode 1) dummy-loc)))
                             dummy-loc)])
    (set! state (assert expr expected "Single binding let* desugaring test")))
  
  ;; Multiple nested bindings
  (let* ([input "(let* ((a 1) (b (+ a 2))) (+ a b))"]
         [expr (parse-and-desugar-string input)])
    
    ;; Outermost structure should be an application (from first let)
    (set! state (assert (Expr-head expr) 'AppHead "Multiple let* produces AppHead"))
    
    ;; First lambda should bind only 'a'
    (let* ([outer-lambda (first (Expr-args expr))]
           [outer-args (first (Expr-args outer-lambda))]
           [outer-vars (first (Expr-args outer-args))])
      (set! state (assert (equal? outer-vars (list "a")) "First level of let* binds only first var"))
      
      ;; Body should contain another let which binds 'b'
      (let* ([outer-body (second (Expr-args outer-lambda))]
             [outer-exprs (second (Expr-args outer-body))]
             [inner-let (first outer-exprs)])
        
        (set! state (assert (Expr-head inner-let) 'AppHead "Second level is another let (application)"))
        
        ;; Inner lambda should bind 'b'
        (let* ([inner-lambda (first (Expr-args inner-let))]
               [inner-args (first (Expr-args inner-lambda))]
               [inner-vars (first (Expr-args inner-args))])
          (set! state (assert (equal? inner-vars (list "b")) "Second level let* binds second var")))
        
        ;; Value for 'b' should reference 'a'
        (let* ([inner-values (second (Expr-args inner-let))]
               [b-value (first inner-values)]
               [b-expr (Expr-args b-value)])
          (set! state (assert (equal? (Expr-head b-value) 'AppHead) "Second binding uses first var"))
          (set! state (assert (equal? (Expr-head (second (first b-expr))) 'IdHead) "Application contains var"))
          (set! state (assert (equal? (first (Expr-args (second (first b-expr)))) "a") "Application uses 'a'"))))))
  
  ;; Variable shadowing with let*
  (let* ([input "(let* ((x 1) (x (+ x 1)) (x (* x 2))) x)"]
         [expr (parse-and-desugar-string input)])
    
    ;; Should be an application (first let)
    (set! state (assert (Expr-head expr) 'AppHead "Shadowing let* produces application"))
    
    ;; First lambda should bind the first 'x' to 1
    (let* ([outer-lambda (first (Expr-args expr))]
           [outer-args (first (Expr-args outer-lambda))]
           [outer-vars (first (Expr-args outer-args))]
           [outer-vals (second (Expr-args expr))])
      (set! state (assert (equal? outer-vars (list "x")) "First let* binds x"))
      (set! state (assert (expr-struct-equal? (first outer-vals)
                                           (make-expr 'ConstHead (list 'IntConstNode 1) dummy-loc))
                        "First x is bound to 1"))
      
      ;; Body should contain another let with second 'x'
      (let* ([outer-body (second (Expr-args outer-lambda))]
             [outer-exprs (second (Expr-args outer-body))]
             [second-let (first outer-exprs)])
        
        ;; Second let should bind x to (+ x 1)
        (let* ([second-lambda (first (Expr-args second-let))]
               [second-args (first (Expr-args second-lambda))]
               [second-vars (first (Expr-args second-args))]
               [second-vals (second (Expr-args second-let))]
               [second-val (first second-vals)])
          (set! state (assert (equal? second-vars (list "x")) "Second let* binds x again"))
          (set! state (assert (Expr-head second-val) 'AppHead "Second x bound to addition"))
          
          ;; Third let should bind x to (* x 2)  
          (let* ([second-body (second (Expr-args second-lambda))]
                 [second-exprs (second (Expr-args second-body))]
                 [third-let (first second-exprs)]
                 [third-lambda (first (Expr-args third-let))]
                 [third-args (first (Expr-args third-lambda))]
                 [third-vars (first (Expr-args third-args))]
                 [third-vals (second (Expr-args third-let))]
                 [third-val (first third-vals)])
            (set! state (assert (equal? third-vars (list "x")) "Third let* binds x again"))
            (set! state (assert (Expr-head third-val) 'AppHead "Third x bound to multiplication"))
            
            ;; The final body should just be the latest x
            (let* ([third-body (second (Expr-args third-lambda))]
                   [third-exprs (second (Expr-args third-body))]
                   [final-expr (first third-exprs)])
              (set! state (assert (Expr-head final-expr) 'IdHead "Final body references x"))
              (set! state (assert (equal? (first (Expr-args final-expr)) "x") "Final body returns x"))))))))
  
  ;; Complex let* with cond
  (let* ([input "(let* ((a 5) (b (if (> a 0) (* 2 a) 0)) (c (cond ((= b 0) 0) ((< b 10) 1) (else 2)))) 
                    (+ a b c))"]
         [expr (parse-and-desugar-string input)])
    
    ;; Should be a nested application structure
    (set! state (assert (Expr-head expr) 'AppHead "Complex let* produces application"))
    
    ;; First let binds 'a' to 5
    (let* ([first-lambda (first (Expr-args expr))]
           [first-arg-vals (second (Expr-args expr))])
      (set! state (assert (expr-struct-equal? (first first-arg-vals)
                                           (make-expr 'ConstHead (list 'IntConstNode 5) dummy-loc))
                        "First binding sets a to 5"))
      
      ;; Second let binds 'b' to an if expression
      (let* ([first-body (second (Expr-args first-lambda))]
             [first-exprs (second (Expr-args first-body))]
             [second-let (first first-exprs)]
             [second-lambda (first (Expr-args second-let))]
             [second-vals (second (Expr-args second-let))]
             [b-val (first second-vals)])
        (set! state (assert (Expr-head b-val) 'IfHead "b is bound to if expression"))
        
        ;; Third let binds 'c' to a cond expression (which becomes if)
        (let* ([second-body (second (Expr-args second-lambda))]
               [second-exprs (second (Expr-args second-body))]
               [third-let (first second-exprs)]
               [third-lambda (first (Expr-args third-let))]
               [third-vals (second (Expr-args third-let))]
               [c-val (first third-vals)])
          (set! state (assert (Expr-head c-val) 'IfHead "c is bound to transformed cond"))
          
          ;; Final body should be an addition
          (let* ([third-body (second (Expr-args third-lambda))]
                 [third-exprs (second (Expr-args third-body))]
                 [final-expr (first third-exprs)])
            (set! state (assert (Expr-head final-expr) 'AppHead "Final expression is application"))
            (let ([func (first (Expr-args final-expr))])
              (set! state (assert (expr-struct-equal? func (make-expr 'IdHead (list "+") dummy-loc))
                                "Final expression is addition"))))))))

  state)

;; test-letrec-desugaring
;;     Tests for 'letrec' expression desugaring into let with set! expressions
(define (test-letrec-desugaring state output-fn)
  (output-fn "Running test-letrec-desugaring...")

  (define assert (lambda (a e m)
                   (assert-expr-struct-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Empty bindings
  (let* ([input "(letrec () 42)"]
         [expr (parse-and-desugar-string input)]
         [expected (make-expr 'AppHead
                             (list
                              (make-expr 'LambdaHead
                                        (list
                                         (make-expr 'ArgHead (list '() '()) dummy-loc)
                                         (make-expr 'BodyHead
                                                   (list 
                                                    '()
                                                    (list (make-expr 'ConstHead 
                                                                    (list 'IntConstNode 42) 
                                                                    dummy-loc)))
                                                   dummy-loc))
                                        dummy-loc)
                              '())
                             dummy-loc)])
    (set! state (assert expr expected "Empty bindings letrec desugaring test")))
    
  ;; Simple letrec structure - detailed structure verification
  (let* ([input "(letrec ((x 1)) x)"]
         [expr (parse-and-desugar-string input)])
    
    ;; Check overall structure - should be application of lambda
    (set! state (assert (Expr-head expr) 'AppHead "Simple letrec main structure test"))
    
    ;; The lambda should have no arguments (empty arg list)
    (let* ([lambda-expr (first (Expr-args expr))]
           [args (first (Expr-args lambda-expr))]
           [vars (first (Expr-args args))])
      (set! state (assert (null? vars) "Simple letrec lambda has empty arg list"))
      
      ;; The body should be a begin expression with set! and the final expression
      (let* ([body (second (Expr-args lambda-expr))]
             [body-exprs (second (Expr-args body))]
             [begin-expr (first body-exprs)])
        
        (set! state (assert (Expr-head begin-expr) 'BeginHead "Simple letrec has begin in body"))
        
        ;; Begin should contain a set! followed by the body expression
        (let ([begin-exprs (Expr-args begin-expr)])
          (set! state (assert (= (length begin-exprs) 2) "Simple letrec begin has two expressions"))
          (set! state (assert (Expr-head (first begin-exprs)) 'SetHead "First expr in begin is set!"))
          (set! state (assert (Expr-head (second begin-exprs)) 'IdHead "Second expr in begin is the body"))))))

  ;; Mutually recursive letrec - structure pattern verification
  (let* ([input "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) 
                          (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) 
                   (even? 10))"]
         [expr (parse-and-desugar-string input)])
    
    ;; Verify the application structure
    (set! state (assert (Expr-head expr) 'AppHead "Mutual recursion letrec produces application"))
    
    (let* ([lambda-expr (first (Expr-args expr))]
           [body (second (Expr-args lambda-expr))]
           [body-exprs (second (Expr-args body))]
           [begin-expr (first body-exprs)])
      
      ;; Begin should contain two set! expressions for the two bindings
      (let ([begin-exprs (Expr-args begin-expr)])
        (set! state (assert (= (length begin-exprs) 3) "Mutual recursion has two set! plus application"))
        (set! state (assert (Expr-head (first begin-exprs)) 'SetHead "First expr sets even?"))
        (set! state (assert (Expr-head (second begin-exprs)) 'SetHead "Second expr sets odd?"))
        
        ;; Verify application of even?
        (set! state (assert (Expr-head (third begin-exprs)) 'AppHead "Third expression is application"))
        
        ;; Verify recursive references in function bodies
        (let* ([set-even (first begin-exprs)]
               [set-odd (second begin-exprs)]
               [even-lambda (third (Expr-args set-even))]
               [odd-lambda (third (Expr-args set-odd))])
          
          ;; Both should be lambdas
          (set! state (assert (Expr-head even-lambda) 'LambdaHead "even? is bound to lambda"))
          (set! state (assert (Expr-head odd-lambda) 'LambdaHead "odd? is bound to lambda"))
          
          ;; Check for references to the other function in if expressions
          (let* ([even-body (second (Expr-args even-lambda))]
                 [even-exprs (second (Expr-args even-body))]
                 [even-if (first even-exprs)]
                 [odd-body (second (Expr-args odd-lambda))]
                 [odd-exprs (second (Expr-args odd-body))]
                 [odd-if (first odd-exprs)])
            
            (set! state (assert (Expr-head even-if) 'IfHead "even? has if expression"))
            (set! state (assert (Expr-head odd-if) 'IfHead "odd? has if expression"))
            
            ;; In the even? function, check for reference to odd? in the else branch
            (let* ([even-if-branches (Expr-args even-if)]
                   [even-else (third even-if-branches)]
                   [odd-if-branches (Expr-args odd-if)]
                   [odd-else (third odd-if-branches)])
              
              ;; In even? else branch, should call odd?
              (when (Expr-head even-else) 
                (set! state (assert (equal? (Expr-head even-else) 'AppHead) "even? else calls odd?"))
                (when (equal? (Expr-head even-else) 'AppHead)
                  (set! state (assert (equal? (Expr-head (first (Expr-args even-else))) 'IdHead) "even? calls function"))))
              
              ;; In odd? else branch, should call even?
              (when (Expr-head odd-else)
                (set! state (assert (equal? (Expr-head odd-else) 'AppHead) "odd? else calls even?"))
                (when (equal? (Expr-head odd-else) 'AppHead)
                  (set! state (assert (equal? (Expr-head (first (Expr-args odd-else))) 'IdHead) "odd? calls function"))))))))))
  
  state)

;; test-named-let-desugaring
;;     Tests for named 'let' expression desugaring into letrec with function application
(define (test-named-let-desugaring state output-fn)
  (output-fn "Running test-named-let-desugaring...")

  (define assert (lambda (a e m)
                   (assert-expr-struct-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Named let - detailed structure verification
  (let* ([input "(let loop ((i 0)) (if (< i 5) (loop (+ i 1)) i))"]
         [expr (parse-and-desugar-string input)])
    
    ;; Should be an application of lambda
    (set! state (assert (Expr-head expr) 'AppHead "Named let main structure test"))
    
    ;; The first argument should be a lambda that takes no arguments
    (let* ([lambda-expr (first (Expr-args expr))]
           [args (first (Expr-args lambda-expr))]
           [vars (first (Expr-args args))])
      (set! state (assert (Expr-head lambda-expr) 'LambdaHead "Named let lambda structure test"))
      (set! state (assert (null? vars) "Named let lambda has no parameters"))
      
      ;; Body should have begin with set! and if-expression
      (let* ([body (second (Expr-args lambda-expr))]
             [body-exprs (second (Expr-args body))]
             [begin-expr (first body-exprs)])
        
        (set! state (assert (Expr-head begin-expr) 'BeginHead "Named let has begin in body"))
        
        ;; Begin should contain a set! for the loop variable and the body expression
        (let ([begin-exprs (Expr-args begin-expr)])
          (set! state (assert (>= (length begin-exprs) 2) "Named let begin has at least set! and body"))
          (set! state (assert (Expr-head (first begin-exprs)) 'SetHead "First expr in begin is set!"))
          
          ;; The set! should bind 'loop' to a lambda
          (let* ([set-expr (first begin-exprs)]
                 [set-var (first (Expr-args set-expr))]
                 [set-val (second (Expr-args set-expr))])
            (set! state (assert (equal? (Expr-head set-var) 'IdHead) "Set! target is identifier"))
            (set! state (assert (equal? (first (Expr-args set-var)) "loop") "Set! target is loop"))
            (set! state (assert (equal? (Expr-head set-val) 'LambdaHead) "Set! value is lambda"))
            
            ;; The loop body should contain an if expression
            (let* ([last-expr (car (reverse begin-exprs))]
                   [app-expr (if (equal? (Expr-head last-expr) 'AppHead) 
                                last-expr
                                (car (reverse (cdr begin-exprs))))])
              
              ;; Application should be to a function with a parameter
              (when (equal? (Expr-head app-expr) 'AppHead)
                (set! state (assert (equal? (length (Expr-args app-expr)) 2) "Application has function and args")))
              
              ;; The loop function should have one parameter 'i'
              (let* ([loop-lambda (second (Expr-args set-expr))]
                     [loop-args (first (Expr-args loop-lambda))]
                     [loop-vars (first (Expr-args loop-args))])
                (set! state (assert (equal? (length loop-vars) 1) "Loop function has one parameter"))
                (set! state (assert (equal? (first loop-vars) "i") "Loop parameter is i")))))))))
  
  ;; Named let with multiple parameters
  (let* ([input "(let sum ((a 1) (b 10)) (if (= a 0) b (sum (- a 1) (+ b a))))"]
         [expr (parse-and-desugar-string input)])
    
    ;; Should be an application
    (set! state (assert (Expr-head expr) 'AppHead "Multi-param named let main structure test"))
    
    ;; Check that the recursive loop function has multiple parameters
    (let* ([lambda-expr (first (Expr-args expr))]
           [body (second (Expr-args lambda-expr))]
           [body-exprs (second (Expr-args body))]
           [begin-expr (first body-exprs)]
           [set-expr (first (Expr-args begin-expr))]
           [loop-lambda (second (Expr-args set-expr))]
           [loop-args (first (Expr-args loop-lambda))]
           [loop-vars (first (Expr-args loop-args))])
      
      ;; Check proper number of parameters
      (set! state (assert (equal? (length loop-vars) 2) "Multi-param loop has two parameters"))
      (set! state (assert (equal? (first loop-vars) "a") "First parameter is a"))
      (set! state (assert (equal? (second loop-vars) "b") "Second parameter is b"))
      
      ;; Check initial values passed to the loop
      (let* ([app-values (second (Expr-args expr))])
        (set! state (assert (= (length app-values) 2) "Two initial values passed to loop"))
        (set! state (assert (equal? (Expr-head (first app-values)) 'ConstHead) "First initial value is constant"))
        (set! state (assert (equal? (Expr-head (second app-values)) 'ConstHead) "Second initial value is constant")))))
  
  state)

;; test-complex-desugaring
;;     Tests for complex nested expressions
(define (test-complex-desugaring state output-fn)
  (output-fn "Running test-complex-desugaring...")

  (define assert (lambda (a e m)
                   (assert-expr-struct-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Test combined and/or structures
  (let* ([input "(and #t (or #f 42))"]
         [expr (parse-and-desugar-string input)])
    
    ;; Top level should be an if from the 'and'
    (set! state (assert (Expr-head expr) 'IfHead "Combined and/or produces if expression"))
    
    ;; First arg should be #t
    (let* ([if-args (Expr-args expr)]
           [condition (first if-args)]
           [then-branch (second if-args)])
      
      ;; Condition should be #t
      (set! state (assert (expr-struct-equal? condition 
                                            (make-expr 'ConstHead (list 'BoolConstNode #t) dummy-loc))
                         "And/or combined condition is #t"))
      
      ;; Then branch should be an or expression (UnNamedLetHead)
      (set! state (assert (Expr-head then-branch) 'UnNamedLetHead "Then branch is or expression"))
      
      ;; Or structure - body should be if expression
      (let* ([or-body (second (Expr-args then-branch))])
        (set! state (assert (Expr-head or-body) 'IfHead "Or body is if expression"))
        
        ;; Else branch of if (or) should be 42
        (let ([or-else (third (Expr-args or-body))])
          (set! state (assert (expr-struct-equal? or-else
                                               (make-expr 'ConstHead (list 'IntConstNode 42) dummy-loc))
                            "Or else branch is 42"))))))

  ;; Test cond + let combination
  (let* ([input "(let ((x 10)) (cond ((> x 0) \"positive\") (else \"non-positive\")))"]
         [expr (parse-and-desugar-string input)])
    
    ;; Outermost should be an application from let
    (set! state (assert (Expr-head expr) 'AppHead "Let + cond produces application"))
    
    ;; Check that the lambda body transforms cond to if
    (let* ([lambda-expr (first (Expr-args expr))]
           [body (second (Expr-args lambda-expr))]
           [body-exprs (second (Expr-args body))]
           [if-expr (first body-exprs)])
      
      ;; Body should be if from cond desugaring
      (set! state (assert (Expr-head if-expr) 'IfHead "Let body contains if from cond"))
      
      ;; If condition should reference x
      (let* ([if-args (Expr-args if-expr)]
             [condition (first if-args)]
             [condition-args (Expr-args condition)])
        
        ;; Condition should be an application (> x 0)
        (set! state (assert (Expr-head condition) 'AppHead "If condition is application"))
        (set! state (assert (expr-struct-equal? (first condition-args)
                                             (make-expr 'IdHead (list ">") dummy-loc))
                          "Application is >")))
        
      ;; Check that the if branches have the expected string constants
      (let* ([then-branch (second (Expr-args if-expr))]
             [else-branch (third (Expr-args if-expr))])
        (set! state (assert (expr-struct-equal? then-branch
                                             (make-expr 'ConstHead (list 'StringConstNode "positive") dummy-loc))
                          "If then branch is \"positive\""))
        (set! state (assert (expr-struct-equal? else-branch
                                             (make-expr 'ConstHead (list 'StringConstNode "non-positive") dummy-loc))
                          "If else branch is \"non-positive\"")))))

  ;; Test complex nested structure with multiple forms
  (let* ([input "(let ((a 1))
                   (letrec ((fact (lambda (n) 
                                    (if (= n 0) 
                                        1 
                                        (* n (fact (- n 1)))))))
                     (cond
                       ((< a 0) \"negative\")
                       ((and (> a 0) (< a 10)) (fact a))
                       (else \"too large\"))))"]
         [expr (parse-and-desugar-string input)])
    
    ;; Outer structure should be application (from let)
    (set! state (assert (Expr-head expr) 'AppHead "Complex nested structure is application"))
    
    ;; Get the body of the outer let
    (let* ([lambda-expr (first (Expr-args expr))]
           [body (second (Expr-args lambda-expr))]
           [body-exprs (second (Expr-args body))]
           [letrec-expr (first body-exprs)])
      
      ;; Body should contain a letrec expression (application)
      (set! state (assert (Expr-head letrec-expr) 'AppHead "Let body contains letrec"))
      
      ;; Get the body of the letrec
      (let* ([letrec-lambda (first (Expr-args letrec-expr))]
             [letrec-body (second (Expr-args letrec-lambda))]
             [letrec-exprs (second (Expr-args letrec-body))]
             [begin-expr (first letrec-exprs)]
             [begin-exprs (Expr-args begin-expr)]
             [if-expr (car (reverse begin-exprs))])
        
        ;; Last expression in begin should be if from cond
        (set! state (assert (Expr-head if-expr) 'IfHead "Letrec body contains if from cond"))
        
        ;; First condition checks a < 0
        (let* ([if-args (Expr-args if-expr)]
               [condition (first if-args)]
               [nested-if (third if-args)])
          
          ;; Second condition checks and(> a 0, < a 10)
          (set! state (assert (Expr-head nested-if) 'IfHead "Nested if for second condition"))
          
          ;; The nested condition should be another if from 'and'
          (let* ([nested-if-args (Expr-args nested-if)]
                 [and-condition (first nested-if-args)]
                 [fact-call (second nested-if-args)]
                 [else-branch (third nested-if-args)])
            
            ;; Verify that the second branch is calling fact
            (set! state (assert (Expr-head fact-call) 'AppHead "Second branch calls fact"))
            (let ([func (first (Expr-args fact-call))])
              (set! state (assert (expr-struct-equal? func (make-expr 'IdHead (list "fact") dummy-loc))
                                "Function called is fact")))
            
            ;; Verify the final else branch
            (set! state (assert (expr-struct-equal? else-branch
                                                 (make-expr 'ConstHead 
                                                          (list 'StringConstNode "too large")
                                                          dummy-loc))
                              "Final else branch is \"too large\"")))))))
  
  state)

;; run-desugar-tests
;;     Runs all desugaring-related tests
(define (run-desugar-tests state output-fn)
  (output-fn "Running desugar tests...")
  (let ([out (make-indented-output-fn output-fn 1)])
    (for/fold ([s state])
              ([f (list 
                    test-and-desugaring
                    test-or-desugaring
                    test-cond-desugaring
                    test-unnamed-let-desugaring
                    test-letstar-desugaring
                    test-letrec-desugaring
                    test-named-let-desugaring
                    test-complex-desugaring)])
      (with-error-handling (lambda () (f s out))
        (symbol->string (object-name f)) s out))))