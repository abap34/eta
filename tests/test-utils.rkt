#lang racket

(provide assert-equal
         report-tests
         reset-tests
         make-test-state
         update-test-state
         make-indented-output-fn
         default-output-fn
         with-error-handling
         expr-struct-equal?
         expr-struct-diff
         assert-expr-struct-equal)

(require "../eta/utils/console.rkt"
         "../eta/parser/ast.rkt"
         racket/trace)

;  make-test-state
;     Creates a new test state.
;  Arguments:
;      None
;  Returns:
;      A list of (test-count . fail-count . error-count).
(define (make-test-state)
  (list 0 0 0))

;  update-test-state
;     Updates the test state based on test results.
;  Arguments:
;      state - The current test state.
;      passed - Boolean indicating if the test passed.
;  Returns:
;      The updated test state.
(define (update-test-state state passed)
  (if passed
      (list (+ (car state) 1) (cadr state) (caddr state))
      (list (+ (car state) 1) (+ (cadr state) 1) (caddr state))))

;  update-test-state-with-error
;     Updates the test state when an error occurs.
;  Arguments:
;      state - The current test state.
;  Returns:
;      The updated test state with incremented error count.
(define (update-test-state-with-error state)
  (list (+ (car state) 1) (cadr state) (+ (caddr state) 1)))

;  reset-tests
;     Resets the test state to its initial values.
;  Arguments:
;      None
;  Returns:
;      A new test state.
(define (reset-tests)
  (make-test-state))

;  report-tests
;     Reports the test results based on the given state.
;  Arguments:
;      state - The current test state.
;  Returns:
;      Never returns.
(define (report-tests state)
  (let ([total (car state)]
        [fails (cadr state)]
        [errors (caddr state)])
    (display (string-append "Total tests: " (number->string total) "\n"))

    (if (= fails 0)
        (display (colorize "Passed tests: All tests passed!\n" 'green))
        (display (string-append "Failed tests: " (number->string fails) "\n")))

    (if (> errors 0)
        (display (string-append "Error tests: " (number->string errors) "\n"))
        (void))

    (if (and (= fails 0) (= errors 0))
        (begin
          (display (colorize "All tests passed successfully!\n" 'green))
          (exit 0))
        (begin
          (display (colorize "Some tests failed or errored!\n" 'red))
          (exit 1)))))

;  assert-equal
;     Asserts that two values are equal and updates the test state.
;  Arguments:
;      actual - The actual value.
;      expected - The expected value.
;      msg - A message describing the test.
;      state - The current test state.
;  Returns:
;      The updated test state.
(define (assert-equal actual expected msg state output-fn #:cmp [cmp equal?])
  (let ([passed (cmp actual expected)])
    (if passed
        (output-fn (colorize (string-append "✓ " msg) 'green))
        (begin
          (output-fn (colorize (string-append "✗ " msg) 'red))
          (output-fn (string-append "    expected: " (format "~s" expected)))
          (output-fn (string-append "      actual: " (format "~s" actual)))))
    (update-test-state state passed)))


;  with-error-handling
;     Evaluates a thunk with error handling and optional stack trace.
;  Arguments:
;      thunk - The thunk to evaluate.
;      msg - A message describing the test.
;      state - The current test state.
;      output-fn - Function to display output.
;      [show-stack-trace] - Boolean indicating whether to show stack trace (default #t)
;  Returns:
;      The updated test state.
;  Example:
;      (with-error-handling
;        (lambda () (test-function arg1 arg2))
;        "Test function X"
;        state
;        output-fn)
(define (with-error-handling thunk msg state output-fn #:show-stack-trace [show-stack-trace #t])
  (with-handlers
      ([exn:fail? (lambda (e)
                    (output-fn (colorize (string-append "! Error in " msg ": " (exn-message e)) 'red))
                    (when show-stack-trace
                      (output-fn (colorize "Stack trace:" 'red))
                      (for-each (lambda (frame)
                                  (output-fn (colorize (format "  ~a" frame) 'red)))
                                (get-stack-trace e)))
                    (update-test-state-with-error state))])
    (thunk)))

;  get-stack-trace
;     Extracts a readable stack trace from an exception.
;  Arguments:
;      exn - The exception from which to extract the stack trace
;  Returns:
;      A list of strings representing the call stack
;  Notes:
;      Formats each stack frame in a more readable way
(define (get-stack-trace exn)
  (if (exn:fail? exn)
      (let ([ct (continuation-mark-set->context (exn-continuation-marks exn))])
        (map (lambda (frame)
               (format-stack-frame frame))
             ct))
      '()))

;  format-stack-frame
;     Formats a stack frame into a readable string.
;  Arguments:
;      frame - The stack frame to format (list or symbol)
;  Returns:
;      A formatted string representing the stack frame
(define (format-stack-frame frame)
  (cond
    [(list? frame)
     (let ([name (first frame)]
           [source (second frame)]
           [line (third frame)]
           [col (fourth frame)])
       (format "at ~a (~a:~a:~a)" name source line col))]
    [(symbol? frame)
     (format "at ~a" frame)]
    [else (format "~a" frame)]))

;  make-indented-output-fn
;     Creates an output function that adds indentation based on depth.
;  Arguments:
;      base-output-fn - The base output function to wrap.
;      depth - The initial depth (integer).
;  Returns:
;      A new output function that adds indentation.
;  Example:
;      (define output-fn (make-indented-output-fn default-output-fn 0))
;      (output-fn "Hello") ; Output: "Hello"
;      (let ((child-output-fn (make-indented-output-fn output-fn 1)))
;        (child-output-fn "World")) ; Output: "  World"
(define (make-indented-output-fn base-output-fn depth)
  (lambda (msg)
    (let ([indent (make-string (* depth 2) #\space)]) ; 2 spaces per depth level
      (base-output-fn (string-append indent msg)))))

;  default-output-fn
;     Default function for handling test output.
;  Arguments:
;      msg - The message to display.
;  Returns:
;      None.
;  Example:
;      (default-output-fn "Hello") ; Output: "Hello"
(define (default-output-fn msg)
  (display (string-append "│ " msg "\n")))

;  expr-struct-equal?
;     Compares two Expr structures for equality, ignoring location information.
;  Arguments:
;      expr1 - First expression to compare
;      expr2 - Second expression to compare
;  Returns:
;      #t if the expressions are structurally equal (ignoring location), #f otherwise
;  Example:
;      (expr-struct-equal? (make-expr 'ConstHead (list 'IntConstNode 42) loc1)
;                         (make-expr 'ConstHead (list 'IntConstNode 42) loc2)) ; => #t
(define (expr-struct-equal? expr1 expr2)
  (null? (expr-struct-diff expr1 expr2)))

;  expr-struct-diff
;     Finds differences between two Expr structures, ignoring location information.
;  Arguments:
;      expr1 - First expression to compare
;      expr2 - Second expression to compare
;  Returns:
;      An empty list if expressions are equal, otherwise a list of differences where each
;      difference is a list containing the path to the difference and the values found
;  Example:
;      (expr-struct-diff (make-expr 'ConstHead (list 'IntConstNode 42) loc1)
;                       (make-expr 'ConstHead (list 'IntConstNode 43) loc2))
;      ; => (((args 1) 42 43))
(define (expr-struct-diff expr1 expr2)
  (expr-struct-diff-impl expr1 expr2 '()))

;  expr-struct-diff-impl
;     Implementation of expr-struct-diff with path tracking.
;  Arguments:
;      expr1 - First expression to compare
;      expr2 - Second expression to compare
;      path - Current path in the structure
;  Returns:
;      List of differences with their paths and values
(define (expr-struct-diff-impl expr1 expr2 path)
  (cond
    ; Both are Expr structures
    [(and (Expr? expr1) (Expr? expr2))
     (if (equal? (Expr-head expr1) (Expr-head expr2))
         (expr-args-diff (Expr-args expr1) (Expr-args expr2) (append path (list 'args)))
         (list (list path (Expr-head expr1) (Expr-head expr2))))]
    
    ; Both are lists
    [(and (list? expr1) (list? expr2))
     (if (= (length expr1) (length expr2))
         (apply append
                (for/list ([e1 expr1] [e2 expr2] [i (in-naturals)])
                  (expr-struct-diff-impl e1 e2 (append path (list i)))))
         (list (list path `(list of length ,(length expr1)) `(list of length ,(length expr2)))))]
    
    ; Other values (numbers, strings, booleans, symbols)
    [(equal? expr1 expr2) '()]
    [else (list (list path expr1 expr2))]))

;  expr-args-diff
;     Helper function to compare the args field of two expressions.
;  Arguments:
;      args1 - First args list to compare
;      args2 - Second args list to compare
;      path - Current path in the structure
;  Returns:
;      List of differences with their paths and values
(define (expr-args-diff args1 args2 path)
  (cond
    ; Both are lists of the same length
    [(and (list? args1) (list? args2) (= (length args1) (length args2)))
     (apply append
            (for/list ([a1 args1] [a2 args2] [i (in-naturals)])
              (expr-struct-diff-impl a1 a2 (append path (list i)))))]
    
    ; Both are non-list values
    [(and (not (list? args1)) (not (list? args2)))
     (if (equal? args1 args2) '() (list (list path args1 args2)))]
    
    ; One is a list and the other is not
    [else (list (list path args1 args2))]))

;  format-expr-diff
;     Formats an expression difference for human-readable display.
;  Arguments:
;      diff - A single difference entry (path and values)
;      indent - Indentation level for formatting
;  Returns:
;      A formatted string describing the difference
(define (format-expr-diff diff indent)
  (let ([path (first diff)]
        [expr1 (second diff)]
        [expr2 (third diff)])
    (string-append 
     (format "Path: ~a\n" (path->string path))
     indent (format "Expected: ~a\n" 
                   (if (Expr? expr1) 
                       (pretty-print-Expr expr1) 
                       expr1))
     indent (format "Actual: ~a" 
                   (if (Expr? expr2) 
                       (pretty-print-Expr expr2) 
                       expr2)))))

;  path->string
;     Converts a path list to a dot-notation string.
;  Arguments:
;      path - A list representing a path in an expression structure
;  Returns:
;      A string representation of the path
(define (path->string path)
  (if (null? path)
      "root"
      (string-join (map ~a path) ".")))

;  assert-true
;     Asserts that a condition is true and provides an error message if not.
;  Arguments:
;      condition - The condition to check
;      message - Test description message
;      error-fn - Function to generate error message details
;      state - Current test state
;      output-fn - Function to display output
;  Returns:
;      Updated test state
(define (assert-true condition message error-fn state output-fn)
  (let ([passed condition])
    (if passed
        (output-fn (colorize (string-append "✓ " message) 'green))
        (begin
          (output-fn (colorize (string-append "✗ " message) 'red))
          (let ([error-details (error-fn)])
            (when error-details
              (for-each output-fn 
                        (map (lambda (line) 
                               (colorize line 'red))
                             (string-split error-details "\n")))))))
    (update-test-state state passed)))

;  assert-expr-struct-equal
;     Asserts that two expressions are structurally equal (ignoring location).
;     If they differ, provides detailed information about where they differ.
;  Arguments:
;      actual - The actual expression value
;      expected - The expected expression value
;      message - Test description message
;      state - Current test state
;      output-fn - Function for test output
;  Returns:
;      Updated test state
(define (assert-expr-struct-equal actual expected message state output-fn)
  (let ([diffs (expr-struct-diff actual expected)])
    (assert-true (null? diffs)
                 message
                 (lambda () 
                   (when (not (null? diffs))
                     (string-append
                      "Found differences:"
                      (string-join
                       (map (lambda (diff) 
                              (string-append 
                               "\n" 
                               (format-expr-diff diff "  ")))
                            diffs)
                       ""))))
                 state
                 output-fn)))
