#lang racket

(provide assert-equal
         report-tests
         reset-tests
         make-test-state
         update-test-state
         make-indented-output-fn
         default-output-fn
         with-error-handling)

(require "../eta/utils/console.rkt")

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
;     Evaluates a thunk with error handling.
;  Arguments:
;      thunk - The thunk to evaluate.
;      msg - A message describing the test.
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      The updated test state.
;  Example:
;      (with-error-handling
;        (lambda () (test-function arg1 arg2))
;        "Test function X"
;        state
;        output-fn)
(define (with-error-handling thunk msg state output-fn)
  (with-handlers
      ([exn:fail? (lambda (e)
                    (output-fn (colorize (string-append "! Error in " msg ": " (exn-message e)) 'red))
                    (update-test-state-with-error state))])
    (thunk)))

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
