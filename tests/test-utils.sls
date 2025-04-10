(library (tests test-utils)
  (export
    assert-equal
    report-tests
    reset-tests
    make-test-state
    update-test-state
    make-indented-output-fn
    default-output-fn
  )
  (import (rnrs) (eta utils console))

  ;  make-test-state
  ;     Creates a new test state.
  ;  Arguments:
  ;      None
  ;  Returns:
  ;      A pair (test-count . fail-count).
  (define (make-test-state)
    (cons 0 0)
  )

  ;  update-test-state
  ;     Updates the test state based on test results.
  ;  Arguments:
  ;      state - The current test state.
  ;      passed - Boolean indicating if the test passed.
  ;  Returns:
  ;      The updated test state.
  (define (update-test-state state passed)
    (if passed
      (cons (+ (car state) 1) (cdr state))
      (cons (+ (car state) 1) (+ (cdr state) 1))
    )
  )

  ;  reset-tests
  ;     Resets the test state to its initial values.
  ;  Arguments:
  ;      None
  ;  Returns:
  ;      A new test state.
  (define (reset-tests)
    (make-test-state)
  )

  ;  report-tests
  ;     Reports the test results based on the given state.
  ;  Arguments:
  ;      state - The current test state.
  ;  Returns:
  ;      Never returns.
  (define (report-tests state)
    (let ((total (car state)) (fails (cdr state)))
      (if (= fails 0)
        (begin
          (display (string-append "Total tests: " (number->string total) "\n"))
          (display (colorize "All tests passed!\n" 'green))
          (exit 0)
        )
        (begin
          (display (string-append "Total tests: " (number->string total) "\n"))
          (display (string-append "Failed tests: " (number->string fails) "\n"))
          (display (colorize "Some tests failed!\n" 'red))
          (exit 1)
        )
      )
    )
  )

  ;  assert-equal
  ;     Asserts that two values are equal and updates the test state.
  ;  Arguments:
  ;      actual - The actual value.
  ;      expected - The expected value.
  ;      msg - A message describing the test.
  ;      state - The current test state.
  ;  Returns:
  ;      The updated test state.
  (define (assert-equal actual expected msg state output-fn)
    (let ((passed (equal? actual expected)))
      (if passed
        (output-fn (string-append "✓ " msg))
        (output-fn (string-append "✗ " msg))
      )
      (update-test-state state passed)
    )
  )

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
      (let ((indent (make-string (* depth 2) #\space))) ; 2 spaces per depth level
        (base-output-fn (string-append indent msg))
      )
    )
  )

  ;  default-output-fn
  ;     Default function for handling test output.
  ;  Arguments:
  ;      msg - The message to display.
  ;  Returns:
  ;      None.
  ;  Example:
  ;      (default-output-fn "Hello") ; Output: "Hello"
  (define (default-output-fn msg)
    (display (string-append "│ " msg "\n"))
  )
)
