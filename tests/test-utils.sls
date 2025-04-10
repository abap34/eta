(library (tests test-utils)
  (export assert-equal report-tests reset-tests make-test-state update-test-state)
  (import (rnrs))

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
          (display "All tests passed!\n")
          (exit 0)
        )
        (begin
          (display (string-append "Total tests: " (number->string total) "\n"))
          (display (string-append "Failed tests: " (number->string fails) "\n"))
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
  (define (assert-equal actual expected msg state)
    (let ((passed (equal? actual expected)))
      (if passed
        (display (string-append "✓ " msg "\n"))
        (display (string-append "✗ " msg "\n"))
      )
      (update-test-state state passed)
    )
  )
)
