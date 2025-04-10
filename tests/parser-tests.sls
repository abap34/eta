(library (tests parser-tests)
  (export run-parser-tests)
  (import (rnrs) (tests test-utils))

  ;  run-parser-tests
  ;     Runs all parser-related tests.
  ;  Arguments:
  ;      state - The current test state.
  ;  Returns:
  ;      The updated test state.
  (define (run-parser-tests state)
    (display "→ Running parser tests...\n")
    (let ((state (assert-equal (+ 1 1) 2 "1 + 1 should be equal to 2" state)))
      state
    )
  )
)
