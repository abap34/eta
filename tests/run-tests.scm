(import (rnrs) (tests parser-tests) (tests test-utils))

(display "=== Running eta test suite ===\n")

(let ((state (reset-tests)))
  (let ((state (run-parser-tests state)))
    (report-tests state)
  )
)
