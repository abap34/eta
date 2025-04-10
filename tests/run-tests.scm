(import (tests test-utils) (tests parser-tests) (eta utils console))

(define (run-tests)
  (display (bold (colorize "Running tests...\n" 'blue)))
  (let ((state (make-test-state)))
    (let ((state (run-parser-tests state default-output-fn))) ; Pass default-output-fn
      (report-tests state)
    )
  )
)

(define (main)
  (run-tests)
)

(main)
