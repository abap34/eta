#lang racket

(require "test-utils.rkt"
         "parser-tests.rkt"
         "tokenizer-tests.rkt"
         "../eta/utils/console.rkt")

;  available-tests
;     Gets a list of available tests and their functions.
;  Arguments:
;      None
;  Returns:
;      A list of available tests and their functions.
(define available-tests
  '(("tokenizer" . run-tokenizer-tests)
    ("parser" . run-parser-tests)))

;  print-help
;     Prints the help message with available tests.
;  Arguments:
;      None
;  Returns:
;      None
(define (print-help)
  (display "Usage: racket run-tests.scm [test-name ...]\n")
  (display "Available tests:\n")
  (for-each
   (lambda (test)
     (display (string-append "  - " (car test) "\n")))
   available-tests)
  (display "Run without arguments to execute all tests.\n"))

;  run-specific-test
;     
;  Arguments:
;      test-name - Name of the test to run
;      state - Current test state
;  Returns:
;      The updated test state
(define (run-specific-test test-name state)
  (let ([test-pair (assoc test-name available-tests)])
    (if test-pair
        (with-error-handling
         (lambda ()
           ((cdr test-pair) state default-output-fn))
         (string-append test-name " tests")
         state
         default-output-fn)
        (begin
          (display (colorize (string-append "Unknown test: "
                                            test-name
                                            "\n")
                             'red))
          state))))

;  run-tests
;     Runs the specified tests or all tests if no arguments are provided.
;  Arguments:
;      test-names - List of test names to run
;  Returns:
;      Never returns (exits with status code 0 or 1)
(define (run-tests [test-names '()])
  (display (bold (colorize "Running tests...\n" 'blue)))

  (let ([state (make-test-state)])
    (let ([final-state
           (if (null? test-names)
               (let ([state (with-error-handling
                             (lambda ()
                               (run-tokenizer-tests
                                state
                                default-output-fn))
                             "tokenizer tests"
                             state
                             default-output-fn)])
                 (with-error-handling
                  (lambda ()
                    (run-parser-tests state
                                      default-output-fn))
                  "parser tests"
                  state
                  default-output-fn))
               (foldl (lambda (test-name state)
                        (run-specific-test test-name state))
                      state
                      test-names))])

      (let ([total (car final-state)]
            [fails (cadr final-state)]
            [errors (caddr final-state)])
        (display "\n")
        (display (bold (colorize "Test Summary:\n" 'blue)))
        (display (string-append "Total tests: "
                                (number->string total)
                                "\n"))

        (if (= fails 0)
            (display (colorize (string-append
                                "Passed tests: "
                                (number->string
                                 (- total fails errors))
                                "\n")
                               'green))
            (display (colorize
                      (string-append "Failed tests: "
                                     (number->string fails)
                                     "\n")
                      'red)))

        (when (> errors 0)
          (display (colorize
                    (string-append "Error tests: "
                                   (number->string errors)
                                   "\n")
                    'red)))

        (if (and (= fails 0) (= errors 0))
            (begin
              (display (colorize
                        "\nAll tests passed successfully!\n"
                        'green))
              (exit 0))
            (begin
              (display (colorize
                        "\nSome tests failed or errored!\n"
                        'red))
              (exit 1)))))))

(define (main)
  (let ([args (vector->list
               (current-command-line-arguments))])
    (cond
      [(member "-h" args) (print-help)]
      [(member "--help" args) (print-help)]
      [else (run-tests args)])))

(main)
