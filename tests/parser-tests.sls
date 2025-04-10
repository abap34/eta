(library (tests parser-tests)
  (export run-parser-tests)
  (import (rnrs) (tests test-utils) (eta parser ast) (eta parser parser))
  (define (test-const state output-fn)
    (output-fn "Running test-const...")

    (let*
      ((input "#t") (parsed (parse input)) (expected (make-Expr 'const (list #t))))
      (set! state (assert-equal parsed expected (string-append "Const test for " input)
       state (make-indented-output-fn output-fn 1)))
    )

    (let*
      ((input "#f") (parsed (parse input)) (expected (make-Expr 'const (list #f))))
      (set! state (assert-equal parsed expected (string-append "Const test for " input)
       state (make-indented-output-fn output-fn 1)))
    )

    (let*
      ((input "123") (parsed (parse input)) (expected (make-Expr 'const (list 123))))
      (set! state (assert-equal parsed expected (string-append "Const test for " input)
       state (make-indented-output-fn output-fn 1)))
    )

    (let*
      ((input "\"hello\"") (parsed (parse input)) (expected (make-Expr 'const (list
       "hello"))))
      (set! state (assert-equal parsed expected (string-append "Const test for " input)
       state (make-indented-output-fn output-fn 1)))
    )

    (let*
      ((input "()") (parsed (parse input)) (expected (make-Expr 'const '(()))))
      (set! state (assert-equal parsed expected (string-append "Const test for " input)
       state (make-indented-output-fn output-fn 1)))
    )
    (let*
      ((input "'x") (parsed (parse input)) (expected (make-Expr 'quote (list (make-Expr
       'id (list 'x))))))
      (set! state (assert-equal parsed expected (string-append "Const test for " input
       " => 'x") state (make-indented-output-fn output-fn 1)))
    )

    state
  )

  (define (test-id state output-fn)
    (output-fn "Running test-id...")

    (let*
      ((input "x") (parsed (parse input)) (expected (make-Expr 'id (list 'x))))
      (set! state (assert-equal parsed expected (string-append "Id test for " input)
       state (make-indented-output-fn output-fn 1)))
    )

    (let*
      ((input "hello-world!") (parsed (parse input)) (expected (make-Expr 'id (list
       'hello-world!))))
      (set! state (assert-equal parsed expected (string-append "Id test for " input)
       state (make-indented-output-fn output-fn 1)))
    )

    (let*
      ((input "x2") (parsed (parse input)) (expected (make-Expr 'id (list 'x2))))
      (set! state (assert-equal parsed expected (string-append "Id test for " input)
       state (make-indented-output-fn output-fn 1)))
    )

    state
  )

  (define (test-lambda state output-fn)
    (output-fn "Running test-lambda...")

    (let*
      ((input "(lambda (x) x)")
        (parsed (parse input))
        (expected (make-Expr 'lambda (list (make-Expr 'args (list 'x)) (make-Expr
         'id (list 'x)))))
      )
      (set! state (assert-equal parsed expected (string-append "Lambda test for "
       input) state (make-indented-output-fn output-fn 1)))
    )

    (let*
      ((input "(lambda (x y) (+ x y))")
        (parsed (parse input))
        (expected
          (make-Expr
            'lambda
            (list
              (make-Expr 'args (list 'x 'y))
              (make-Expr 'application (list (make-Expr 'id (list '+)) (make-Expr 'id
               (list 'x)) (make-Expr 'id (list 'y))))
            )
          )
        )
      )
      (set! state (assert-equal parsed expected (string-append "Lambda test for "
       input) state (make-indented-output-fn output-fn 1)))
    )

    state
  )

  (define (test-application state output-fn)
    (output-fn "Running test-application...")

    (let*
      ((input "(f x)")
        (parsed (parse input))
        (expected (make-Expr 'application (list (make-Expr 'id (list 'f)) (make-Expr
         'id (list 'x)))))
      )
      (set! state (assert-equal parsed expected (string-append "Application test for "
       input) state (make-indented-output-fn output-fn 1)))
    )

    (let*
      ((input "(f x y)")
        (parsed (parse input))
        (expected (make-Expr 'application (list (make-Expr 'id (list 'f)) (make-Expr
         'id (list 'x)) (make-Expr 'id (list 'y)))))
      )
      (set! state (assert-equal parsed expected (string-append "Application test for "
       input) state (make-indented-output-fn output-fn 1)))
    )

    (let*
      ((input "((lambda (x) x) 42)")
        (parsed (parse input))
        (expected
          (make-Expr
            'application
            (list (make-Expr 'lambda (list (make-Expr 'args (list 'x)) (make-Expr
             'id (list 'x)))) (make-Expr 'const (list 42)))
          )
        )
      )
      (set! state (assert-equal parsed expected (string-append "Application test for "
       input) state (make-indented-output-fn output-fn 1)))
    )

    (output-fn "✓ Mock: test-application executed.")
    state
  )

  (define (test-define state output-fn)
    (output-fn "Running test-define...")

    (let*
      ((input "(define x 42)")
        (parsed (parse input))
        (expected (make-Expr 'define (list (make-Expr 'id (list 'x)) (make-Expr 'const
         (list 42)))))
      )
      (set! state (assert-equal parsed expected (string-append "Define test for "
       input) state (make-indented-output-fn output-fn 1)))
    )

    (let*
      ((input "(define (f x) (+ x 1))")
        (parsed (parse input))
        (expected
          (make-Expr
            'define
            (list
              (make-Expr 'id (list 'f))
              (make-Expr 'args (list 'x))
              (make-Expr 'application (list (make-Expr 'id (list '+)) (make-Expr 'id
               (list 'x)) (make-Expr 'const (list 1))))
            )
          )
        )
      )
      (set! state (assert-equal parsed expected (string-append "Define test for function form "
       input) state (make-indented-output-fn output-fn 1)))
    )

    state
  )

  (define (run-parser-tests state output-fn)
    (output-fn "Running parser tests...")
    (let ((child-output-fn (make-indented-output-fn output-fn 1)))
      (let ((state (test-const state child-output-fn)))
        (let ((state (test-id state child-output-fn)))
          (let ((state (test-lambda state child-output-fn)))
            (let ((state (test-application state child-output-fn)))
              (test-define state child-output-fn)
            )
          )
        )
      )
    )
  )
)
