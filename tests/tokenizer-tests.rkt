#lang racket

(provide run-tokenizer-tests)
(require "test-utils.rkt"
         "../eta/parser/tokenizer.rkt"
         "../eta/utils/location.rkt"
         "../eta/utils/error.rkt")

;  remove-eof
;     Removes the EOF token from the end of a token list.
;  Arguments:
;     tokens - A list of tokens possibly ending with an EOF token
;  Returns:
;     The token list with the EOF token removed
;  Notes:
;     Verifies that the last token is an EOF token before removing it
(define (remove-eof tokens)
  (if (and (list? tokens)
           (not (empty? tokens))
           (eq? (Token-typ (last tokens)) EOF))
      (take tokens (sub1 (length tokens)))
      (error "Expected EOF token at end of token list")))

;  tokenize-without-eof
;     Tokenizes input and removes the trailing EOF token
;  Arguments:
;     input - The input string to tokenize
;  Returns:
;     A list of tokens with the EOF token removed
(define (tokenize-without-eof input)
  (remove-eof (tokenize input)))

;  test-basic-tokens
;     Tests basic token types like parentheses, dot, and quote.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-basic-tokens state output-fn)
  (output-fn "Running test-basic-tokens...")

  (let* ([input "("]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token LParen "(" (Location 1 1 1 2)))])
    (set! state (assert-equal tokens expected
                              "LParen token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input ")"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token RParen ")" (Location 1 1 1 2)))])
    (set! state (assert-equal tokens expected
                              "RParen token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "."]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token DotSym "." (Location 1 1 1 2)))])
    (set! state (assert-equal tokens expected
                              "DotSym token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "'"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token QuoteSym "'" (Location 1 1 1 2)))])
    (set! state (assert-equal tokens expected
                              "Quote token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "()"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token LParen "(" (Location 1 1 1 2))
                    (Token RParen ")" (Location 1 2 1 3)))])
    (set! state (assert-equal tokens expected
                              "Multiple token test for parentheses"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(a . b)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token LParen "(" (Location 1 1 1 2))
                    (Token Id "a" (Location 1 2 1 3))
                    (Token DotSym "." (Location 1 4 1 5))
                    (Token Id "b" (Location 1 6 1 7))
                    (Token RParen ")" (Location 1 7 1 8)))])
    (set! state (assert-equal tokens expected
                              "DotSymted pair token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  state)

;  test-boolean-tokens
;     Tests boolean token recognition.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-boolean-tokens state output-fn)
  (output-fn "Running test-boolean-tokens...")

  (let* ([input "#t"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token Bool "#t" (Location 1 1 1 3)))])
    (set! state (assert-equal tokens expected
                              "True boolean token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "#f"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token Bool "#f" (Location 1 1 1 3)))])
    (set! state (assert-equal tokens expected
                              "False boolean token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(if #t 1 #f)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token LParen "(" (Location 1 1 1 2))
                    (Token Id "if" (Location 1 2 1 4))
                    (Token Bool "#t" (Location 1 5 1 7))
                    (Token Num "1" (Location 1 8 1 9))
                    (Token Bool "#f" (Location 1 10 1 12))
                    (Token RParen ")" (Location 1 12 1 13)))])
    (set! state (assert-equal tokens expected
                              "Boolean in expression test"
                              state
                              (make-indented-output-fn output-fn 1))))

  state)

;  test-number-tokens
;     Tests numeric token recognition.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-number-tokens state output-fn)
  (output-fn "Running test-number-tokens...")

  (let* ([input "123"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token Num "123" (Location 1 1 1 4)))])
    (set! state (assert-equal tokens expected
                              "Simple number token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "123 456"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token Num "123" (Location 1 1 1 4))
                    (Token Num "456" (Location 1 5 1 8)))])
    (set! state (assert-equal tokens expected
                              "Multiple number token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(+ 1 2)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token LParen "(" (Location 1 1 1 2))
                    (Token Id "+" (Location 1 2 1 3))
                    (Token Num "1" (Location 1 4 1 5))
                    (Token Num "2" (Location 1 6 1 7))
                    (Token RParen ")" (Location 1 7 1 8)))])
    (set! state (assert-equal tokens expected
                              "Numbers in expression test"
                              state
                              (make-indented-output-fn output-fn 1))))

  state)

;  test-string-tokens
;     Tests string token recognition including escape sequences.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-string-tokens state output-fn)
  (output-fn "Running test-string-tokens...")

  (let* ([input "\"hello\""]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token String "hello" (Location 1 1 1 8)))])
    (set! state (assert-equal tokens expected
                              "Simple string token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "\"hello\\\"world\""]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token String "hello\"world" (Location 1 1 1 15)))])
    (set! state (assert-equal tokens expected
                              "String with escaped quote test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "\"line1\\nline2\\tindent\""]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token String (string-append "line1" (string #\newline) "line2" (string #\tab) "indent")
                                (Location 1 1 1 23)))])
    (set! state (assert-equal tokens expected
                              "String with special escapes test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(display \"hello\")"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token LParen "(" (Location 1 1 1 2))
                    (Token Id "display" (Location 1 2 1 9))
                    (Token String "hello" (Location 1 10 1 17))
                    (Token RParen ")" (Location 1 17 1 18)))])
    (set! state (assert-equal tokens expected
                              "String in expression test"
                              state
                              (make-indented-output-fn output-fn 1))))

  state)

;  test-keyword-tokens
;     Tests identifiers that were previously treated as keywords.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-keyword-tokens state output-fn)
  (output-fn "Running test-keyword-tokens...")

  (let* ([input "define"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token Id "define" (Location 1 1 1 7)))])
    (set! state (assert-equal tokens expected
                              "define identifier test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "lambda"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token Id "lambda" (Location 1 1 1 7)))])
    (set! state (assert-equal tokens expected
                              "lambda identifier test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "let let* letrec if cond quote set! and or begin do load else"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token Id "let" (Location 1 1 1 4))
                    (Token Id "let*" (Location 1 5 1 9))
                    (Token Id "letrec" (Location 1 10 1 16))
                    (Token Id "if" (Location 1 17 1 19))
                    (Token Id "cond" (Location 1 20 1 24))
                    (Token Id "quote" (Location 1 25 1 30))
                    (Token Id "set!" (Location 1 31 1 35))
                    (Token Id "and" (Location 1 36 1 39))
                    (Token Id "or" (Location 1 40 1 42))
                    (Token Id "begin" (Location 1 43 1 48))
                    (Token Id "do" (Location 1 49 1 51))
                    (Token Id "load" (Location 1 52 1 56))
                    (Token Id "else" (Location 1 57 1 61)))])
    (set! state (assert-equal tokens expected
                              "All keywords now as identifiers test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(define x 10)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token LParen "(" (Location 1 1 1 2))
                    (Token Id "define" (Location 1 2 1 8))
                    (Token Id "x" (Location 1 9 1 10))
                    (Token Num "10" (Location 1 11 1 13))
                    (Token RParen ")" (Location 1 13 1 14)))])
    (set! state (assert-equal tokens expected
                              "Special form in expression test"
                              state
                              (make-indented-output-fn output-fn 1))))

  state)

;  test-identifier-tokens
;     Tests identifier token recognition.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-identifier-tokens state output-fn)
  (output-fn "Running test-identifier-tokens...")

  (let* ([input "x"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token Id "x" (Location 1 1 1 2)))])
    (set! state (assert-equal tokens expected
                              "Simple identifier test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "hello-world!"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token Id "hello-world!" (Location 1 1 1 13)))])
    (set! state (assert-equal tokens expected
                              "Identifier with special chars test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "x2 y3z"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token Id "x2" (Location 1 1 1 3))
                    (Token Id "y3z" (Location 1 4 1 7)))])
    (set! state (assert-equal tokens expected
                              "Identifiers with numbers test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "+ - * / < = > ! $ % & = @ ^ _"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token Id "+" (Location 1 1 1 2))
                    (Token Id "-" (Location 1 3 1 4))
                    (Token Id "*" (Location 1 5 1 6))
                    (Token Id "/" (Location 1 7 1 8))
                    (Token Id "<" (Location 1 9 1 10))
                    (Token Id "=" (Location 1 11 1 12))
                    (Token Id ">" (Location 1 13 1 14))
                    (Token Id "!" (Location 1 15 1 16))
                    (Token Id "$" (Location 1 17 1 18))
                    (Token Id "%" (Location 1 19 1 20))
                    (Token Id "&" (Location 1 21 1 22))
                    (Token Id "=" (Location 1 23 1 24))
                    (Token Id "@" (Location 1 25 1 26))
                    (Token Id "^" (Location 1 27 1 28))
                    (Token Id "_" (Location 1 29 1 30)))])
    (set! state (assert-equal tokens expected
                              "Symbolic identifiers test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "map filter fold-left fold-right"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token Id "map" (Location 1 1 1 4))
                    (Token Id "filter" (Location 1 5 1 11))
                    (Token Id "fold-left" (Location 1 12 1 21))
                    (Token Id "fold-right" (Location 1 22 1 32)))])
    (set! state (assert-equal tokens expected
                              "Complex identifiers test"
                              state
                              (make-indented-output-fn output-fn 1))))

  state)

;  test-complex-expressions
;     Tests tokenization of complex Scheme expressions.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-complex-expressions state output-fn)
  (output-fn "Running test-complex-expressions...")

  (let* ([input "(lambda (x) (+ x 1))"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token LParen "(" (Location 1 1 1 2))
                    (Token Id "lambda" (Location 1 2 1 8))
                    (Token LParen "(" (Location 1 9 1 10))
                    (Token Id "x" (Location 1 10 1 11))
                    (Token RParen ")" (Location 1 11 1 12))
                    (Token LParen "(" (Location 1 13 1 14))
                    (Token Id "+" (Location 1 14 1 15))
                    (Token Id "x" (Location 1 16 1 17))
                    (Token Num "1" (Location 1 18 1 19))
                    (Token RParen ")" (Location 1 19 1 20))
                    (Token RParen ")" (Location 1 20 1 21)))])
    (set! state (assert-equal tokens expected
                              "Lambda expression test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(let ((x 1) (y 2)) (+ x y))"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token LParen "(" (Location 1 1 1 2))
                    (Token Id "let" (Location 1 2 1 5))
                    (Token LParen "(" (Location 1 6 1 7))
                    (Token LParen "(" (Location 1 7 1 8))
                    (Token Id "x" (Location 1 8 1 9))
                    (Token Num "1" (Location 1 10 1 11))
                    (Token RParen ")" (Location 1 11 1 12))
                    (Token LParen "(" (Location 1 13 1 14))
                    (Token Id "y" (Location 1 14 1 15))
                    (Token Num "2" (Location 1 16 1 17))
                    (Token RParen ")" (Location 1 17 1 18))
                    (Token RParen ")" (Location 1 18 1 19))
                    (Token LParen "(" (Location 1 20 1 21))
                    (Token Id "+" (Location 1 21 1 22))
                    (Token Id "x" (Location 1 23 1 24))
                    (Token Id "y" (Location 1 25 1 26))
                    (Token RParen ")" (Location 1 26 1 27))
                    (Token RParen ")" (Location 1 27 1 28)))])
    (set! state (assert-equal tokens expected
                              "Let expression test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "'(1 2 3)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token QuoteSym "'" (Location 1 1 1 2))
                    (Token LParen "(" (Location 1 2 1 3))
                    (Token Num "1" (Location 1 3 1 4))
                    (Token Num "2" (Location 1 5 1 6))
                    (Token Num "3" (Location 1 7 1 8))
                    (Token RParen ")" (Location 1 8 1 9)))])
    (set! state (assert-equal tokens expected
                              "Quoted list test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token LParen "(" (Location 1 1 1 2))
                    (Token Id "define" (Location 1 2 1 8))
                    (Token LParen "(" (Location 1 9 1 10))
                    (Token Id "fact" (Location 1 10 1 14))
                    (Token Id "n" (Location 1 15 1 16))
                    (Token RParen ")" (Location 1 16 1 17))
                    (Token LParen "(" (Location 1 18 1 19))
                    (Token Id "if" (Location 1 19 1 21))
                    (Token LParen "(" (Location 1 22 1 23))
                    (Token Id "=" (Location 1 23 1 24))
                    (Token Id "n" (Location 1 25 1 26))
                    (Token Num "0" (Location 1 27 1 28))
                    (Token RParen ")" (Location 1 28 1 29))
                    (Token Num "1" (Location 1 30 1 31))
                    (Token LParen "(" (Location 1 32 1 33))
                    (Token Id "*" (Location 1 33 1 34))
                    (Token Id "n" (Location 1 35 1 36))
                    (Token LParen "(" (Location 1 37 1 38))
                    (Token Id "fact" (Location 1 38 1 42))
                    (Token LParen "(" (Location 1 43 1 44))
                    (Token Id "-" (Location 1 44 1 45))
                    (Token Id "n" (Location 1 46 1 47))
                    (Token Num "1" (Location 1 48 1 49))
                    (Token RParen ")" (Location 1 49 1 50))
                    (Token RParen ")" (Location 1 50 1 51))
                    (Token RParen ")" (Location 1 51 1 52))
                    (Token RParen ")" (Location 1 52 1 53))
                    (Token RParen ")" (Location 1 53 1 54)))])
    (set! state (assert-equal tokens expected
                              "Complex factorial definition test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(define (square x)\n  (* x x))"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token LParen "(" (Location 1 1 1 2))
                    (Token Id "define" (Location 1 2 1 8))
                    (Token LParen "(" (Location 1 9 1 10))
                    (Token Id "square" (Location 1 10 1 16))
                    (Token Id "x" (Location 1 17 1 18))
                    (Token RParen ")" (Location 1 18 1 19))
                    (Token LParen "(" (Location 2 3 2 4))
                    (Token Id "*" (Location 2 4 2 5))
                    (Token Id "x" (Location 2 6 2 7))
                    (Token Id "x" (Location 2 8 2 9))
                    (Token RParen ")" (Location 2 9 2 10))
                    (Token RParen ")" (Location 2 10 2 11)))])
    (set! state (assert-equal tokens expected
                              "Multi-line code test"
                              state
                              (make-indented-output-fn output-fn 1))))

  state)

;  test-tokenizer-errors
;     Tests error handling in the tokenizer.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-tokenizer-errors state output-fn)
  (output-fn "Running test-tokenizer-errors...")

  (define assert (lambda (a e m)
                   (assert-equal a e m state (make-indented-output-fn output-fn 1))))

  ;; Test unterminated string
  (let* ([input "\"hello"]
         [result (tokenize input)])
    (set! state (assert (TokenizeError? result) #t 
                        "Unterminated string literal detected as error")))

  ;; Test invalid escape sequence in string
  (let* ([input "\"hello\\"]
         [result (tokenize input)])
    (set! state (assert (TokenizeError? result) #t 
                        "Invalid escape sequence detected as error")))

  ;; Test invalid boolean literal
  (let* ([input "#x"]
         [result (tokenize input)])
    (set! state (assert (TokenizeError? result) #t 
                        "Invalid boolean literal detected as error")))

  ;; Test error message for unterminated string
  (let* ([input "\"hello"]
         [result (tokenize input)])
    (set! state (assert (string-contains? (EtaError-message result) "Unterminated string") #t 
                        "Error message for unterminated string is correct")))

  ;; Test error location
  (let* ([input "\n\n  \"hello"]
         [result (tokenize input)])
    (set! state (assert (Location-sline (EtaError-location result)) 3 
                        "Error location line number is correct"))
    (set! state (assert (Location-scol (EtaError-location result)) 3 
                        "Error location column number is correct")))


  state)

;  run-tokenizer-tests
;     Runs all tokenizer tests.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (run-tokenizer-tests state output-fn)
  (output-fn "Running tokenizer tests...")
  (let ([child-output-fn (make-indented-output-fn output-fn 1)])
    (let ([state (with-error-handling
                     (lambda ()
                       (test-basic-tokens state child-output-fn))
                   "test-basic-tokens"
                   state
                   child-output-fn)])
      (let ([state (with-error-handling
                       (lambda ()
                         (test-boolean-tokens state child-output-fn))
                     "test-boolean-tokens"
                     state
                     child-output-fn)])
        (let ([state (with-error-handling
                         (lambda ()
                           (test-number-tokens state child-output-fn))
                       "test-number-tokens"
                       state
                       child-output-fn)])
          (let ([state (with-error-handling
                           (lambda ()
                             (test-string-tokens state child-output-fn))
                         "test-string-tokens"
                         state
                         child-output-fn)])
            (let ([state (with-error-handling
                             (lambda ()
                               (test-keyword-tokens state child-output-fn))
                           "test-keyword-tokens"
                           state
                           child-output-fn)])
              (let ([state (with-error-handling
                               (lambda ()
                                 (test-identifier-tokens state child-output-fn))
                             "test-identifier-tokens"
                             state
                             child-output-fn)])
                (let ([state (with-error-handling
                                 (lambda ()
                                   (test-tokenizer-errors state child-output-fn))
                               "test-tokenizer-errors"
                               state
                               child-output-fn)])
                  (with-error-handling
                      (lambda ()
                        (test-complex-expressions state child-output-fn))
                    "test-complex-expressions"
                    state
                    child-output-fn))))))))))
