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
           (eq? (Token-typ (last tokens)) 'EOFToken))
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
         [expected (list (Token 'LParenToken "(" (Location #f 1 1 1 2)))])
    (set! state (assert-equal tokens expected
                              "LParen token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input ")"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'RParenToken ")" (Location #f 1 1 1 2)))])
    (set! state (assert-equal tokens expected
                              "RParen token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "."]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'DotSymToken "." (Location #f 1 1 1 2)))])
    (set! state (assert-equal tokens expected
                              "DotSym token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "'"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'QuoteSymToken "'" (Location #f 1 1 1 2)))])
    (set! state (assert-equal tokens expected
                              "Quote token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "()"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'RParenToken ")" (Location #f 1 2 1 3)))])
    (set! state (assert-equal tokens expected
                              "Multiple token test for parentheses"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(a . b)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'IdToken "a" (Location #f 1 2 1 3))
                    (Token 'DotSymToken "." (Location #f 1 4 1 5))
                    (Token 'IdToken "b" (Location #f 1 6 1 7))
                    (Token 'RParenToken ")" (Location #f 1 7 1 8)))])
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
         [expected (list (Token 'BoolToken "#t" (Location #f 1 1 1 3)))])
    (set! state (assert-equal tokens expected
                              "True boolean token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "#f"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'BoolToken "#f" (Location #f 1 1 1 3)))])
    (set! state (assert-equal tokens expected
                              "False boolean token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(if #t 1 #f)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'KeywordToken "if" (Location #f 1 2 1 4))
                    (Token 'BoolToken "#t" (Location #f 1 5 1 7))
                    (Token 'IntToken "1" (Location #f 1 8 1 9))
                    (Token 'BoolToken "#f" (Location #f 1 10 1 12))
                    (Token 'RParenToken ")" (Location #f 1 12 1 13)))])
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
         [expected (list (Token 'IntToken "123" (Location #f 1 1 1 4)))])
    (set! state (assert-equal tokens expected
                              "Simple number token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "123 456"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'IntToken "123" (Location #f 1 1 1 4))
                    (Token 'IntToken "456" (Location #f 1 5 1 8)))])
    (set! state (assert-equal tokens expected
                              "Multiple number token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(+ 1 2)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'IdToken "+" (Location #f 1 2 1 3))
                    (Token 'IntToken "1" (Location #f 1 4 1 5))
                    (Token 'IntToken "2" (Location #f 1 6 1 7))
                    (Token 'RParenToken ")" (Location #f 1 7 1 8)))])
    (set! state (assert-equal tokens expected
                              "Numbers in expression test"
                              state
                              (make-indented-output-fn output-fn 1))))
                              
  ; Test for negative integer
  (let* ([input "-123"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'IntToken "-123" (Location #f 1 1 1 5)))])
    (set! state (assert-equal tokens expected
                              "Negative integer token test"
                              state
                              (make-indented-output-fn output-fn 1))))
                              
  ; Test for negative number in expression
  (let* ([input "(+ -1 2)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'IdToken "+" (Location #f 1 2 1 3))
                    (Token 'IntToken "-1" (Location #f 1 4 1 6))
                    (Token 'IntToken "2" (Location #f 1 7 1 8))
                    (Token 'RParenToken ")" (Location #f 1 8 1 9)))])
    (set! state (assert-equal tokens expected
                              "Negative number in expression test"
                              state
                              (make-indented-output-fn output-fn 1))))
                              
  ; Test for minus symbol followed by space (should be an identifier, not a negative number)
  (let* ([input "- 123"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'IdToken "-" (Location #f 1 1 1 2))
                    (Token 'IntToken "123" (Location #f 1 3 1 6)))])
    (set! state (assert-equal tokens expected
                              "Minus symbol followed by space test"
                              state
                              (make-indented-output-fn output-fn 1))))

  state)

;  test-float-tokens
;     Tests floating point number token recognition.
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-float-tokens state output-fn)
  (output-fn "Running test-float-tokens...")

  (let* ([input "123.45"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'FloatToken "123.45" (Location #f 1 1 1 7)))])
    (set! state (assert-equal tokens expected
                              "Simple float token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "0.5"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'FloatToken "0.5" (Location #f 1 1 1 4)))])
    (set! state (assert-equal tokens expected
                              "Zero point float token test"
                              state
                              (make-indented-output-fn output-fn 1))))
                              
  (let* ([input "3.14159"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'FloatToken "3.14159" (Location #f 1 1 1 8)))])
    (set! state (assert-equal tokens expected
                              "Pi approximation float token test"
                              state
                              (make-indented-output-fn output-fn 1))))
                              
  ; Test for negative float
  (let* ([input "-3.14"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'FloatToken "-3.14" (Location #f 1 1 1 6)))])
    (set! state (assert-equal tokens expected
                              "Negative float token test"
                              state
                              (make-indented-output-fn output-fn 1))))
                              
  ; Test for negative float in expression
  (let* ([input "(+ -1.5 2.7)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'IdToken "+" (Location #f 1 2 1 3))
                    (Token 'FloatToken "-1.5" (Location #f 1 4 1 8))
                    (Token 'FloatToken "2.7" (Location #f 1 9 1 12))
                    (Token 'RParenToken ")" (Location #f 1 12 1 13)))])
    (set! state (assert-equal tokens expected
                              "Negative float in expression test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(+ 1.5 2.7)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'IdToken "+" (Location #f 1 2 1 3))
                    (Token 'FloatToken "1.5" (Location #f 1 4 1 7))
                    (Token 'FloatToken "2.7" (Location #f 1 8 1 11))
                    (Token 'RParenToken ")" (Location #f 1 11 1 12)))])
    (set! state (assert-equal tokens expected
                              "Floats in expression test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(* 3 4.25)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'IdToken "*" (Location #f 1 2 1 3))
                    (Token 'IntToken "3" (Location #f 1 4 1 5))
                    (Token 'FloatToken "4.25" (Location #f 1 6 1 10))
                    (Token 'RParenToken ")" (Location #f 1 10 1 11)))])
    (set! state (assert-equal tokens expected
                              "Mixed integer and float in expression test"
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
         [expected (list (Token 'StringToken "hello" (Location #f 1 1 1 8)))])
    (set! state (assert-equal tokens expected
                              "Simple string token test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "\"hello\\\"world\""]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'StringToken "hello\"world" (Location #f 1 1 1 15)))])
    (set! state (assert-equal tokens expected
                              "StringToken with escaped quote test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "\"line1\\nline2\\tindent\""]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'StringToken (string-append "line1" (string #\newline) "line2" (string #\tab) "indent")
                                (Location #f 1 1 1 23)))])
    (set! state (assert-equal tokens expected
                              "StringToken with special escapes test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(display \"hello\")"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'IdToken "display" (Location #f 1 2 1 9))
                    (Token 'StringToken "hello" (Location #f 1 10 1 17))
                    (Token 'RParenToken ")" (Location #f 1 17 1 18)))])
    (set! state (assert-equal tokens expected
                              "StringToken in expression test"
                              state
                              (make-indented-output-fn output-fn 1))))

  state)

;  test-keyword-tokens
;     Tests keyword token recognition (define, lambda, if etc).
;  Arguments:
;      state - The current test state.
;      output-fn - Function to display output.
;  Returns:
;      Updated test state.
(define (test-keyword-tokens state output-fn)
  (output-fn "Running test-keyword-tokens...")

  (let* ([input "define"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'KeywordToken "define" (Location #f 1 1 1 7)))])
    (set! state (assert-equal tokens expected
                              "define keyword test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "lambda"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'KeywordToken "lambda" (Location #f 1 1 1 7)))])
    (set! state (assert-equal tokens expected
                              "lambda keyword test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "let let* letrec if cond quote set! and or begin do load else"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'KeywordToken "let" (Location #f 1 1 1 4))
                    (Token 'KeywordToken "let*" (Location #f 1 5 1 9))
                    (Token 'KeywordToken "letrec" (Location #f 1 10 1 16))
                    (Token 'KeywordToken "if" (Location #f 1 17 1 19))
                    (Token 'KeywordToken "cond" (Location #f 1 20 1 24))
                    (Token 'KeywordToken "quote" (Location #f 1 25 1 30))
                    (Token 'KeywordToken "set!" (Location #f 1 31 1 35))
                    (Token 'KeywordToken "and" (Location #f 1 36 1 39))
                    (Token 'KeywordToken "or" (Location #f 1 40 1 42))
                    (Token 'KeywordToken "begin" (Location #f 1 43 1 48))
                    (Token 'KeywordToken "load" (Location #f 1 52 1 56))
                    (Token 'KeywordToken "else" (Location #f 1 57 1 61)))])
    (set! state (assert-equal tokens expected
                              "All keywords test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(define x 10)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'KeywordToken "define" (Location #f 1 2 1 8))
                    (Token 'IdToken "x" (Location #f 1 9 1 10))
                    (Token 'IntToken "10" (Location #f 1 11 1 13))
                    (Token 'RParenToken ")" (Location #f 1 13 1 14)))])
    (set! state (assert-equal tokens expected
                              "Keyword in expression test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(if #t (lambda (x) x) (define-syntax macro '()))"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'KeywordToken "if" (Location #f 1 2 1 4))
                    (Token 'BoolToken "#t" (Location #f 1 5 1 7))
                    (Token 'LParenToken "(" (Location #f 1 8 1 9))
                    (Token 'KeywordToken "lambda" (Location #f 1 9 1 15))
                    (Token 'LParenToken "(" (Location #f 1 16 1 17))
                    (Token 'IdToken "x" (Location #f 1 17 1 18))
                    (Token 'RParenToken ")" (Location #f 1 18 1 19))
                    (Token 'IdToken "x" (Location #f 1 20 1 21))
                    (Token 'RParenToken ")" (Location #f 1 21 1 22))
                    (Token 'LParenToken "(" (Location #f 1 23 1 24))
                    (Token 'IdToken "define-syntax" (Location #f 1 24 1 37))
                    (Token 'IdToken "macro" (Location #f 1 38 1 43))
                    (Token 'QuoteSymToken "'" (Location #f 1 44 1 45))
                    (Token 'LParenToken "(" (Location #f 1 45 1 46))
                    (Token 'RParenToken ")" (Location #f 1 46 1 47))
                    (Token 'RParenToken ")" (Location #f 1 47 1 48))
                    (Token 'RParenToken ")" (Location #f 1 48 1 49)))])
    (set! state (assert-equal tokens expected
                              "Mixed keywords and identifiers test"
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
         [expected (list (Token 'IdToken "x" (Location #f 1 1 1 2)))])
    (set! state (assert-equal tokens expected
                              "Simple identifier test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "hello-world!"]
         [tokens (tokenize-without-eof input)]
         [expected (list (Token 'IdToken "hello-world!" (Location #f 1 1 1 13)))])
    (set! state (assert-equal tokens expected
                              "Identifier with special chars test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "x2 y3z"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'IdToken "x2" (Location #f 1 1 1 3))
                    (Token 'IdToken "y3z" (Location #f 1 4 1 7)))])
    (set! state (assert-equal tokens expected
                              "Identifiers with numbers test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "+ - * / < = > ! $ % & = @ ^ _"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'IdToken "+" (Location #f 1 1 1 2))
                    (Token 'IdToken "-" (Location #f 1 3 1 4))
                    (Token 'IdToken "*" (Location #f 1 5 1 6))
                    (Token 'IdToken "/" (Location #f 1 7 1 8))
                    (Token 'IdToken "<" (Location #f 1 9 1 10))
                    (Token 'IdToken "=" (Location #f 1 11 1 12))
                    (Token 'IdToken ">" (Location #f 1 13 1 14))
                    (Token 'IdToken "!" (Location #f 1 15 1 16))
                    (Token 'IdToken "$" (Location #f 1 17 1 18))
                    (Token 'IdToken "%" (Location #f 1 19 1 20))
                    (Token 'IdToken "&" (Location #f 1 21 1 22))
                    (Token 'IdToken "=" (Location #f 1 23 1 24))
                    (Token 'IdToken "@" (Location #f 1 25 1 26))
                    (Token 'IdToken "^" (Location #f 1 27 1 28))
                    (Token 'IdToken "_" (Location #f 1 29 1 30)))])
    (set! state (assert-equal tokens expected
                              "Symbolic identifiers test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "map filter fold-left fold-right"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'IdToken "map" (Location #f 1 1 1 4))
                    (Token 'IdToken "filter" (Location #f 1 5 1 11))
                    (Token 'IdToken "fold-left" (Location #f 1 12 1 21))
                    (Token 'IdToken "fold-right" (Location #f 1 22 1 32)))])
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
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'KeywordToken "lambda" (Location #f 1 2 1 8))
                    (Token 'LParenToken "(" (Location #f 1 9 1 10))
                    (Token 'IdToken "x" (Location #f 1 10 1 11))
                    (Token 'RParenToken ")" (Location #f 1 11 1 12))
                    (Token 'LParenToken "(" (Location #f 1 13 1 14))
                    (Token 'IdToken "+" (Location #f 1 14 1 15))
                    (Token 'IdToken "x" (Location #f 1 16 1 17))
                    (Token 'IntToken "1" (Location #f 1 18 1 19))
                    (Token 'RParenToken ")" (Location #f 1 19 1 20))
                    (Token 'RParenToken ")" (Location #f 1 20 1 21)))])
    (set! state (assert-equal tokens expected
                              "Lambda expression test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(let ((x 1) (y 2)) (+ x y))"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'KeywordToken "let" (Location #f 1 2 1 5))
                    (Token 'LParenToken "(" (Location #f 1 6 1 7))
                    (Token 'LParenToken "(" (Location #f 1 7 1 8))
                    (Token 'IdToken "x" (Location #f 1 8 1 9))
                    (Token 'IntToken "1" (Location #f 1 10 1 11))
                    (Token 'RParenToken ")" (Location #f 1 11 1 12))
                    (Token 'LParenToken "(" (Location #f 1 13 1 14))
                    (Token 'IdToken "y" (Location #f 1 14 1 15))
                    (Token 'IntToken "2" (Location #f 1 16 1 17))
                    (Token 'RParenToken ")" (Location #f 1 17 1 18))
                    (Token 'RParenToken ")" (Location #f 1 18 1 19))
                    (Token 'LParenToken "(" (Location #f 1 20 1 21))
                    (Token 'IdToken "+" (Location #f 1 21 1 22))
                    (Token 'IdToken "x" (Location #f 1 23 1 24))
                    (Token 'IdToken "y" (Location #f 1 25 1 26))
                    (Token 'RParenToken ")" (Location #f 1 26 1 27))
                    (Token 'RParenToken ")" (Location #f 1 27 1 28)))])
    (set! state (assert-equal tokens expected
                              "Let expression test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "'(1 2 3)"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'QuoteSymToken "'" (Location #f 1 1 1 2))
                    (Token 'LParenToken "(" (Location #f 1 2 1 3))
                    (Token 'IntToken "1" (Location #f 1 3 1 4))
                    (Token 'IntToken "2" (Location #f 1 5 1 6))
                    (Token 'IntToken "3" (Location #f 1 7 1 8))
                    (Token 'RParenToken ")" (Location #f 1 8 1 9)))])
    (set! state (assert-equal tokens expected
                              "Quoted list test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'KeywordToken "define" (Location #f 1 2 1 8))
                    (Token 'LParenToken "(" (Location #f 1 9 1 10))
                    (Token 'IdToken "fact" (Location #f 1 10 1 14))
                    (Token 'IdToken "n" (Location #f 1 15 1 16))
                    (Token 'RParenToken ")" (Location #f 1 16 1 17))
                    (Token 'LParenToken "(" (Location #f 1 18 1 19))
                    (Token 'KeywordToken "if" (Location #f 1 19 1 21))
                    (Token 'LParenToken "(" (Location #f 1 22 1 23))
                    (Token 'IdToken "=" (Location #f 1 23 1 24))
                    (Token 'IdToken "n" (Location #f 1 25 1 26))
                    (Token 'IntToken "0" (Location #f 1 27 1 28))
                    (Token 'RParenToken ")" (Location #f 1 28 1 29))
                    (Token 'IntToken "1" (Location #f 1 30 1 31))
                    (Token 'LParenToken "(" (Location #f 1 32 1 33))
                    (Token 'IdToken "*" (Location #f 1 33 1 34))
                    (Token 'IdToken "n" (Location #f 1 35 1 36))
                    (Token 'LParenToken "(" (Location #f 1 37 1 38))
                    (Token 'IdToken "fact" (Location #f 1 38 1 42))
                    (Token 'LParenToken "(" (Location #f 1 43 1 44))
                    (Token 'IdToken "-" (Location #f 1 44 1 45))
                    (Token 'IdToken "n" (Location #f 1 46 1 47))
                    (Token 'IntToken "1" (Location #f 1 48 1 49))
                    (Token 'RParenToken ")" (Location #f 1 49 1 50))
                    (Token 'RParenToken ")" (Location #f 1 50 1 51))
                    (Token 'RParenToken ")" (Location #f 1 51 1 52))
                    (Token 'RParenToken ")" (Location #f 1 52 1 53))
                    (Token 'RParenToken ")" (Location #f 1 53 1 54)))])
    (set! state (assert-equal tokens expected
                              "Complex factorial definition test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(define (square x)\n  (* x x))"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'KeywordToken "define" (Location #f 1 2 1 8))
                    (Token 'LParenToken "(" (Location #f 1 9 1 10))
                    (Token 'IdToken "square" (Location #f 1 10 1 16))
                    (Token 'IdToken "x" (Location #f 1 17 1 18))
                    (Token 'RParenToken ")" (Location #f 1 18 1 19))
                    (Token 'LParenToken "(" (Location #f 2 3 2 4))
                    (Token 'IdToken "*" (Location #f 2 4 2 5))
                    (Token 'IdToken "x" (Location #f 2 6 2 7))
                    (Token 'IdToken "x" (Location #f 2 8 2 9))
                    (Token 'RParenToken ")" (Location #f 2 9 2 10))
                    (Token 'RParenToken ")" (Location #f 2 10 2 11)))])
    (set! state (assert-equal tokens expected
                              "Multi-line code test"
                              state
                              (make-indented-output-fn output-fn 1))))

  (let* ([input "(cond ((= x 0) 1) ((> x 0) x) (else 0))"]
         [tokens (tokenize-without-eof input)]
         [expected (list
                    (Token 'LParenToken "(" (Location #f 1 1 1 2))
                    (Token 'KeywordToken "cond" (Location #f 1 2 1 6))
                    (Token 'LParenToken "(" (Location #f 1 7 1 8))
                    (Token 'LParenToken "(" (Location #f 1 8 1 9))
                    (Token 'IdToken "=" (Location #f 1 9 1 10))
                    (Token 'IdToken "x" (Location #f 1 11 1 12))
                    (Token 'IntToken "0" (Location #f 1 13 1 14))
                    (Token 'RParenToken ")" (Location #f 1 14 1 15))
                    (Token 'IntToken "1" (Location #f 1 16 1 17))
                    (Token 'RParenToken ")" (Location #f 1 17 1 18))
                    (Token 'LParenToken "(" (Location #f 1 19 1 20))
                    (Token 'LParenToken "(" (Location #f 1 20 1 21))
                    (Token 'IdToken ">" (Location #f 1 21 1 22))
                    (Token 'IdToken "x" (Location #f 1 23 1 24))
                    (Token 'IntToken "0" (Location #f 1 25 1 26))
                    (Token 'RParenToken ")" (Location #f 1 26 1 27))
                    (Token 'IdToken "x" (Location #f 1 28 1 29))
                    (Token 'RParenToken ")" (Location #f 1 29 1 30))
                    (Token 'LParenToken "(" (Location #f 1 31 1 32))
                    (Token 'KeywordToken "else" (Location #f 1 32 1 36))
                    (Token 'IntToken "0" (Location #f 1 37 1 38))
                    (Token 'RParenToken ")" (Location #f 1 38 1 39))
                    (Token 'RParenToken ")" (Location #f 1 39 1 40)))])
    (set! state (assert-equal tokens expected
                              "Cond expression test"
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
                             (test-float-tokens state child-output-fn))
                         "test-float-tokens"
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
                      child-output-fn)))))))))))
