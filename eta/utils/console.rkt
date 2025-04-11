#lang racket

(provide colorize bold)

(define esc "\x1b")

(define reset (string-append esc "[0m"))

(define bold-code (string-append esc "[1m"))

(define color-codes
  (list
    (cons 'black (string-append esc "[30m"))
    (cons 'red (string-append esc "[31m"))
    (cons 'green (string-append esc "[32m"))
    (cons 'yellow (string-append esc "[33m"))
    (cons 'blue (string-append esc "[34m"))
    (cons 'magenta (string-append esc "[35m"))
    (cons 'cyan (string-append esc "[36m"))
    (cons 'white (string-append esc "[37m"))
  ))

;  get-color-code
;      Get ANSI escape code string for a given color symbol.
;  Arguments:
;      color-symbol : a symbol like 'red, 'blue, etc.
;  Returns:
;      A string representing the ANSI escape sequence for that color.
(define (get-color-code color-symbol)
  (cond ((assoc color-symbol color-codes) => cdr)
        (else (error 'get-color-code "Unsupported color" color-symbol))))

;  colorize
;      Wrap a string with ANSI escape sequences for a　given color.
;  Arguments:
;      str          : the string to be colorized
;      color-symbol : a symbol like 'red, 'blue, etc.
;  Returns:
;      A string with ANSI color formatting applied.
(define (colorize str color-symbol)
  (string-append (get-color-code color-symbol) str reset))

;  bold
;      Wrap a string with ANSI escape sequences for bold text.
;  Arguments:
;      str : the string to be bolded
;  Returns:
;      A string with ANSI bold formatting applied.
(define (bold str)
  (string-append bold-code str reset))