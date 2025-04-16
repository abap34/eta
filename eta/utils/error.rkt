#lang racket

;; Provides error-related functionality for the Eta language
(provide EtaError
         make-eta-error
         EtaError?
         EtaError-message
         EtaError-location
         EtaError-type
         eta-error->string
         format-error-with-source
         make-token-error
         
         ;; New specific error types
         TokenizeError
         make-tokenize-error
         TokenizeError?
         
         ParseError
         make-parse-error
         ParseError?
         
         RuntimeError
         make-runtime-error
         RuntimeError?)

(require "location.rkt" "console.rkt")

;; Generic error type
;; EtaError
;;    Represents a runtime error in the Eta language.
;; Fields:
;;    type - The type of error (e.g., 'syntax, 'runtime, 'parse)
;;    message - A string describing the error
;;    location - Source location information (optional)
(struct EtaError (type message location) #:transparent)

;; Specific error types that extend EtaError
(struct TokenizeError EtaError () #:transparent)
(struct ParseError EtaError () #:transparent)
(struct RuntimeError EtaError () #:transparent)

;; make-eta-error
;;    Creates a new EtaError object.
;; Arguments:
;;    type - The type of error ('syntax, 'runtime, 'parse, etc.)
;;    message - A string describing the error
;;    location - Source location information (optional, defaults to #f)
;; Returns:
;;    A new EtaError instance
;; Example:
;;    (make-eta-error 'runtime "Undefined variable" (Location 10 5 10 6))
(define (make-eta-error type message [location #f])
  (EtaError type message location))

;; make-tokenize-error
;;    Creates a new TokenizeError object.
;; Arguments:
;;    message - A string describing the error
;;    location - Source location information
;; Returns:
;;    A new TokenizeError instance
;; Example:
;;    (make-tokenize-error "Unexpected character" (make-location 1 5 1 6))
(define (make-tokenize-error message location)
  (TokenizeError 'tokenize message location))

;; make-parse-error
;;    Creates a new ParseError object.
;; Arguments:
;;    message - A string describing the error
;;    location - Source location information
;; Returns:
;;    A new ParseError instance
;; Example:
;;    (make-parse-error "Expected closing parenthesis" (make-location 2 10 2 11))
(define (make-parse-error message location)
  (ParseError 'parse message location))

;; make-runtime-error
;;    Creates a new RuntimeError object.
;; Arguments:
;;    message - A string describing the error
;;    location - Source location information (optional, defaults to #f)
;; Returns:
;;    A new RuntimeError instance
;; Example:
;;    (make-runtime-error "Division by zero" (make-location 3 15 3 18))
(define (make-runtime-error message [location #f])
  (RuntimeError 'runtime message location))

;; make-token-error
;;    Creates an EtaError for tokenizer errors
;; Arguments:
;;    message - Error message
;;    line - Line number
;;    col - Column number
;; Returns:
;;    A TokenizeError struct
(define (make-token-error message line col)
  (make-tokenize-error message (make-location line col col)))

;; eta-error->string
;;    Converts an EtaError to a human-readable string.
;; Arguments:
;;    error - The EtaError to format
;; Returns:
;;    A string representation of the error
;; Example:
;;    (eta-error->string (make-eta-error 'runtime "Undefined variable" (Location 10 5 10 6)))
;;    ; => "RuntimeError at line 10, column 5: Undefined variable"
(define (eta-error->string error)
  (let ([type (EtaError-type error)]
        [message (EtaError-message error)]
        [location (EtaError-location error)]
        [error-type (cond
                      [(TokenizeError? error) "TokenizeError"]
                      [(ParseError? error) "ParseError"]
                      [(RuntimeError? error) "RuntimeError"]
                      [else "Error"])])
    (string-append
     error-type
     (if location
         (format " at ~a" (location->string location))
         "")
     (format ":\n~a" message))))

;; format-error-with-source
;;    Formats an error with visual markers pointing to the error location in the source code.
;; Arguments:
;;    error - The EtaError to format
;;    source - The source code string where the error occurred
;; Returns:
;;    A formatted string with error message and visual markers
;; Example:
;;    (format-error-with-source 
;;       (make-eta-error 'runtime "Unexpected token" (Location 1 10 1 15))
;;       "(define (f x) (+ x 1")
;;    ; => "RuntimeError at line 1, column 10: Unexpected token"
;;    ;     (define (f x) (+ x 1
;;    ;                ^^^^^
(define (format-error-with-source error source)
  (let* ([message (colorize (eta-error->string error) 'red)]
         [location (EtaError-location error)])
    
    (if (and location source)
        (let* ([lines (string-split source "\n")]
               [sline (Location-sline location)]
               [scol (Location-scol location)]
               [eline (Location-eline location)]
               [ecol (Location-ecol location)])
          
          (if (and (>= sline 1) (<= sline (length lines)))
              (let* ([error-line (list-ref lines (- sline 1))]
                     [line-num-str (number->string sline)]
                     [line-num (colorize (string-append "    " line-num-str " | ") 'blue)]
                     [code-line (string-append line-num error-line "\n")]
                     [marker-padding (make-string (+ 4 (string-length line-num-str) 3 (max 0 (- scol 1))) #\space)]
                     [marker (make-string (max 1 (- ecol scol)) #\^)]
                     [marker-line (string-append marker-padding (colorize marker 'red) "\n")])
                (string-append "\n" code-line marker-line message))
              
              (string-append message "Error location out of bounds\n")))
        
        message)))
