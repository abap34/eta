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
         localize-error-location
         TokenizeError
         make-tokenize-error
         TokenizeError?
         ParseError
         make-parse-error
         ParseError?
         RuntimeError
         make-runtime-error
         RuntimeError?
         EvaluationInterruptedError
         make-evaluation-interrupted-error
         EvaluationInterruptedError?
         )

(require "location.rkt" "console.rkt" racket/file)

;; Generic error type
;; EtaError
;;    Represents a runtime error in the Eta language.
;; Fields:
;;    type - The type of error (e.g., 'syntax, 'runtime, 'parse)
;;    message - A string describing the error
;;    location - Source location information (optional). If no location, #f
(struct EtaError (type message location) #:transparent)

;; Specific error types that extend EtaError
(struct TokenizeError EtaError () #:transparent)
(struct ParseError EtaError () #:transparent)
(struct RuntimeError EtaError () #:transparent)
(struct EvaluationInterruptedError RuntimeError () #:transparent)

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
;; Returns:      []
;;    A new RuntimeError instance
;; Example:
;;    (make-runtime-error "Division by zero" (make-location 3 15 3 18))
(define (make-runtime-error message [location #f])
  (unless (string? message)
    (error "make-runtime-error: message must be a string"))
  (RuntimeError 'runtime message location))

;; make-evaluation-interrupted-error
;;    Creates a new EvaluationInterruptedError object.
;; Arguments:
;;    message - A string describing the error
;;    location - Source location information (optional, defaults to #f)
;; Returns:
;;    A new EvaluationInterruptedError instance
(define (make-evaluation-interrupted-error [message "Evaluation interrupted by user"] [location #f])
  (EvaluationInterruptedError 'interrupted message location))
  

;; make-token-error
;;    Creates an EtaError for tokenizer errors
;; Arguments:
;;    message - Error message
;;    line - Line number
;;    col - Column number
;;    file - Optional file name (string, symbol, or #f)
;; Returns:
;;    A TokenizeError struct
(define (make-token-error message line col [file #f])
  (make-tokenize-error message (make-location line col col file)))

; localize-error-location
;;    Adds location information to an EtaError.
;; Arguments:
;;    error - The EtaError to modify
;;    location - The location information to add
;; Returns:
;;    The modified EtaError with location information
(define (localize-error-location error location)
  (if (EtaError? error)
      (let ([message (EtaError-message error)])
         (cond 
           [(TokenizeError? error) (TokenizeError (EtaError-type error) message location)]
           [(ParseError? error) (ParseError (EtaError-type error) message location)]
           [(RuntimeError? error) (RuntimeError (EtaError-type error) message location)]
           [(EvaluationInterruptedError? error) (EvaluationInterruptedError (EtaError-type error) message location)]
         )
      )
      (error (format "Expected an EtaError, got: ~a" error))))


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
                      [(EvaluationInterruptedError? error) "EvaluationInterruptedError"]
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
;; Returns:
;;    A formatted string with error message and visual markers
(define (format-error-with-source error)
  (let* ([message (colorize (eta-error->string error) 'red)]
         [location (EtaError-location error)]
         [source-code (and location 
                           (Location-file location)
                           (string? (Location-file location))
                           (file-exists? (Location-file location))
                           (with-handlers ([exn:fail? (lambda (e) #f)])
                             (file->string (Location-file location))))])
    
    (if (and location source-code)
        (let* ([lines (string-split source-code "\n")]
               [file (Location-file location)]
               [file-display (cond
                              [(string? file) (format "~a:" file)]
                              [(symbol? file) (format "~a:" (symbol->string file))]
                              [else ""])]
               [sline (Location-sline location)]
               [scol (Location-scol location)]
               [eline (Location-eline location)]
               [ecol (Location-ecol location)]
               [nlines (length lines)]
               [line-num-str (number->string sline)]
               [line-num-len (string-length line-num-str)]

               [prefix
                (lambda (lnum)
                  (colorize (string-append "    " (number->string lnum) " | ") 'blue))]

               [get-line
                (lambda (i)
                  (if (and (<= 1 i) (<= i nlines))
                      (list-ref lines (- i 1))
                      ""))]
                      
               [range
                (lambda (start end)
                  (if (> start end)
                      '()
                      (let loop ([i start] [acc '()])
                        (if (> i end)
                            (reverse acc)
                            (loop (+ i 1) (cons i acc))))))]

               [join-lines
                (lambda (nums)
                  (apply string-append
                         (map (lambda (i)
                                (string-append (prefix i) (get-line i) "\n"))
                              nums)))]

               [before-lines (let ([start-line (max 1 (- sline 2))]
                                   [end-line (- sline 1)])
                               (cond
                                 [(< end-line 1) ""]  
                                 [(> start-line end-line) (join-lines (list end-line))] 
                                 [else (join-lines (range start-line end-line))]))]
               [error-line (get-line sline)]
               [code-line (string-append (prefix sline) error-line "\n")]
               [marker-padding (make-string (+ 4 line-num-len 3 (max 0 (- scol 1))) #\space)]
               [marker (make-string (max 1 (- ecol scol)) #\^)]
               [marker-line (string-append marker-padding (colorize marker 'red) "\n")]
               [after-lines (let ([end-line (min nlines (+ eline 2))])
                              (if (<= end-line eline)
                                  ""
                                  (join-lines (range (+ eline 1) end-line))))])
          
          (string-append "\n" 
                         (if (not (string=? file-display ""))
                             (string-append (colorize file-display 'magenta) "\n")
                             "")
                         before-lines code-line marker-line after-lines message))
        
        message)))
