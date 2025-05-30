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
         RuntimeError-stack-trace
         EvaluationInterruptedError
         make-evaluation-interrupted-error
         EvaluationInterruptedError?
         
         ;; Stack trace related exports
         StackTraceEntry
         make-stack-trace-entry
         StackTraceEntry?
         StackTraceEntry-func-name
         StackTraceEntry-location
         add-stack-trace-to-error
         MAX-STACK-TRACE-DISPLAY
         )

(require "location.rkt" "console.rkt" racket/file)

;; Maximum depth of stack trace to display
(define MAX-STACK-TRACE-DISPLAY 20)

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

;; Stack trace related structures
;; StackTraceEntry
;;    Represents a single entry in a stack trace
;; Fields:
;;    func-name - The name of the function (string)
;;    location - Source location information
(struct StackTraceEntry (func-name location) #:transparent)

;; make-stack-trace-entry
;;    Creates a new StackTraceEntry object.
;; Arguments:
;;    func-name - The name of the function
;;    location - Source location information
;; Returns:
;;    A new StackTraceEntry instance
(define (make-stack-trace-entry func-name location)
  (StackTraceEntry func-name location))

;; RuntimeError with built-in stack trace
;; RuntimeError
;;    Represents a runtime error with stack trace information
;; Fields:
;;    type, message, location - Same as EtaError
;;    stack-trace - A list of StackTraceEntry objects (optional, can be empty list)
(struct RuntimeError EtaError (stack-trace) #:transparent)
(struct EvaluationInterruptedError RuntimeError () #:transparent)

;; make-runtime-error
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
;;    stack-trace - A list of StackTraceEntry objects (optional, defaults to empty list)
;; Returns:      []
;;    A new RuntimeError instance
;; Example:
;;    (make-runtime-error "Division by zero" (make-location 3 15 3 18))
(define (make-runtime-error message [location #f] [stack-trace '()])
  (unless (string? message)
    (error "make-runtime-error: message must be a string"))
  (unless (list? stack-trace)
    (error "make-runtime-error: stack-trace must be a list"))
  (RuntimeError 'runtime message location stack-trace))

;; make-evaluation-interrupted-error
;;    Creates a new EvaluationInterruptedError object.
;; Arguments:
;;    message - A string describing the error
;;    location - Source location information (optional, defaults to #f)
;;    stack-trace - A list of StackTraceEntry objects (optional, defaults to empty list)
;; Returns:
;;    A new EvaluationInterruptedError instance
(define (make-evaluation-interrupted-error [message "Evaluation interrupted by user"] [location #f] [stack-trace '()])
  (EvaluationInterruptedError 'interrupted message location stack-trace))
  

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
           [(RuntimeError? error) 
            (RuntimeError (EtaError-type error) message location (RuntimeError-stack-trace error))]
           [(EvaluationInterruptedError? error) 
            (EvaluationInterruptedError (EtaError-type error) message location 
                                        (RuntimeError-stack-trace error))]
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
     (format ":\n~a" message)
     
     ;; Add stack trace if available
     (if (RuntimeError? error)
         (let ([stack-trace (RuntimeError-stack-trace error)])
           (if (null? stack-trace)
               ""
               (let* ([limited-stack (take (if (> (length stack-trace) MAX-STACK-TRACE-DISPLAY)
                                              (append (take stack-trace MAX-STACK-TRACE-DISPLAY)
                                                    (list (make-stack-trace-entry "..." #f)))
                                              stack-trace)
                                          (min (+ 1 MAX-STACK-TRACE-DISPLAY) (length stack-trace)))]
                      [stack-strings 
                       (map (lambda (entry)
                              (let ([func-name (StackTraceEntry-func-name entry)]
                                    [loc (StackTraceEntry-location entry)])
                                (string-append 
                                 "  at " func-name 
                                 (if loc
                                     (format " (at ~a)" (location->string loc))
                                     ""))))
                            limited-stack)])
                 (string-append "\n\nStack trace:\n" (string-join stack-strings "\n")))))
         ""))))

;; format-error-with-source
;;    Formats an error with visual markers pointing to the error location in the source code.
;; Arguments:
;;    error - The EtaError to format
;;    [source-getter] - Optional function to get source code from a file identifier.
;; Returns:
;;    A formatted string with error message and visual markers
(define (format-error-with-source error [source-getter #f])
  (let* ([message (colorize (eta-error->string error) 'red)]
         [location (EtaError-location error)]
         [source-code (cond
                        [(and source-getter location (Location-file location))
                         (source-getter (Location-file location))]
                        [(and location 
                              (Location-file location)
                              (string? (Location-file location))
                              (file-exists? (Location-file location)))
                         (with-handlers ([exn:fail? (lambda (e) #f)])
                           (file->string (Location-file location)))]
                        [else #f])])
    
    (if (and location source-code)
        (let* ([lines (string-split source-code "\n")]
               [file (Location-file location)]
               [file-display (cond
                              [(string? file) (format "~a:" file)]
                              [(symbol? file) (format "~a:" (symbol->string file))]
                              [(and (list? file) (= (length file) 2) (eq? (first file) 'repl-history))
                               (format "REPL[~a]:" (second file))]
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

;; add-stack-trace-to-error
;;    Adds stack trace information to a RuntimeError
;; Arguments:
;;    error - The RuntimeError
;;    stack-entries - List of StackTraceEntry objects
;; Returns:
;;    A RuntimeError object with the stack trace added
(define (add-stack-trace-to-error error stack-entries)
  (if (RuntimeError? error)
      (let ([message (EtaError-message error)]
            [location (EtaError-location error)]
            [current-stack (RuntimeError-stack-trace error)])
        (if (null? current-stack)
            ;; If no stack trace yet, add it
            (make-runtime-error message location stack-entries)
            ;; Otherwise, keep the existing one
            error))
      (error (format "Expected a RuntimeError, got: ~a" error))))
