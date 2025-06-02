#lang racket

(provide StringState
         ParseState
         string-state
         count-bracket-balance
         input-needs-continuation?
         read-multi-line-input)

(require "../utils/console.rkt")

;; String analysis state
(struct StringState (in-string? escaped?) #:transparent)

;; Parser state including bracket balance and string state
(struct ParseState (balance string-state) #:transparent)

;  string-state
;     Analyze the string literal state in a given string
;  Arguments:
;     str - Input string to analyze
;     [initial-state] - Initial string analysis state (default: not in string, not escaped)
;  Returns:
;     A StringState struct
(define (string-state str [initial-state (StringState #f #f)])
  (define (process-chars chars state)
    (if (null? chars)
        state  ; Return final state
        (let* ([char (car chars)]
               [rest (cdr chars)]
               [in-string? (StringState-in-string? state)]
               [escaped? (StringState-escaped? state)])
          (cond
            ;; Handle escape sequences inside strings
            [(and in-string? escaped?)
             (process-chars rest (StringState in-string? #f))]
            
            ;; Handle escape character
            [(and in-string? (char=? char #\\))
             (process-chars rest (StringState in-string? #t))]
            
            ;; Handle string delimiters
            [(char=? char #\")
             (process-chars rest (StringState (not in-string?) #f))]
            
            ;; Skip other characters
            [else
             (process-chars rest (StringState in-string? #f))]))))
  
  (process-chars (string->list str) initial-state))

;  count-bracket-balance
;     Count the bracket balance (left - right) in a string
;     Ignoring brackets inside string literals
;  Arguments:
;     str - Input string to analyze
;     [initial-state] - Initial parse state (default: balance 0, not in string, not escaped)
;  Returns:
;     A ParseState struct
(define (count-bracket-balance str [initial-state (ParseState 0 (StringState #f #f))])
  (define (process-chars chars state)
    (if (null? chars)
        state  ; Return final state
        (let* ([char (car chars)]
               [rest (cdr chars)]
               [balance (ParseState-balance state)]
               [str-state (ParseState-string-state state)]
               [in-string? (StringState-in-string? str-state)]
               [escaped? (StringState-escaped? str-state)])
          (cond
            ;; Handle escape sequences inside strings
            [(and in-string? escaped?)
             (process-chars rest 
                           (ParseState balance 
                                        (StringState in-string? #f)))]
            
            ;; Handle escape character
            [(and in-string? (char=? char #\\))
             (process-chars rest 
                           (ParseState balance 
                                        (StringState in-string? #t)))]
            
            ;; Handle string delimiters
            [(char=? char #\")
             (process-chars rest 
                           (ParseState balance 
                                        (StringState (not in-string?) #f)))]
            
            ;; Count opening brackets (when not in string)
            [(and (not in-string?) (or (char=? char #\() (char=? char #\[) (char=? char #\{)))
             (process-chars rest 
                           (ParseState (+ balance 1) 
                                        (StringState in-string? #f)))]
            
            ;; Count closing brackets (when not in string)
            [(and (not in-string?) (or (char=? char #\)) (char=? char #\]) (char=? char #\})))
             (process-chars rest 
                           (ParseState (- balance 1) 
                                        (StringState in-string? #f)))]
            
            ;; Skip other characters
            [else
             (process-chars rest 
                           (ParseState balance 
                                        (StringState in-string? #f)))]))))
  
  (process-chars (string->list str) initial-state))

;  input-needs-continuation?
;     Determine whether input needs to continue to the next line
;  Arguments:
;     input - Current input string
;     [initial-state] - Initial parse state (default: balance 0, not in string, not escaped)
;  Returns:
;     Boolean indicating whether more input is needed
(define (input-needs-continuation? input [initial-state (ParseState 0 (StringState #f #f))])
  (let* ([final-state (count-bracket-balance input initial-state)]
         [balance (ParseState-balance final-state)]
         [str-state (ParseState-string-state final-state)]
         [in-string? (StringState-in-string? str-state)])
    (or (> balance 0)     ; Unclosed brackets
        in-string?        ; Unclosed string
        )))

;  read-multi-line-input
;     Read input until bracket balance is zero or negative and all string literals are closed
;  Arguments:
;     initial-line - First line of input
;     [prompt-fn] - Optional function to display continuation prompt (default: no prompt)
;  Returns:
;     Complete multi-line input as a single string
(define (read-multi-line-input initial-line [prompt-fn (lambda () (void))])
  (let loop ([lines (list initial-line)]
             [parse-st (ParseState 0 (StringState #f #f))])
    (let* ([line-state (count-bracket-balance (car lines) parse-st)]
           [new-balance (ParseState-balance line-state)]
           [new-str-state (ParseState-string-state line-state)]
           [new-in-string? (StringState-in-string? new-str-state)])
      (if (and (<= new-balance 0) (not new-in-string?))
          ;; If balance is zero/negative and not in a string, we're done
          (string-join (reverse lines) "\n")
          (begin
            (prompt-fn)
            (let ([next-line (read-line)])
              (cond
                [(eof-object? next-line)
                 (string-join (reverse lines) "\n")]
                [else
                 ;; Continue with next line
                 (loop (cons next-line lines) line-state)])))))))
