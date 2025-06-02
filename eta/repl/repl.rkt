#lang racket

(provide init-repl)

(require "../utils/error.rkt"
         "../utils/console.rkt"
         "../eval/interp-interface.rkt"
         "../eval/env.rkt"
         "../eval/runtime-values.rkt"
         "../eval/builtins.rkt")

;; Vector-based history store for REPL
(struct repl-history-store (vector index count) #:transparent)

;; repl-history-create
;;    Create a new REPL history store
;; Arguments:
;;    [capacity] - Maximum capacity of the history (default: 100)
;; Returns:
;;    A new REPL history store
(define (repl-history-create [capacity 100])
  (repl-history-store (make-vector capacity #f) 0 0))

;; repl-history-add
;;    Add an input to the REPL history
;; Arguments:
;;    history - REPL history store
;;    input - Input string
;; Returns:
;;    A pair of updated REPL history store and history index (1-based)
(define (repl-history-add history input)
  (let* ([vector (repl-history-store-vector history)]
         [index (repl-history-store-index history)]
         [count (repl-history-store-count history)]
         [capacity (vector-length vector)])
    (vector-set! vector index input)
    (let ([new-index (modulo (add1 index) capacity)]
          [new-count (add1 count)])
      (values (repl-history-store vector new-index new-count) new-count))))

;; repl-history-get
;;    Get an input from the REPL history
;; Arguments:
;;    history - REPL history store
;;    index - History index (1-based)
;; Returns:
;;    Input string or #f
(define (repl-history-get history index)
  (let* ([vector (repl-history-store-vector history)]
         [count (repl-history-store-count history)]
         [capacity (vector-length vector)])
    (if (and (>= index 1) (<= index count))
        (let* ([real-idx (modulo (- index 1) capacity)])
          (vector-ref vector real-idx))
        #f)))

;; get-source-from-identifier
;;    Get source code from a file identifier
;; Arguments:
;;    history - REPL history store
;;    file-id - File identifier (string, symbol, or list)
;; Returns:
;;    Source code (string) or #f
(define (get-source-from-identifier history file-id)
  (cond
    ;; Regular file
    [(and (string? file-id) (file-exists? file-id))
     (with-handlers ([exn:fail? (lambda (e) #f)])
       (file->string file-id))]
    ;; REPL history
    [(and (list? file-id) 
          (= (length file-id) 2)
          (eq? (first file-id) 'repl-history)
          (number? (second file-id)))
     (repl-history-get history (second file-id))]
    [else #f]))

(define (banner)
  (display (bold "✨ Welcome to "))
  (display (bold (colorize "eta" 'blue)))
  (displayln (bold " REPL! ✨"))
  (displayln (colorize "Type ':help' for available commands." 'cyan)))

;  count-bracket-balance
;     Count the bracket balance (left - right) in a string
;  Arguments:
;     str - Input string to analyze
;  Returns:
;     Integer representing bracket balance (positive = more left brackets)
(define (count-bracket-balance str)
  (define (count-char char)
    (cond
      [(char=? char #\() 1]
      [(char=? char #\)) -1]
      [(char=? char #\[) 1]
      [(char=? char #\]) -1]
      [(char=? char #\{) 1]
      [(char=? char #\}) -1]
      [else 0]))
  
  (foldl + 0 (map count-char (string->list str))))

;  read-multi-line-input
;     Read input until bracket balance is zero or negative
;  Arguments:
;     initial-line - First line of input
;  Returns:
;     Complete multi-line input as a single string
(define (read-multi-line-input initial-line)
  (define (continuation-prompt)
    ; NOTE: eta> {program star here}, so number of dots is 4
    (display (colorize ".... " 'yellow)))
  
  (let loop ([lines (list initial-line)]
             [balance (count-bracket-balance initial-line)])
    (if (<= balance 0)
        (string-join (reverse lines) "\n")
        (begin
          (continuation-prompt)
          (let ([next-line (read-line)])
            (cond
              [(eof-object? next-line)
               (string-join (reverse lines) "\n")]
              [else
               (let ([new-balance (+ balance (count-bracket-balance next-line))])
                 (loop (cons next-line lines) new-balance))]))))))

(define (prompt)
  (display (colorize "eta> " 'green)))

;  repl-loop
;     Enhanced Read-Eval-Print Loop for eta with command support.
;     Supports both eta expressions and special REPL commands (starting with ':').
;  Arguments:
;      env - The environment in which to evaluate expressions.
;      history - REPL history store
;  Returns:
;      Never returns (loops indefinitely until exit command).
(define (repl-loop env history)
  (prompt)
  (let ([input (read-line)])
    (cond
      [(eof-object? input)
       (displayln (colorize "\nGoodbye!" 'yellow))
       (exit)]
      [(string=? input "")
       (repl-loop env history)]
      ;; Handle REPL commands (starting with ':')
      [(and (> (string-length input) 0) (char=? (string-ref input 0) #\:))
       (let ([result (process-repl-command input env history)])
         (if (car result)
             (repl-loop env (cdr result))
             (exit)))]
      [else 
       ;; Check bracket balance and read multi-line input if needed
       (let* ([complete-input (if (> (count-bracket-balance input) 0)
                                  (read-multi-line-input input)
                                  input)])
         ;; Add input to history and get its index
         (let-values ([(new-history history-index) (repl-history-add history complete-input)])
           (with-clean-break-state
            (lambda ()
              (with-handlers ([exn:break? (lambda (e)
                                            (displayln (colorize "Evaluation interrupted." 'yellow)))])
                ;; Pass REPL history info as location identifier
                ;; Create a closure to retrieve source code
                (displayln (format-eval-result 
                            (eta-eval-in-thread env complete-input (list 'repl-history history-index))
                            (lambda (file-id) (get-source-from-identifier new-history file-id)))))))
           (reset-break-handler)
           (repl-loop env new-history)))])))

;  init-repl
;     Initializes and starts the eta REPL.
;  Arguments:
;      None
;  Returns:
;      Never returns (starts the REPL loop).
(define (init-repl)
  (let ([global-env (init-basic-env)]
        [history (repl-history-create)])
    (banner)
    (repl-loop global-env history)))


;  show-help
;     Display help information for REPL commands
;  Arguments:
;     None
;  Returns:
;     void (prints to stdout)
(define (show-help)
  (displayln (colorize "Available REPL Commands:" 'magenta))
  (displayln (format "  ~a - Show this help message" (colorize ":help" 'yellow)))
  (displayln (format "  ~a - Display current environment" (colorize ":env" 'yellow)))
  (displayln (format "  ~a - Display current environment with built-ins" (colorize ":env-all" 'yellow)))
  (displayln (format "  ~a - Show REPL history" (colorize ":history" 'yellow)))
  (displayln (format "  ~a / ~a - Exit REPL" (colorize ":exit" 'yellow) (colorize ":quit" 'yellow)))
  (newline))


;  clear-history
;     Clear REPL history and return a new empty history store
;  Arguments:
;     history - Current REPL history store
;  Returns:
;     A new empty REPL history store
(define (clear-history history)
  (displayln (colorize "History cleared." 'green))
  (repl-history-create (vector-length (repl-history-store-vector history))))

;  process-repl-command
;     Process REPL commands (starting with ':')
;  Arguments:
;     input - Input string
;     env - Current environment
;     history - Current REPL history store
;  Returns:
;     A pair (continue? . new-history) where continue? indicates whether to continue REPL
(define (process-repl-command input env history)
  (define cmd (string-trim input))
  (cond
    [(or (string=? cmd ":help") (string=? cmd ":h"))
     (show-help)
     (cons #t history)]
    [(string=? cmd ":env")
     (pretty-print-Env env #f)
     (cons #t history)]
    [(string=? cmd ":env-all")
     (pretty-print-Env env #t)
     (cons #t history)]
    [(or (string=? cmd ":exit") (string=? cmd ":quit") (string=? cmd ":q"))
     (displayln (colorize "Goodbye!" 'yellow))
     (cons #f history)]
    [else
     (displayln (format "Unknown command: ~a. Type ':help' for available commands." 
                       (colorize cmd 'red)))
     (cons #t history)]))
