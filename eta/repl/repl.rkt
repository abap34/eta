#lang racket

(provide init-repl)

(require "../utils/error.rkt"
         "../utils/console.rkt"
         "../eval/interp-interface.rkt")

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
  (displayln (bold " REPL! ✨")))

(define (prompt)
  (display (colorize "eta> " 'green)))

;  repl-loop
;     Simple Read-Eval-Print Loop for eta.
;     Currently only parses and pretty prints the result, without evaluation.
;  Arguments:
;      env - The environment in which to evaluate expressions.
;      history - REPL history store
;  Returns:
;      Never returns (loops indefinitely).
(define (repl-loop env history)
  (prompt)
  (let ([input (read-line)])
    (cond
      [(eof-object? input)
       (displayln (colorize "\nGoodbye!" 'yellow))
       (exit)]
      [(string=? input "")
       (repl-loop env history)]
      [else 
       ;; Add input to history and get its index
       (let-values ([(new-history history-index) (repl-history-add history input)])
         (with-clean-break-state
          (lambda ()
            (with-handlers ([exn:break? (lambda (e)
                                          (displayln (colorize "Evaluation interrupted." 'yellow)))])
              ;; Pass REPL history info as location identifier
              ;; Create a closure to retrieve source code
              (displayln (format-eval-result 
                          (eta-eval-in-thread env input (list 'repl-history history-index))
                          (lambda (file-id) (get-source-from-identifier new-history file-id)))))))
         (reset-break-handler)
         (repl-loop env new-history))])))

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
