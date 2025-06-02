#lang racket

(provide repl-history-store
         repl-history-create
         repl-history-add
         repl-history-get
         get-source-from-identifier
         clear-history
         show-history)

(require "../utils/console.rkt")

;; Vector-based history store for REPL
(struct repl-history-store (vector index count) #:transparent)

;  repl-history-create
;     Create a new REPL history store
;  Arguments:
;     [capacity] - Maximum capacity of the history (default: 100)
;  Returns:
;     A new REPL history store
(define (repl-history-create [capacity 100])
  (repl-history-store (make-vector capacity #f) 0 0))

;  repl-history-add
;     Add an input to the REPL history
;  Arguments:
;     history - REPL history store
;     input - Input string
;  Returns:
;     A pair of updated REPL history store and history index (1-based)
(define (repl-history-add history input)
  (let* ([vector (repl-history-store-vector history)]
         [index (repl-history-store-index history)]
         [count (repl-history-store-count history)]
         [capacity (vector-length vector)])
    (vector-set! vector index input)
    (let ([new-index (modulo (add1 index) capacity)]
          [new-count (add1 count)])
      (values (repl-history-store vector new-index new-count) new-count))))

;  repl-history-get
;     Get an input from the REPL history
;  Arguments:
;     history - REPL history store
;     index - History index (1-based)
;  Returns:
;     Input string or #f
(define (repl-history-get history index)
  (let* ([vector (repl-history-store-vector history)]
         [count (repl-history-store-count history)]
         [capacity (vector-length vector)])
    (if (and (>= index 1) (<= index count))
        (let* ([real-idx (modulo (- (+ count index) 1) capacity)])
          (vector-ref vector real-idx))
        #f)))

;  get-source-from-identifier
;     Get source code from a file identifier
;  Arguments:
;     history - REPL history store
;     file-id - File identifier (string, symbol, or list)
;  Returns:
;     Source code (string) or #f
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

;  clear-history
;     Clear REPL history and return a new empty history store
;  Arguments:
;     history - Current REPL history store
;  Returns:
;     A new empty REPL history store
(define (clear-history history)
  (displayln (colorize "History cleared." 'green))
  (repl-history-create (vector-length (repl-history-store-vector history))))

;  show-history
;     Display REPL command history
;  Arguments:
;     history - REPL history store
;  Returns:
;     void (prints to stdout)
(define (show-history history)
  (let ([count (repl-history-store-count history)])
    (if (= count 0)
        (displayln (colorize "History is empty." 'yellow))
        (let loop ([i 1])
          (when (<= i count)
            (let ([entry (repl-history-get history i)])
              (when entry
                (displayln (format "~a: ~a" 
                                  (colorize (number->string i) 'cyan)
                                  (if (> (string-length entry) 60)
                                      (string-append (substring entry 0 57) "...")
                                      entry))))
              (loop (add1 i))))))))
