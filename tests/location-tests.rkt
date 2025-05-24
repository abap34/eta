#lang racket

(require "../eta/utils/location.rkt"
         "../eta/parser/tokenizer.rkt"
         "./test-utils.rkt")

(provide run-location-tests)

;; Test for the Location struct with file information
(define (test-location-file-info state output-fn)
  (output-fn "Running test-location-file-info...")
  
  ;; Test basic location creation with file info
  (let* ([loc (make-location 1 2 3 "test.eta")]
         [expected-file "test.eta"]
         [actual-file (Location-file loc)])
    (set! state (assert-equal actual-file expected-file
                              "Location with file string test"
                              state
                              (make-indented-output-fn output-fn 1))))
  
  ;; Test with symbol file info (for REPL)
  (let* ([loc (make-location 1 2 3 'repl)]
         [expected-file 'repl]
         [actual-file (Location-file loc)])
    (set! state (assert-equal actual-file expected-file
                              "Location with symbol file test"
                              state
                              (make-indented-output-fn output-fn 1))))
  
  ;; Test default (no file info)
  (let* ([loc (make-location 1 2 3)]
         [expected-file #f]
         [actual-file (Location-file loc)])
    (set! state (assert-equal actual-file expected-file
                              "Location without file info test"
                              state
                              (make-indented-output-fn output-fn 1))))
  
  ;; Test location->string with file info
  (let* ([loc (make-location 1 2 3 "test.eta")]
         [str (location->string loc)]
         [expected "test.eta:(1, 2) - (1, 3)"])
    (set! state (assert-equal str expected
                              "location->string with file test"
                              state
                              (make-indented-output-fn output-fn 1))))
  
  ;; Test location->string with REPL symbol
  (let* ([loc (make-location 1 2 3 'repl)]
         [str (location->string loc)]
         [expected "repl:(1, 2) - (1, 3)"])
    (set! state (assert-equal str expected
                              "location->string with repl symbol test"
                              state
                              (make-indented-output-fn output-fn 1))))
  
  ;; Test span creation with matching file info
  (let* ([loc1 (make-location 1 2 3 "test.eta")]
         [loc2 (make-location 4 5 6 "test.eta")]
         [span (create-span-location loc1 loc2)]
         [expected-file "test.eta"])
    (set! state (assert-equal (Location-file span) expected-file
                              "create-span-location with matching files test"
                              state
                              (make-indented-output-fn output-fn 1))))
  
  ;; Test span creation with different file info
  (let* ([loc1 (make-location 1 2 3 "file1.eta")]
         [loc2 (make-location 4 5 6 "file2.eta")]
         [span (create-span-location loc1 loc2)]
         [expected-file "file1.eta"]) ; Should take first file
    (set! state (assert-equal (Location-file span) expected-file
                              "create-span-location with different files test"
                              state
                              (make-indented-output-fn output-fn 1))))
  
  ;; Test tokenizer with file info
  (let* ([input "(define x 10)"]
         [tokens (tokenize input "test.eta")]
         [first-token (car tokens)]
         [expected-file "test.eta"])
    (set! state (assert-equal (Location-file (Token-loc first-token)) expected-file
                              "Tokenizer with file info test"
                              state
                              (make-indented-output-fn output-fn 1))))
  
  state)

;; Run all location tests
(define (run-location-tests state output-fn)
  (output-fn "Running location tests...")
  (let ([child-output-fn (make-indented-output-fn output-fn 1)])
    (with-error-handling
        (lambda ()
          (test-location-file-info state child-output-fn))
      "test-location-file-info"
      state
      child-output-fn)))
