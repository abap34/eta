#lang racket

;; Provides location-related functionality for the Eta language
(provide Location
         make-dummy-location
         make-location
         location-equal?
         Location?
         Location-file
         Location-sline
         Location-scol
         Location-eline
         Location-ecol
         location->string
         create-span-location
         location<?)

;; Location
;;    Represents a source code location
;; Fields:
;;    file - Source file name (string, symbol for special locations like 'repl, or #f if unknown)
;;    sline - Starting line number
;;    scol - Starting column number
;;    eline - Ending line number
;;    ecol - Ending column number
(struct Location (file sline scol eline ecol) #:transparent)

;; location->string
;;    Converts a location to a string representation
;; Arguments:
;;    loc - A Location object
;; Returns:
;;   A string representation of the location
(define (location->string loc)
  (if (not (Location-file loc))
    "Unknown location"
    (let* ([file (Location-file loc)]
          [file-str (cond
                      [(string? file) (format "~a:" file)]
                      [(symbol? file) (format "~a:" (symbol->string file))]
                      [(and (list? file) (= (length file) 2) (equal? (first file) 'repl-history)) (format "REPL[~a]:" (second file))]
                      [file (format "~a:" file)]
                      [else ""])])
      (format "~a(~a, ~a) - (~a, ~a)"
              file-str
              (Location-sline loc)
              (Location-scol loc)
              (Location-eline loc)
              (Location-ecol loc)))))

;; make-dummy-location
;;   Creates a dummy location object with no file and line/column numbers
(define (make-dummy-location)
  (Location #f 0 0 0 0))


;; make-location
;;    Creates a location object for a token
;; Arguments:
;;    line - Starting line number
;;    col - Starting column number
;;    end-col - Ending column number (same line)
;;    file - Source file name (string, symbol, or #f) (optional)
;; Returns:
;;    A Location struct
(define (make-location line col end-col [file #f])
  (Location file line col line end-col))

;; location-equal?
;;    Checks if two locations are equal
;; Arguments:
;;    l1 - First location
;;    l2 - Second location
;; Returns:
;;    Boolean indicating if the locations are equal
(define (location-equal? l1 l2)
  (and (equal? (Location-file l1) (Location-file l2))
       (= (Location-sline l1) (Location-sline l2))
       (= (Location-scol l1) (Location-scol l2))
       (= (Location-eline l1) (Location-eline l2))
       (= (Location-ecol l1) (Location-ecol l2))))

;; create-span-location
;;    Creates a new location that spans from the start location to the end location
;; Arguments:
;;    start-loc - The starting location
;;    end-loc - The ending location
;; Returns:
;;    A new Location that spans from the start to the end
(define (create-span-location start-loc end-loc)
  (let ([file (cond
                [(and (Location-file start-loc) (Location-file end-loc)
                      (equal? (Location-file start-loc) (Location-file end-loc)))
                 (Location-file start-loc)]
                [(Location-file start-loc) (Location-file start-loc)]
                [(Location-file end-loc) (Location-file end-loc)]
                [else #f])])
    (Location file
              (Location-sline start-loc) 
              (Location-scol start-loc)
              (Location-eline end-loc)
              (Location-ecol end-loc))))


;;  location<? 
;;      Compare whether loc2 comes after loc1.
;;  Arguments:
;;      loc1 - The first Location
;;      loc2 - The second Location
;;  Returns:
;;      #t if loc2 comes after loc1.
;;      Note: Defines "loc1 comes before loc2" as:
;;            - loc1 and loc2 are in same file.
;;            - (loc1's sline is less than loc2's sline) or
;;            - ((loc1's sline is equal to loc2's sline) and (loc1's scol is less than loc2's scol)).
(define (location<? loc1 loc2)
  (if (equal? (Location-file loc1) (Location-file loc2))
      (or (< (Location-sline loc1) (Location-sline loc2))
          (and (= (Location-sline loc1) (Location-sline loc2))
               (< (Location-scol loc1) (Location-scol loc2))))
      #f))
