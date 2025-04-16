#lang racket

;; Provides location-related functionality for the Eta language
(provide Location
         make-location
         location-equal?
         Location?
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
;;    sline - Starting line number
;;    scol - Starting column number
;;    eline - Ending line number
;;    ecol - Ending column number
(struct Location (sline scol eline ecol) #:transparent)

;; location->string
;;    Converts a location to a string representation
;; Arguments:
;;    loc - A Location object
;; Returns:
;;   A string representation of the location
(define (location->string loc)
  (format "(~a, ~a) - (~a, ~a)"
          (Location-sline loc)
          (Location-scol loc)
          (Location-eline loc)
          (Location-ecol loc)))


;; make-location
;;    Creates a location object for a token
;; Arguments:
;;    line - Starting line number
;;    col - Starting column number
;;    end-col - Ending column number (same line)
;; Returns:
;;    A Location struct
(define (make-location line col end-col)
  (Location line col line end-col))

;; location-equal?
;;    Checks if two locations are equal
;; Arguments:
;;    l1 - First location
;;    l2 - Second location
;; Returns:
;;    Boolean indicating if the locations are equal
(define (location-equal? l1 l2)
  (and (= (Location-sline l1) (Location-sline l2))
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
  (Location (Location-sline start-loc) 
            (Location-scol start-loc)
            (Location-eline end-loc)
            (Location-ecol end-loc)))


;;  location<? 
;;      Compare whether loc2 comes after loc1.
;;  Arguments:
;;      loc1 - The first Location
;;      loc2 - The second Location
;;  Returns:
;;      #t if loc2 comes after loc1, otherwise #f
(define (location<? loc1 loc2)
  (cond
    [(> (Location-sline loc2) (Location-sline loc1)) #t]
    [(= (Location-sline loc2) (Location-sline loc1))
     (> (Location-scol loc2) (Location-scol loc1))]
    [else #f]))
