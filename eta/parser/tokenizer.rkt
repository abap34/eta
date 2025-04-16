#lang racket

(require rebellion/type/enum)
(require "../utils/location.rkt")
(require "../utils/error.rkt")

(provide tokenize Token Token-typ Token-val Token-loc
         Token? 
         TokenType
         TokenType?
         Token-loc
         Token-typ
         Token-val
         LParen RParen DotSym QuoteSym
         Bool Num String 
         tokens-span
         format-token
         Id EOF)

(define-enum-type TokenType (LParen RParen DotSym QuoteSym Bool Num String Id EOF))

;; Token
;;    Represents a token in the source code
;; Arguments:
;;    typ - TokenType enum value
;;    val - String value of the token
;;    loc - Location object with position information
(struct Token (typ val loc)
  #:transparent
  #:guard (lambda (typ val loc name)
            (unless (TokenType? typ)
              (error 'Token "expected a TokenType value for typ, got: ~v" typ))
            ;; Check consistency between token type and its value
            (case typ
              [(== LParen) (unless (equal? val "(")
                          (error 'Token "LParen token must have value '(', got: ~v" val))]
              [(== RParen) (unless (equal? val ")")
                          (error 'Token "RParen token must have value ')', got: ~v" val))]
              [(== DotSym) (unless (equal? val ".")
                           (error 'Token "DotSym token must have value '.', got: ~v" val))]
              [(== QuoteSym) (unless (equal? val "'")
                             (error 'Token "QuoteSym token must have value ''', got: ~v" val))]
              [(== Bool) (unless (or (equal? val "#t") (equal? val "#f"))
                         (error 'Token "Bool token must have value '#t' or '#f', got: ~v" val))]
              [(== EOF) (unless (equal? val "")
                        (error 'Token "EOF token must have empty value, got: ~v" val))]
              [else (void)]) ; No specific checks for Num, String, and Id
            (values typ val loc)))

(define (tokens-span tokens)
  (if (empty? tokens)
      (Location 0 0 0 0)
      (let ([first-token (first tokens)]
            [last-token (last tokens)])
        (if (and first-token last-token)
            (create-span-location (Token-loc first-token) (Token-loc last-token))
            (if first-token
                (Token-loc first-token)
                (Location 0 0 0 0))))))

(define (TokenType->name typ)
  (match typ
    [(== RParen)  "RParen"]
    [(== LParen)  "LParen"]
    [(== DotSym)  "DotSym"]
    [(== QuoteSym) "QuoteSym"]
    [(== Bool)     "Bool"]
    [(== Num)      "Num"]
    [(== String)   "String"]
    [(== Id)       "Id"]
    [(== EOF)      "EOF"]
    [_ (error 'TokenType->name "Unknown TokenType: ~v" typ)]))


(define (format-token token)
  (format "~a: ~a at ~a"
          (TokenType->name (Token-typ token))
          (Token-val token)
          (location->string (Token-loc token))))

(define (tokenize src)
  (define len (string-length src))

  (define (finish? pos) (= pos len))
  (define (get-at pos) (string-ref src pos))

  ;; advance 1 character and return new pos, line, col, char
  (define (advance pos line col)
    (let ((c (get-at pos)))
      (values (+ pos 1)
              (if (char=? c #\newline) (+ line 1) line)
              (if (char=? c #\newline) 1 (+ col 1))
              c)))

  (define (symbol-char? c)
    (or (char-alphabetic? c)
        (char-numeric? c)
        (member c '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/
                        #\< #\= #\> #\? #\@ #\^ #\_))))

  (define (make-token typ val sline scol eline ecol)
    (Token typ val (Location sline scol eline ecol)))

  (define (read-symbol pos line col)
    (define (loop p)
      (if (or (>= p len) (not (symbol-char? (get-at p))))
          p
          (loop (+ p 1))))
    (let* ((end (loop pos))
           (lexeme (substring src pos end)))
      (values (make-token Id lexeme line col line (+ col (- end pos)))
              end line (+ col (- end pos)))))

  (define (read-number pos line col)
    (define (loop p)
      (if (and (< p len) (char-numeric? (get-at p)))
          (loop (+ p 1))
          p))
    (let* ((end (loop pos))
           (lexeme (substring src pos end)))
      (values (make-token Num lexeme line col line (+ col (- end pos)))
              end line (+ col (- end pos)))))

  (define (read-string pos line col)
    (define (loop p acc l c)
      (if (>= p len)
          (values (make-tokenize-error "Unterminated string literal" (make-location line col col)) p l c)
          (let ((ch (get-at p)))
            (cond
              [(char=? ch #\")
               (values (make-token String (list->string (reverse acc)) line col l (+ c 1))
                       (+ p 1) l (+ c 1))]
              [(char=? ch #\\)
               (if (>= (+ p 1) len)
                   (values (make-tokenize-error 
                           "Unexpected end after escape character" 
                           (make-location l c c)) 
                           p l c)
                   (let ((next (get-at (+ p 1))))
                     (loop (+ p 2)
                           (cons (case next
                                   [(#\\) #\\] [(#\") #\"] [(#\n) #\newline] [(#\t) #\tab]
                                   [else next])
                                 acc)
                           l (+ c 2))))]
              [(char=? ch #\newline)
               (loop (+ p 1) (cons ch acc) (+ l 1) 1)]
              [else
               (loop (+ p 1) (cons ch acc) l (+ c 1))]))))
    (loop (+ pos 1) '() line (+ col 1)))

  (define (read-bool pos line col)
    (if (>= (+ pos 1) len)
        (values (make-tokenize-error 
                "Unexpected end after '#'" 
                (make-location line col col)) 
                pos line col)
        (let ((ch (get-at (+ pos 1))))
          (cond
            [(char=? ch #\t)
             (values (make-token Bool "#t" line col line (+ col 2))
                     (+ pos 2) line (+ col 2))]
            [(char=? ch #\f)
             (values (make-token Bool "#f" line col line (+ col 2))
                     (+ pos 2) line (+ col 2))]
            [else
             (values (make-tokenize-error 
                     (format "Invalid boolean literal: #~a" ch)
                     (make-location line col col)
                     )
                     (+ pos 2) line (+ col 2))]))))

  (define (read-next-token pos line col)
    (if (finish? pos)
        (values (make-token EOF "" line col line col) pos line col)
        (let-values (((pos1 line1 col1 ch) (advance pos line col)))
          (cond
            [(char-whitespace? ch)
             (read-next-token pos1 line1 col1)]
            [(char=? ch #\() (values (make-token LParen "(" line col line col1) pos1 line1 col1)]
            [(char=? ch #\)) (values (make-token RParen ")" line col line col1) pos1 line1 col1)]
            [(char=? ch #\.) (values (make-token DotSym "." line col line col1) pos1 line1 col1)]
            [(char=? ch #\') (values (make-token QuoteSym "'" line col line col1) pos1 line1 col1)]
            [(char=? ch #\#) (read-bool pos line col)]
            [(char=? ch #\") (read-string pos line col)]
            [(char-numeric? ch) (read-number pos line col)]
            [(symbol-char? ch) (read-symbol pos line col)]
            [else (values (make-tokenize-error 
                          (format "Unexpected character: ~a" ch)
                          (make-location line col col)
                          )
                          pos1 line1 col1)]))))

  (define (tokenize-loop pos line col tokens-acc errors-acc)
    (let-values (((token-or-error pos* line* col*) (read-next-token pos line col)))
      (cond
        [(EtaError? token-or-error) 
         (if (null? errors-acc)
             token-or-error  ; Return the first error
             (car errors-acc))]
        [(eq? (Token-typ token-or-error) EOF)
         (if (null? errors-acc)
             (reverse (cons token-or-error tokens-acc))  ; Include the EOF token in the result
             (car errors-acc))]
        [else
         (tokenize-loop pos* line* col* 
                       (cons token-or-error tokens-acc)
                       errors-acc)])))

  (tokenize-loop 0 1 1 '() '())
)


