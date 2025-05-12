#lang racket

(require "../utils/location.rkt"
         "../utils/error.rkt")

(provide tokenize 
         Token 
         Token-typ 
         Token-val 
         Token-loc
         Token? 
         TokenType?
         Token-loc
         Token-typ
         Token-val
         tokens-span
         format-token
         eta-keyword?
)

(define (TokenType? typ)
  (or (equal? typ 'LParenToken)
      (equal? typ 'RParenToken)
      (equal? typ 'DotSymToken)
      (equal? typ 'QuoteSymToken)
      (equal? typ 'BoolToken)
      (equal? typ 'IntToken)
      (equal? typ 'FloatToken)
      (equal? typ 'StringToken)
      (equal? typ 'IdToken)
      (equal? typ 'KeywordToken)
      (equal? typ 'EOFToken)))


;; Token
;;    Represents a token in the source code
;; Arguments:
;;    typ - TokenType 
;;    val - StringToken value of the token
;;    loc - Location object with position information
(struct Token (typ val loc) #:transparent)

(define (make-token typ val loc)
  (unless (TokenType? typ)
      (error 'Token "expected a TokenType value for typ, got: ~v" typ))
  (cond
      [(equal? typ 'LParenToken) (unless (equal? val "(") (error 'Token "LParen token must have value '(', got: ~v" val))]
      [(equal? typ 'RParenToken) (unless (equal? val ")") (error 'Token "RParen token must have value ')', got: ~v" val))]
      [(equal? typ 'DotSymToken) (unless (equal? val ".") (error 'Token "DotSym token must have value '.', got: ~v" val))]
      [(equal? typ 'QuoteSymToken) (unless (equal? val "'") (error 'Token "QuoteSym token must have value ''', got: ~v" val))]
      [(equal? typ 'BoolToken) (unless (or (equal? val "#t") (equal? val "#f")) (error 'Token "Bool token must have value '#t' or '#f', got: ~v" val))]
      [(equal? typ 'EOFToken) (unless (equal? val "") (error 'Token "EOF token must have empty value, got: ~v" val))])
    (Token typ val loc))

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
  (cond
    [(equal? typ 'RParenToken)     "RParen"]
    [(equal? typ 'LParenToken)     "LParen"]
    [(equal? typ 'DotSymToken)     "DotSym"]
    [(equal? typ 'QuoteSymToken)   "QuoteSym"]
    [(equal? typ 'BoolToken)       "Bool"]
    [(equal? typ 'IntToken)        "Num"]
    [(equal? typ 'FloatToken)      "Float"]
    [(equal? typ 'StringToken)     "StringToken"]
    [(equal? typ 'IdToken)         "Id"]
    [(equal? typ 'KeywordToken)    "Keyword"]
    [(equal? typ 'EOFToken)        "EOF"]
    [else (error 'TokenType->name  "Unknown TokenType: ~v" typ)]))


;; List of eta language keywords
(define eta-keywords
  '("define" "lambda" "quote" "set!" "let" "let*" "letrec"
    "if" "cond" "else" "and" "or" "begin" "do" "load"))

;; keyword?
;;   Check if a string is an eta language keyword
;; Arguments:
;;   s - String to check
;; Returns:
;;   #t if s is a keyword, #f otherwise
(define (eta-keyword? s)
  (member s eta-keywords))

(define (format-token token)
  (format "~a: ~a at ~a"
          (TokenType->name (Token-typ token))
          (Token-val token)
          (location->string (Token-loc token))))

(define (tokenize src)
  (unless (string? src)
    (error 'tokenize "expected a string, got: ~v" src))

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
           (lexeme (substring src pos end))
           ;; Check if the lexeme is a keyword
           (token-type (if (eta-keyword? lexeme) 'KeywordToken 'IdToken)))
      (values (make-token token-type lexeme line col line (+ col (- end pos)))
              end line (+ col (- end pos)))))

  ;; read-number
  ;;   Read a number literal (integer or floating-point) from the source code
  ;;   Handles both positive and negative numbers
  ;; Arguments:
  ;;   pos - Current position in the source string
  ;;   line - Current line number
  ;;   col - Current column number
  ;; Returns:
  ;;   A NumToken for integers or FloatToken for floating-point numbers,
  ;;   along with the updated position, line, and column
  (define (read-number pos line col)
    (define (read-digits p)
      (if (and (< p len) (char-numeric? (get-at p)))
          (read-digits (+ p 1))
          p))
    
    ; Check if the number starts with a minus sign
    (define is-negative (and (< pos len) (char=? (get-at pos) #\-)))
    
    ; Adjust the starting position if we have a negative sign
    (define num-start (if is-negative (+ pos 1) pos))
    
    (let* ((int-end (read-digits num-start))
           ; If we moved past the starting digit position, we have valid digits
           (has-valid-digits (> int-end num-start))
           ; Only treat as negative number if digits follow the minus sign
           (effective-pos (if (and is-negative has-valid-digits) pos num-start))
           (has-decimal (and has-valid-digits (< int-end len) (char=? (get-at int-end) #\.)))
           (decimal-start (if has-decimal (+ int-end 1) int-end))
           (end (if has-decimal (read-digits decimal-start) int-end))
           ; Only proceed with number parsing if we have valid digits
           (lexeme (if has-valid-digits (substring src effective-pos end) (substring src pos pos)))
           (token-type (if has-decimal 'FloatToken 'IntToken)))
      
      ; If no valid digits were found after a minus sign, treat as symbol instead
      (if (and is-negative (not has-valid-digits))
          (read-symbol pos line col)  ; Fall back to symbol parsing
          (values (make-token token-type lexeme line col line (+ col (- end effective-pos)))
                end line (+ col (- end effective-pos))))))

  (define (read-string pos line col)
    (define (loop p acc l c)
      (if (>= p len)
          (values (make-tokenize-error "Unterminated string literal" (make-location line col col)) p l c)
          (let ((ch (get-at p)))
            (cond
              [(char=? ch #\")
               (values (make-token 'StringToken (list->string (reverse acc)) line col l (+ c 1))
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
             (values (make-token 'BoolToken "#t" line col line (+ col 2))
                     (+ pos 2) line (+ col 2))]
            [(char=? ch #\f)
             (values (make-token 'BoolToken "#f" line col line (+ col 2))
                     (+ pos 2) line (+ col 2))]
            [else
             (values (make-tokenize-error 
                     (format "Invalid boolean literal: #~a" ch)
                     (make-location line col col)
                     )
                     (+ pos 2) line (+ col 2))]))))

  (define (read-next-token pos line col)
    (if (finish? pos)
        (values (make-token 'EOFToken "" line col line col) pos line col)
        (let-values (((pos1 line1 col1 ch) (advance pos line col)))
          (cond
            [(char-whitespace? ch)
             (read-next-token pos1 line1 col1)]
            [(char=? ch #\() (values (make-token 'LParenToken "(" line col line col1) pos1 line1 col1)]
            [(char=? ch #\)) (values (make-token 'RParenToken ")" line col line col1) pos1 line1 col1)]
            [(char=? ch #\.) (values (make-token 'DotSymToken "." line col line col1) pos1 line1 col1)]
            [(char=? ch #\') (values (make-token 'QuoteSymToken "'" line col line col1) pos1 line1 col1)]
            [(char=? ch #\#) (read-bool pos line col)]
            [(char=? ch #\") (read-string pos line col)]
            [(char-numeric? ch) (read-number pos line col)]
            ;; Handle negative numbers: check if current char is - and next is a digit
            [(and (char=? ch #\-) 
                  (< pos1 len) 
                  (char-numeric? (get-at pos1)))
             (read-number pos line col)]
            [(symbol-char? ch) (read-symbol pos line col)]
            [else (values (make-tokenize-error 
                          (format "Unexpected character: ~a" ch)
                          (make-location line col col))
                          pos1 line1 col1)]))))

  (define (tokenize-loop pos line col tokens-acc errors-acc)
    (let-values (((token-or-error pos* line* col*) (read-next-token pos line col)))
      (cond
        [(EtaError? token-or-error) 
         (if (null? errors-acc)
             token-or-error  ; Return the first error
             (car errors-acc))]
        [(eq? (Token-typ token-or-error) 'EOFToken)
         (if (null? errors-acc)
             (reverse (cons token-or-error tokens-acc))  ; Include the EOF token in the result
             (car errors-acc))]
        [else
         (tokenize-loop pos* line* col* 
                       (cons token-or-error tokens-acc)
                       errors-acc)])))

  (tokenize-loop 0 1 1 '() '()))


