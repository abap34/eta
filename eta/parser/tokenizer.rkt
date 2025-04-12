#lang racket

(require rebellion/type/enum)
(require "../utils/location.rkt")
(require "../utils/error.rkt")

(provide tokenize Token Token-typ Token-val Token-loc
         TokenType
         LParen RParen Dot QuoteSym
         Bool Num String Keyword
         Id EOF)

(define-enum-type TokenType (LParen RParen Dot QuoteSym Bool Num String Keyword Id EOF))

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
            (values typ val loc)))

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
           (lexeme (substring src pos end))
           (typ (if (member lexeme '("define" "lambda" "let" "let*" "letrec"
                                              "if" "cond" "quote" "set!" "and" "or"
                                              "begin" "do" "load" "else"))
                    Keyword Id)))
      (values (make-token typ lexeme line col line (+ col (- end pos)))
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
                     `(got ,ch))
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
            [(char=? ch #\.) (values (make-token Dot "." line col line col1) pos1 line1 col1)]
            [(char=? ch #\') (values (make-token QuoteSym "'" line col line col1) pos1 line1 col1)]
            [(char=? ch #\#) (read-bool pos line col)]
            [(char=? ch #\") (read-string pos line col)]
            [(char-numeric? ch) (read-number pos line col)]
            [(symbol-char? ch) (read-symbol pos line col)]
            [else (values (make-tokenize-error 
                          (format "Unexpected character: ~a" ch)
                          (make-location line col col)
                          `(char ,ch))
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
             (reverse tokens-acc)
             (car errors-acc))]
        [else
         (tokenize-loop pos* line* col* 
                       (cons token-or-error tokens-acc)
                       errors-acc)])))

  (tokenize-loop 0 1 1 '() '())
)


