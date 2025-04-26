#lang racket

(require "../parser/ast.rkt"
         "../utils/error.rkt"
         "../parser/parser.rkt")

(provide desugar
         parse-and-desugar) ; Integration point with the parser

;  desugar
;     Transform syntactic sugar in the AST to simpler core forms
;  Arguments:
;      expr - The expression to desugar (Expr instance or a primitive value)
;  Returns:
;      A desugared expression (Expr instance or a primitive value)
;  Example:
;      (desugar (make-expr Cond [test1 expr1 test2 expr2] loc))
;      ; => (make-expr If [test1 expr1 (make-expr If [test2 expr2 #f] loc)] loc)
;  Notes:
;      This is the main entry point for the desugaring process
(define (desugar expr)
  (cond
    ; Process Expr nodes based on their head
    [(Expr? expr)
     (let ([head (Expr-head expr)]
           [args (Expr-args expr)]
           [loc (Expr-loc expr)])
       (match head
         [(== Const) expr]  ; Constants dont need recursion

         [(== Var) expr]    ; Variables dont need recursion

         [(== Nil) expr]    ; Nil doesnt need recursion

         [(== Lambda)
          (make-expr Lambda
                     (list
                      (first args)  ; Parameters stay the same
                      (make-expr Body (map desugar (Expr-args (second args))) loc)) ; Desugar body
                     loc)]

         [(== Quote) expr]  ; Quoted expressions remain as is

         [(== Define)
          (make-expr Define
                     (list (first args) (desugar (second args)))
                     loc)]

         [(== If)
          (make-expr If
                     (map desugar args)
                     loc)]

         [(== Set!)
          (make-expr Set!
                     (list (first args) (desugar (second args)))
                     loc)]

         ; Syntactic forms to be completely transformed
         [(== Cond) (desugar-cond args loc)]
         [(== And) (desugar-and args loc)]
         [(== Or) (desugar-or args loc)]
         [(== NamedLet) (desugar-named-let args loc)]
         [(== UnNamedLet) (desugar-unnamed-let args loc)]
         [(== LetStar) (desugar-let-star args loc)]
         [(== LetRec) (desugar-letrec args loc)]
         [(== Begin) (desugar-begin args loc)]
         [(== Do) (desugar-do args loc)]
         [(== Load) (desugar-load args loc)]

         ; For all other forms, recursively process arguments
         [else (make-expr head (map desugar args) loc)]))]

    ; Process lists recursively
    [(list? expr) (map desugar expr)]

    ; Pass through primitive values
    [else expr]))

;  desugar-cond
;     Transform a Cond expression into nested If expressions
;  Arguments:
;      args - Arguments of the Cond expression (list of CondClause expressions)
;      loc - Source location information
;  Returns:
;      An equivalent expression using If
;  Example:
;      (desugar-cond (list clause1 clause2 elseclause) loc)
;      ; => (if test1 expr1 (if test2 expr2 else-expr))
;  Notes:
;      This handles the special 'else' clause correctly.
;      Empty cond expressions return #void.
(define (desugar-cond args loc)
  (cond
    ; (cond) => #void
    [(null? args)
     (make-expr Const (list 'Void) loc)]

    [else
     ; Process the first clause
     (let ([first-clause (car args)])
       ; Verify it's a CondClause
       (unless (and (Expr? first-clause) (eq? (Expr-head first-clause) CondClause))
         (error 'desugar-cond "Expected a CondClause, got: ~a" first-clause))

       (let ([clause-args (Expr-args first-clause)]
             [clause-loc (Expr-loc first-clause)])

         ; Extract test and expressions from the clause
         (let ([test (car clause-args)]
               [exprs (cdr clause-args)])

           ; Check if this is an 'else' clause (should only be the last one)
           (if (and (eq? test 'else) (not (null? (cdr args))))
               (error 'desugar-cond "'else' clause must be the last clause in cond")

               ; Process based on clause type
               (cond
                 ; Handle 'else' clause
                 [(eq? test 'else)
                  (if (null? exprs)
                      (make-expr Const (list 'Void) loc)
                      ; Create a Begin expression for multiple expressions
                      (if (= (length exprs) 1)
                          (desugar (car exprs))
                          (desugar (make-expr Begin exprs clause-loc))))]

                 ; Handle regular clauses
                 [else
                  ; If there are no expressions, result is the test value
                  (let ([result-expr
                         (if (null? exprs)
                             test  ; (cond [test]) => test
                             (if (= (length exprs) 1)
                                 (car exprs)  ; Single expression
                                 (make-expr Begin exprs clause-loc)))]  ; Multiple expressions

                        [rest-expr
                         (if (null? (cdr args))
                             (make-expr Const (list 'Void) loc)  ; No more clauses
                             (desugar-cond (cdr args) loc))])  ; Process rest of clauses

                    ; Create an If expression
                    (make-expr
                     If
                     (list
                      (desugar test)
                      (desugar result-expr)
                      rest-expr)
                     loc))])))))]))

;  desugar-and
;     Transform an And expression into nested If expressions
;  Arguments:
;      args - Arguments of the And expression (list of expressions)
;      loc - Source location information
;  Returns:
;      An equivalent expression using If
;  Example:
;      (desugar-and (list expr1 expr2) loc)
;      ; => (if expr1 (if expr2 #t #f) #f)
;  Notes:
;      Special cases:
;      - (and) => #t
;      - (and expr) => expr
(define (desugar-and args loc)
  (cond
    [(null? args)
     ; (and) => #t
     (make-expr Const (list 'Bool #t) loc)]
    [(null? (cdr args))
     ; (and expr) => expr
     (desugar (car args))]
    [else
     ; (and expr1 expr2 ...) => (if expr1 (and expr2 ...) #f)
     (let ([first-arg (desugar (car args))]
           [rest-args (cdr args)])
       (make-expr
        If
        (list first-arg
              (desugar-and rest-args loc)
              (make-expr Const (list 'Bool #f) loc))
        loc))]))

;  desugar-or
;     Transform an Or expression into nested If expressions with value capture
;  Arguments:
;      args - Arguments of the Or expression (list of expressions)
;      loc - Source location information
;  Returns:
;      A fully desugared expression using only core forms (If, Lambda, App, etc.)
;  Example:
;      (desugar-or (list expr1 expr2) loc)
;      ; => ((lambda (temp) (if temp temp expr2)) expr1)
;  Notes:
;      Special cases:
;      - (or) => #f
;      - (or expr) => expr
;      This implementation correctly handles short-circuit evaluation
;      and ensures the first truthy value is returned.
(define (desugar-or args loc)
  (cond
    [(null? args)
     ; (or) => #f
     (make-expr Const (list 'Bool #f) loc)]
    [(null? (cdr args))
     ; (or expr) => expr
     (desugar (car args))]
    [else
     ; Generate a temporary variable name
     (let* ([temp-var-name (symbol->string (gensym 'or_temp))]
            [temp-var (make-expr Var (list temp-var-name) loc)]  ; Create Var expression
            [first-arg (desugar (car args))]
            [rest-or (desugar-or (cdr args) loc)])

       ; First, create the intermediate form:
       ; (let ([temp expr1]) (if temp temp (or expr2 ...)))
       (let ([let-expr
              (make-expr
               UnNamedLet
               (list
                ; Bindings: ([temp-var first-arg])
                (make-expr Bindings
                           (list (make-expr Bind (list temp-var first-arg) loc))
                           loc)
                ; Body: (if temp-var temp-var (or rest...))
                (make-expr
                 If
                 (list temp-var
                       temp-var
                       rest-or)
                 loc))
               loc)])

         ; Further desugar the intermediate form to get core forms only
         (desugar let-expr)))]))

;  desugar-unnamed-let
;     Transform an unnamed Let expression into a Lambda application
;  Arguments:
;      args - Arguments of the Let expression:
;             First arg: Bindings expression containing Bind expressions
;             Rest args: Body expressions
;      loc - Source location information
;  Returns:
;      An equivalent Lambda application
;  Example:
;      (desugar-unnamed-let (list bindings body1 body2) loc)
;      ; => ((lambda (vars...) body1 body2) values...)
;  Notes:
;      The standard transformation: (let ([x1 e1] ... [xn en]) body) =>
;      ((lambda (x1 ... xn) body) e1 ... en)
(define (desugar-unnamed-let args loc)
  (unless (>= (length args) 1)
    (error 'desugar-unnamed-let "Let requires bindings and body, got: ~a" args))

  (let ([bindings-expr (car args)]
        [body-exprs (cdr args)])

    ; Verify bindings structure
    (unless (and (Expr? bindings-expr)
                 (eq? (Expr-head bindings-expr) Bindings))
      (error 'desugar-unnamed-let "Expected Bindings expression, got: ~a" bindings-expr))

    ; Extract the individual bindings
    (let* ([bindings (Expr-args bindings-expr)]
           ; Create the parameter list and values list without using for/lists and values
           [params
            (map
             (lambda (binding)
               (unless (and (Expr? binding) (eq? (Expr-head binding) Bind))
                 (error 'desugar-unnamed-let "Expected Bind expression, got: ~a" binding))
               (car (Expr-args binding)))  ; Extract parameter name
             bindings)]

           [vals
            (map
             (lambda (binding)
               (unless (and (Expr? binding) (eq? (Expr-head binding) Bind))
                 (error 'desugar-unnamed-let "Expected Bind expression, got: ~a" binding))
               (desugar (cadr (Expr-args binding))))  ; Extract and desugar value
             bindings)]

           ; Desugar the body expressions
           [desugared-body (map desugar body-exprs)])

      ; Create the lambda application
      (make-expr
       App
       (cons
        ; (lambda (params...) body...)
        (make-expr
         Lambda
         (list
          params ; Parameter list
          (make-expr Body desugared-body loc))
         loc)
        ; Values for the parameters
        vals)
       loc))))

;  desugar-named-let
;     Transform a named Let expression into a Letrec with a Lambda
;  Arguments:
;      args - Arguments of the named Let expression:
;             First arg: name of the procedure (either as a string or as a Var expression)
;             Second arg: Bindings expression containing Bind expressions
;             Rest args: Body expressions
;      loc - Source location information
;  Returns:
;      A fully desugared expression equivalent to named Let
;  Example:
;      (desugar-named-let (list 'loop bindings body1 body2) loc)
;      ; => ((lambda (loop) ((lambda (x) (loop x)) (lambda (vars...) body1 body2))) values...)
;  Notes:
;      The transformation: (let name ([x1 e1] ... [xn en]) body) =>
;      ((letrec ([name (lambda (params...) body)]) name) e1 ... en)
;      And then the letrec itself is further transformed to core forms.
(define (desugar-named-let args loc)
  (unless (>= (length args) 2)
    (error 'desugar-named-let "Named Let requires name, bindings and body, got: ~a" args))

  (let ([raw-proc-name (car args)]
        [bindings-expr (cadr args)]
        [body-exprs (cddr args)])

    ; Extract the actual procedure name string
    (define proc-name
      (cond
        ; If it's already a string, use it directly
        [(string? raw-proc-name) raw-proc-name]
        ; If it's a Var expression, extract the string from it
        [(and (Expr? raw-proc-name) (eq? (Expr-head raw-proc-name) Var))
         (let ([var-args (Expr-args raw-proc-name)])
           (if (and (list? var-args) (not (null? var-args)) (string? (car var-args)))
               (car var-args)
               (error 'desugar-named-let "Invalid loop name in named let: ~a" raw-proc-name)))]
        ; Otherwise, report an error
        [else (error 'desugar-named-let "Invalid loop name in named let: ~a" raw-proc-name)]))

    ; Verify bindings structure
    (unless (and (Expr? bindings-expr)
                 (eq? (Expr-head bindings-expr) Bindings))
      (error 'desugar-named-let "Expected Bindings expression, got: ~a" bindings-expr))

    ; Extract the individual bindings
    (let* ([bindings (Expr-args bindings-expr)]
           ; Create the parameter list and values list without using for/lists and values
           [params
            (map
             (lambda (binding)
               (unless (and (Expr? binding) (eq? (Expr-head binding) Bind))
                 (error 'desugar-named-let "Expected Bind expression, got: ~a" binding))
               (car (Expr-args binding)))  ; Extract parameter name
             bindings)]

           [vals
            (map
             (lambda (binding)
               (unless (and (Expr? binding) (eq? (Expr-head binding) Bind))
                 (error 'desugar-named-let "Expected Bind expression, got: ~a" binding))
               (desugar (cadr (Expr-args binding))))  ; Extract and desugar value
             bindings)]

           ; Desugar the body expressions
           [desugared-body (map desugar body-exprs)])

      ; First, create the letrec + lambda application
      (let ([letrec-expr
             (make-expr
              App
              (cons
               ; (letrec ([name (lambda (params...) body...)]) name)
               (make-expr
                LetRec
                (list
                 ; Bindings - just one for the recursive function
                 (make-expr
                  Bindings
                  (list
                   (make-expr
                    Bind
                    (list
                     (make-expr Var (list proc-name) loc)  ; Use as name
                     ; Lambda function definition
                     (make-expr
                      Lambda
                      (list
                       params ; Parameter list
                       (make-expr Body desugared-body loc))
                      loc))
                    loc))
                  loc)
                 ; Body of letrec - just return the function name
                 (make-expr Var (list proc-name) loc))
                loc)
               ; Values for the parameters
               vals)
              loc)])

        ; Now completely desugar the letrec expression
        ; This ensures we get core forms only (Let, Lambda, App, etc.)
        (desugar letrec-expr)))))

;  desugar-let-star
;     Transform a Let* expression into nested Let expressions
;  Arguments:
;      args - Arguments of the Let* expression:
;             First arg: Bindings expression containing Bind expressions
;             Rest args: Body expressions
;      loc - Source location information
;  Returns:
;      A fully desugared expression using only core forms
;  Example:
;      (desugar-let-star (list bindings body1 body2) loc)
;      ; => ((lambda (var1) ((lambda (var2) body1 body2) val2)) val1)
;  Notes:
;      The transformation: (let* ([x1 e1] [x2 e2] ...) body) =>
;      (let ([x1 e1]) (let ([x2 e2]) ... body))
;      Empty bindings case: (let* () body) => (let () body)
(define (desugar-let-star args loc)
  (unless (>= (length args) 1)
    (error 'desugar-let-star "Let* requires bindings and body, got: ~a" args))

  (let ([bindings-expr (car args)]
        [body-exprs (cdr args)])

    ; Verify bindings structure
    (unless (and (Expr? bindings-expr)
                 (eq? (Expr-head bindings-expr) Bindings))
      (error 'desugar-let-star "Expected Bindings expression, got: ~a" bindings-expr))

    ; Extract the individual bindings
    (let ([bindings (Expr-args bindings-expr)])

      (cond
        ; No bindings, equivalent to (let () body...)
        [(null? bindings)
         (desugar-unnamed-let args loc)]

        ; Just one binding, equivalent to (let ([var val]) body...)
        [(= (length bindings) 1)
         (desugar-unnamed-let args loc)]

        ; Multiple bindings, create nested lets
        [else
         ; First binding
         (let* ([first-binding (car bindings)]
                ; Create a new bindings expr with just the first binding
                [first-bindings-expr
                 (make-expr Bindings (list first-binding) (Expr-loc bindings-expr))]

                ; Create a new let* for remaining bindings
                [remaining-bindings
                 (make-expr Bindings (cdr bindings) (Expr-loc bindings-expr))]
                [inner-let-star
                 (make-expr LetStar
                            (cons remaining-bindings body-exprs)
                            loc)])

           ; Create intermediate form: (let ([var1 val1]) (let* ([var2 val2]...) body...))
           ; and then fully desugar it to core forms
           (desugar
            (make-expr
             UnNamedLet
             (list first-bindings-expr inner-let-star)
             loc)))]))))

;  desugar-letrec
;     Transform a Letrec expression into a Let with Set! expressions
;  Arguments:
;      args - Arguments of the Letrec expression:
;             First arg: Bindings expression containing Bind expressions
;             Rest args: Body expressions
;      loc - Source location information
;  Returns:
;      A fully desugared expression using Let, Lambda, App and Set!
;  Example:
;      (desugar-letrec (list bindings body1 body2) loc)
;      ; => ((lambda (var1 var2) (set! var1 val1) (set! var2 val2) body1 body2) undefined undefined)
;  Notes:
;      The transformation: (letrec ([x1 e1] ... [xn en]) body) =>
;      (let ([x1 undefined] ... [xn undefined])
;        (set! x1 e1) ... (set! xn en) body)
;      An undefined placeholder is used for initial values.
(define (desugar-letrec args loc)
  (unless (>= (length args) 1)
    (error 'desugar-letrec "Letrec requires bindings and body, got: ~a" args))

  (let ([bindings-expr (car args)]
        [body-exprs (cdr args)])

    ; Verify bindings structure
    (unless (and (Expr? bindings-expr)
                 (eq? (Expr-head bindings-expr) Bindings))
      (error 'desugar-letrec "Expected Bindings expression, got: ~a" bindings-expr))

    ; Extract the individual bindings
    (let* ([bindings (Expr-args bindings-expr)]
           ; Create initial let bindings with undefined values
           [init-bindings
            (map
             (lambda (binding)
               (unless (and (Expr? binding) (eq? (Expr-head binding) Bind))
                 (error 'desugar-letrec "Expected Bind expression, got: ~a" binding))

               (let ([binding-args (Expr-args binding)])
                 ; Create a binding with undefined value
                 (make-expr Bind
                            (list (car binding-args)
                                  (make-expr Const (list 'Undefined) loc))
                            (Expr-loc binding))))
             bindings)]

           ; Create set! expressions for each binding
           [set-exprs
            (map
             (lambda (binding)
               (let* ([binding-args (Expr-args binding)]
                      [var (car binding-args)]
                      [val (cadr binding-args)])
                 ; Create a set! expression
                 (make-expr Set!
                            (list var (desugar val))
                            (Expr-loc binding))))
             bindings)]

           ; Initial let bindings expression
           [init-bindings-expr
            (make-expr Bindings init-bindings (Expr-loc bindings-expr))]

           ; All expressions in the body: set! expressions followed by original body
           [all-body-exprs (append set-exprs (map desugar body-exprs))]

           ; Create the let expression for transformation to core forms
           [let-expr (if (null? all-body-exprs)
                         ; Empty body case - just return void
                         (make-expr Const (list 'Void) loc)
                         ; Normal case with set! and body expressions
                         (if (= (length all-body-exprs) 1)
                             ; One body expression - no need for begin
                             (make-expr UnNamedLet
                                        (list init-bindings-expr (car all-body-exprs))
                                        loc)
                             ; Multiple body expressions - wrap in begin
                             (make-expr UnNamedLet
                                        (list
                                         init-bindings-expr
                                         (make-expr Begin all-body-exprs loc))
                                        loc)))])

      ; Apply another round of desugaring to get only core forms
      (desugar let-expr))))

;  desugar-begin
;     Transform a Begin expression into a Lambda application
;  Arguments:
;      args - Arguments of the Begin expression (list of expressions)
;      loc - Source location information
;  Returns:
;      An equivalent Lambda application or the single expression if there's only one
;  Example:
;      (desugar-begin (list expr1 expr2) loc)
;      ; => ((lambda () expr1 expr2))
;  Notes:
;      Special cases:
;      - (begin) => #void
;      - (begin expr) => expr
(define (desugar-begin args loc)
  (cond
    [(null? args)
     ; (begin) => #void
     (make-expr Const (list 'Void) loc)]
    [(null? (cdr args))
     ; (begin expr) => expr
     (desugar (car args))]
    [else
     ; (begin expr1 expr2 ...) => ((lambda () expr1 expr2 ...))
     (let ([desugared-body (map desugar args)])
       (make-expr
        App
        (list
         ; (lambda () expr1 expr2 ...)
         (make-expr
          Lambda
          (list
           '() ; Empty parameter list
           (make-expr Body desugared-body loc))
          loc)
         ; No arguments
         )
        loc))]))

;  desugar-do
;     Transform a Do expression into a named Let expression
;  Arguments:
;      args - Arguments of the Do expression:
;             First arg: DoLet expression (variable bindings with init and step)
;             Second arg: DoFinal expression (test and result expressions)
;             Rest args: Body expressions to execute each iteration
;      loc - Source location information
;  Returns:
;      A fully desugared expression using core forms only
;  Example:
;      (desugar-do (list dolet dofinal body1 body2) loc)
;      ; => ((lambda (loop) ...) (lambda (var1...) ...))
;  Notes:
;      The transformation: (do ([var init step] ...) (test result ...) body ...) =>
;      (let loop ([var init] ...)
;        (if test
;            (begin result ...)
;            (begin body ... (loop step ...))))
;      And then the named let is further desugared to core forms.
(define (desugar-do args loc)
  (unless (>= (length args) 2)
    (error 'desugar-do "Do requires bindings, test/result clause, and optional body, got: ~a" args))

  (let ([dolet-expr (car args)]
        [dofinal-expr (cadr args)]
        [body-exprs (cddr args)])

    ; Verify DoLet and DoFinal structure
    (unless (and (Expr? dolet-expr) (eq? (Expr-head dolet-expr) DoLet))
      (error 'desugar-do "Expected DoLet expression, got: ~a" dolet-expr))

    (unless (and (Expr? dofinal-expr) (eq? (Expr-head dofinal-expr) DoFinal))
      (error 'desugar-do "Expected DoFinal expression, got: ~a" dofinal-expr))

    ; Extract bindings from DoLet
    (let ([bindings (Expr-args dolet-expr)])
      ; Each binding is [var init step]

      ; Extract test and result expressions from DoFinal
      (let* ([final-args (Expr-args dofinal-expr)]
             [test (car final-args)]
             [result-exprs (cdr final-args)]

             ; Generate a loop name
             [loop-name (symbol->string (gensym 'do_loop))]
             [loop-var (make-expr Var (list loop-name) loc)]

             ; Extract vars, inits, and steps from bindings
             [vars-inits-steps
              (map
               (lambda (binding)
                 ; Each binding should be a list of [var init step]
                 (unless (and (list? binding) (>= (length binding) 2))
                   (error 'desugar-do "Invalid binding in do: ~a" binding))

                 (let* ([var (car binding)]
                        [init (cadr binding)]
                        ; If step is missing, use var as step
                        [step (if (= (length binding) 2) var (caddr binding))])
                   (list var init step)))
               bindings)]

             ; Extract the vars and inits for the named let
             [vars (map car vars-inits-steps)]
             [inits (map cadr vars-inits-steps)]

             ; Extract steps for the recursive call
             [steps (map caddr vars-inits-steps)]

             ; Create the result expression (either a single expr or a begin)
             [result-expr
              (if (null? result-exprs)
                  (make-expr Const (list 'Void) loc)  ; No result expressions
                  (if (= (length result-exprs) 1)
                      (car result-exprs)  ; Single result expression
                      (make-expr Begin result-exprs loc)))] ; Multiple result expressions

             ; Create a list of desugared step expressions for the recursive call
             [desugared-steps (map desugar steps)]

             ; Create the loop body
             [loop-body
              (make-expr
               If
               (list
                (desugar test)  ; Test expression
                (desugar result-expr)  ; Result if test is true
                ; Else: body followed by recursive call
                (make-expr
                 Begin
                 (append
                  (map desugar body-exprs)  ; Body expressions
                  (list
                   ; Recursive call with step values
                   (make-expr
                    App
                    (cons
                     loop-var  ; Call to loop using the Var node
                     desugared-steps)              ; Step values as individual arguments
                    loc)))
                 loc))
               loc)]

             ; Create the bindings for the named let
             [let-bindings
              (map
               (lambda (var init)
                 (make-expr Bind (list var (desugar init)) loc))
               vars inits)]

             ; Create the bindings expression
             [bindings-expr (make-expr Bindings let-bindings loc)])

        ; First create the named let expression
        (let ([named-let-expr
               (make-expr
                NamedLet
                (list
                 loop-name  ; Use the string name for the loop
                 bindings-expr
                 loop-body)
                loc)])

          ; Then completely desugar it to ensure we get only core forms
          (desugar named-let-expr))))))

;  desugar-load
;     Transform a Load expression into a primitive-load function call
;  Arguments:
;      args - Arguments of the Load expression (typically a single filename)
;      loc - Source location information
;  Returns:
;      A fully desugared expression using App and Var
;  Example:
;      (desugar-load (list "file.scm") loc)
;      ; => (primitive-load "file.scm")
;  Notes:
;      This preserves proper error reporting for invalid argument counts
(define (desugar-load args loc)
  ; Verify argument count
  (unless (= (length args) 1)
    (error 'desugar-load "Load requires exactly one argument (filename), got: ~a" (length args)))

  ; Transform to function call: (primitive-load filename)
  (make-expr
   App
   (list
    ; Function name: primitive-load
    (make-expr Var (list "primitive-load") loc)
    ; Argument: filename (desugared)
    (desugar (car args)))
   loc))

;  parse-and-desugar
;     Parse input tokens and then desugar the resulting AST
;  Arguments:
;      tokens - The token stream to parse
;  Returns:
;      A desugared AST or an error
;  Example:
;      (parse-and-desugar (tokenize "(and #t #f)"))
;      ; => A desugared expression equivalent to (if #t #f #f)
(define (parse-and-desugar tokens)
  (let ([parsed (parse tokens)])
    (if (EtaError? parsed)
        parsed  ; If there was a parsing error, just return it
        (desugar parsed))))  ; Otherwise, desugar the result

