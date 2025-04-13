Note
Origin: https://github.com/psg-titech/NewcomerProject/blob/master/2025.txt

## Notation

The grammar is specified using the following notation:

- Non-terminal symbols: Begin with uppercase letters (e.g., `Exp`, `Define`)
- Terminal symbols: Lowercase or specific characters (e.g., `(`, `)`, `.`)
- `X*`: Zero or more occurrences of X
- `X+`: One or more occurrences of X
- `[X]`: Optional X (zero or one occurrence)

## Grammar

```
Toplevel ::= Exp
         | Define
         | (load String)                      ; Load file contents

Define ::= (define Id Exp)                    ; Variable definition
       | (define (Id Id* [. Id]) Body)        ; Function definition

Exp ::= Const                                 ; Constant
    | Id                                      ; Variable
    | (lambda Arg Body)                       ; Lambda abstraction
    | (Exp Exp*)                              ; Function application
    | (quote S-Exp)                           ; Quote 
    | ('S-Exp)                                ; Quote shorthand 
    | (set! Id Exp)                           ; Assignment
    | (let [Id] Bindings Body)                ; Let
    | (let* Bindings Body)                    ; Let* (Note 4)
    | (letrec Bindings Body)                  ; Letrec
    | (if Exp Exp [Exp])                      ; Conditional (if)
    | (cond (Exp Exp+)* [(else Exp+)])        ; Conditional (cond) (Note 5)
    | (and Exp*)                              ; Logical AND
    | (or Exp*)                               ; Logical OR
    | (begin Exp*)                            ; Sequential execution
    | (do ((Id Exp Exp)*) (Exp Exp*) Body)    ; Iteration

Body ::= Define* Exp+                         ; Function body

Arg ::= Id                                    ; Single argument
    | (Id* [Id . Id])                         ; Argument list 

Bindings ::= ((Id Exp)*)                      ; Variable bindings

S-Exp ::= Const                               ; S-expression constant
      | Id                                    ; S-expression identifier
      | (S-Exp* [S-Exp . S-Exp])              ; S-expression list

Const ::= Num                                 ; Number
      | Bool                                  ; Boolean
      | String                                ; String
      | ()                                    ; Empty list

Num ::= Decimal                               ; Decimal integer (at minimum)

Bool ::= #t                                   ; True
     | #f                                     ; False

String ::= "..."                              ; Double-quoted string (Note 2)

Id ::= [0-9A-Za-z!$%&*+-./<=>?@^_]+           ; Identifier (Note 3)
```

## Notes

1. Strings support backslash `\` escapes.
2. Identifiers cannot be valid numbers.
3. `let*` is a distinct construct, not a repetition of `let`.
4. `cond` must have at least one clause (including possibly an `else` clause), and at most one `else` clause.
