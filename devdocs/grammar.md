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
    | Id                                      ; Identifier
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

Num ::= Int                                   ; Integer 
     | Float                                  ; Floating-point number

Int ::= [0-9]+                               ; Integer 
Float ::= [0-9]+.[0-9]*                      ; Floating-point number

Bool ::= #t                                   ; True
     | #f                                     ; False

String ::= "..."                              ; Double-quoted string

Id ::= [0-9A-Za-z!$%&*+-./<=>?@^_]+           ; Identifier 
```
