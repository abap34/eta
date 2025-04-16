# eta Language AST Structure Documentation

This document outlines the Abstract Syntax Tree (AST) structure for the eta language implementation. The AST represents the parsed structure of eta programs and is used by the evaluator to execute code.

## AST Node Structure

In eta, all AST nodes share a common structure defined by the `Expr` struct:

```scheme
(struct Expr (head args loc) #:transparent)
```

Where:
- `head`: An `ExprHead` enum value that indicates the type of expression
- `args`: A list of arguments specific to the expression type
- `loc`: Source code location information

## Expression Types

The eta language defines the following expression types via the `ExprHead` enum:

| ExprHead   | Description                                          |
| ---------- | ---------------------------------------------------- |
| Const      | Constant value (number, boolean, string)             |
| Var        | Variable reference                                   |
| App        | Function application                                 |
| Lambda     | Lambda expression (anonymous function)               |
| Quote      | Quoted expression                                    |
| Define     | Definition (variable or function)                    |
| If         | Conditional expression                               |
| Begin      | Sequence of expressions                              |
| UnNamedLet | Local variable binding without name                  |
| NamedLet   | Named local variable binding (for recursion)         |
| LetRec     | Recursive local variable binding                     |
| LetStar    | Sequential local variable binding                    |
| Cond       | Multi-way conditional                                |
| CondClause | Individual clause in a cond expression               |
| And        | Logical AND                                          |
| Or         | Logical OR                                           |
| Set!       | Variable assignment                                  |
| Load       | Load a file                                          |
| Dot        | Dotted pair notation                                 |
| Do         | Loop construct                                       |
| DoLet      | Variable binding in a do loop                        |
| DoFinal    | Final expression in a do loop                        |
| Nil        | Empty list                                           |
| Body       | Body of function/procedure with internal definitions |
| Bind       | Single binding in a let/let*/letrec expression       |
| Bindings   | Collection of bindings                               |
| Arg        | Function argument or argument list                   |

## Args Structure by Expression Type

The following table summarizes the structure of the `args` field for each `ExprHead` type:

| ExprHead   | Args Structure                         | Description                                                                                                      |
| ---------- | -------------------------------------- | ---------------------------------------------------------------------------------------------------------------- |
| Const      | `(list tag value)`                     | `tag`: Symbol ('Num, 'Bool, 'String)<br>`value`: The actual value                                                |
| Var        | `(list name)`                          | `name`: String - name of the variable                                                                            |
| App        | `(list operator args)`                 | `operator`: Expr - function to apply<br>`args`: List of Expr - arguments                                         |
| Lambda     | `(list args body)`                     | `args`: Expr (Arg) - argument pattern<br>`body`: Expr (Body) - function body                                     |
| Quote      | `(list expr)`                          | `expr`: Expr - quoted expression                                                                                 |
| Define     | `(list name value)`                    | `name`: Token or Expr (Var) - variable name<br>`value`: Expr - value                                             |
| If         | `(list has-else? test then [else])`    | `has-else?`: Boolean - #t if else exists<br>`test`: Expr<br>`then`: Expr<br>`else`: Expr (optional)              |
| Begin      | List of expressions                    | List of Expr objects to be evaluated in sequence                                                                 |
| UnNamedLet | `(list bindings body)`                 | `bindings`: Expr (Bindings)<br>`body`: Expr (Body)                                                               |
| NamedLet   | `(list name bindings body)`            | `name`: Token or Expr (Var)<br>`bindings`: Expr (Bindings)<br>`body`: Expr (Body)                                |
| LetRec     | `(list bindings body)`                 | `bindings`: Expr (Bindings)<br>`body`: Expr (Body)                                                               |
| LetStar    | `(list bindings body)`                 | `bindings`: Expr (Bindings)<br>`body`: Expr (Body)                                                               |
| Cond       | `(list has-else? clauses [else-expr])` | `has-else?`: Boolean - #t if else exists<br>`clauses`: List of Expr (CondClause)<br>`else-expr`: Expr (optional) |
| CondClause | `(list test body)`                     | `test`: Expr - condition<br>`body`: List of Expr - expressions if condition is true                              |
| And        | List of expressions                    | List of Expr objects to be AND-ed                                                                                |
| Or         | List of expressions                    | List of Expr objects to be OR-ed                                                                                 |
| Set!       | `(list var value)`                     | `var`: Token or Expr (Var) - variable<br>`value`: Expr - new value                                               |
| Load       | `(list filename)`                      | `filename`: String - path to file                                                                                |
| Do         | `(list vars test body)`                | `vars`: List of Expr (DoLet)<br>`test`: Expr (DoFinal)<br>`body`: Expr (Body)                                    |
| DoLet      | `(list name init step)`                | `name`: Token or Expr (Var)<br>`init`: Expr - initial value<br>`step`: Expr - update expr                        |
| DoFinal    | `(list test result)`                   | `test`: Expr - termination condition<br>`result`: List of Expr - result expressions                              |
| Nil        | Empty list `'()`                       | No arguments                                                                                                     |
| Body       | `(list defines expressions)`           | `defines`: List of Expr (Define)<br>`expressions`: List of Expr - body expressions                               |
| Bind       | `(list name value)`                    | `name`: Token or Expr (Var)<br>`value`: Expr - bound value                                                       |
| Bindings   | List of binding expressions            | List of Expr (Bind) objects                                                                                      |
| Arg        | `(list required-args variadic-args)`   | `required-args`: List of required arguments<br>`variadic-args`: List of variadic args                            |

## Constructor Functions

The parser module provides specialized constructor functions for creating different expression types:

### Constants and Variables

```scheme
(make-const location tag value)
(make-var location name)
(make-nil location)
```

### Function and Application

```scheme
(make-lambda location args body)
(make-app location operator args)
(make-single-arg location name)
(make-list-arg location required-args variadic-args)
```

### Special Forms

```scheme
(make-quote location value)
(make-define location name value)
(make-setbang location name value)
(make-ifthen location test then)
(make-ifthenelse location test then else)
```

### Let Expressions

```scheme
(make-unnamed-let location bindings body)
(make-named-let location name bindings body)
(make-letstar location bindings body)
(make-letrec location bindings body)
(make-bind location name value)
(make-bindings location binds)
```

### Control Flow

```scheme
(make-begin location expressions)
(make-and location expressions)
(make-or location expressions)
(make-cond-clause location test body)
(make-cond-noelse location clauses)
(make-cond-else location clauses else)
```

### Iteration

```scheme
(make-do location var-specs test-expr body-expr)
(make-do-let location name init step)
(make-do-final location cond body)
(make-body location defines expressions)
```

### Other

```scheme
(make-load location filename)
```

## Detailed Examples

### Const

Constants represent literal values like numbers, booleans, and strings.

```scheme
;; Number: 42
(make-const loc 'Num 42)

;; Boolean: #t
(make-const loc 'Bool #t)

;; String: "hello"
(make-const loc 'String "hello")

;; Empty list (using Nil head, not Const)
(make-nil loc)
```

### If Expression

If expressions can have either two or three parts (with or without an else clause).

```scheme
;; Without else: (if (> x 0) "positive")
(make-ifthen 
  loc
  (make-app loc 
            (make-var loc ">")
            (list (make-var loc "x") (make-const loc 'Num 0)))
  (make-const loc 'String "positive"))

;; With else: (if (> x 0) "positive" "negative")
(make-ifthenelse
  loc
  (make-app loc 
            (make-var loc ">")
            (list (make-var loc "x") (make-const loc 'Num 0)))
  (make-const loc 'String "positive")
  (make-const loc 'String "negative"))
```

### Define

Define expressions are used for both variable and function definitions.

```scheme
;; Variable definition: (define x 42)
(make-define 
  loc
  (make-var loc "x")
  (make-const loc 'Num 42))

;; Function definition: (define (add x y) (+ x y))
(make-define
  loc
  (make-var loc "add")
  (make-lambda 
    lambda-loc
    (make-list-arg args-loc 
                  (list (make-var arg1-loc "x")
                        (make-var arg2-loc "y"))
                  '())
    (make-body
      body-loc
      '()  ;; No internal defines
      (list
        (make-app
          expr-loc
          (make-var plus-loc "+")
          (list
            (make-var x-loc "x")
            (make-var y-loc "y")))))))
```

### Let Expressions

Let expressions provide local variable bindings.

```scheme
;; Unnamed let: (let ((x 1) (y 2)) (+ x y))
(make-unnamed-let
  loc
  (make-bindings
    bindings-loc
    (list
      (make-bind bind1-loc (make-var var1-loc "x") (make-const val1-loc 'Num 1))
      (make-bind bind2-loc (make-var var2-loc "y") (make-const val2-loc 'Num 2))))
  (make-body
    body-loc
    '()  ;; No internal defines
    (list
      (make-app
        app-loc
        (make-var plus-loc "+")
        (list 
          (make-var x-loc "x")
          (make-var y-loc "y"))))))

;; Named let: (let loop ((i 0)) (if (< i 10) (loop (+ i 1)) i))
(make-named-let
  loc
  (make-var name-loc "loop")
  (make-bindings
    bindings-loc
    (list
      (make-bind 
        bind-loc 
        (make-var var-loc "i") 
        (make-const val-loc 'Num 0))))
  (make-body
    body-loc
    '()  ;; No internal defines
    (list
      (make-ifthenelse
        if-loc
        (make-app
          test-loc
          (make-var lt-loc "<")
          (list
            (make-var i1-loc "i")
            (make-const ten-loc 'Num 10)))
        (make-app
          then-loc
          (make-var loop-loc "loop")
          (list
            (make-app
              inc-loc
              (make-var plus-loc "+")
              (list
                (make-var i2-loc "i")
                (make-const one-loc 'Num 1)))))
        (make-var else-loc "i")))))
```

### Lambda and Function Arguments

Lambda expressions define anonymous functions with various argument patterns.

```scheme
;; Simple lambda: (lambda (x) (+ x 1))
(make-lambda
  loc
  (make-list-arg
    args-loc
    (list (make-var x-loc "x"))
    '())
  (make-body
    body-loc
    '()  ;; No internal defines
    (list
      (make-app
        app-loc
        (make-var plus-loc "+")
        (list
          (make-var x-loc "x")
          (make-const one-loc 'Num 1))))))

;; Lambda with rest args: (lambda (x . rest) x)
(make-lambda
  loc
  (make-list-arg
    args-loc
    (list (make-var x-loc "x"))  ;; Required args
    (list (make-var rest-loc "rest")))  ;; Variadic args
  (make-body
    body-loc
    '()  ;; No internal defines
    (list (make-var result-loc "x"))))

;; Lambda with single arg: (lambda x x)
(make-lambda
  loc
  (make-single-arg arg-loc "x")
  (make-body
    body-loc
    '()
    (list (make-var result-loc "x"))))
```

### Cond Expression

Conditional expressions with multiple clauses.

```scheme
;; (cond ((< x 0) "negative") ((> x 0) "positive") (else "zero"))
(make-cond-else
  loc
  (list
    (make-cond-clause
      clause1-loc
      (make-app
        test1-loc
        (make-var lt-loc "<")
        (list
          (make-var x1-loc "x")
          (make-const zero1-loc 'Num 0)))
      (list (make-const result1-loc 'String "negative")))
    (make-cond-clause
      clause2-loc
      (make-app
        test2-loc
        (make-var gt-loc ">")
        (list
          (make-var x2-loc "x")
          (make-const zero2-loc 'Num 0)))
      (list (make-const result2-loc 'String "positive"))))
  (list (make-const else-loc 'String "zero")))
```

### Do Loop

Iterative looping construct.

```scheme
;; (do ((i 0 (+ i 1)) (sum 0 (+ sum i))) 
;;     ((= i 10) sum)
;;     (display i))
(make-do
  loc
  (list
    (make-do-let
      i-loc
      (make-var i-name-loc "i")
      (make-const i-init-loc 'Num 0)
      (make-app
        i-step-loc
        (make-var plus1-loc "+")
        (list
          (make-var i-var-loc "i")
          (make-const one-loc 'Num 1))))
    (make-do-let
      sum-loc
      (make-var sum-name-loc "sum")
      (make-const sum-init-loc 'Num 0)
      (make-app
        sum-step-loc
        (make-var plus2-loc "+")
        (list
          (make-var sum-var-loc "sum")
          (make-var i-ref-loc "i")))))
  (make-do-final
    test-loc
    (make-app
      condition-loc
      (make-var eq-loc "=")
      (list
        (make-var i-test-loc "i")
        (make-const limit-loc 'Num 10)))
    (list (make-var result-loc "sum")))
  (make-body
    body-loc
    '()
    (list
      (make-app
        display-loc
        (make-var disp-name-loc "display")
        (list (make-var i-disp-loc "i"))))))
```

### Begin Expression

Sequence of expressions executed in order.

```scheme
;; (begin (display "Hello") (display "World") (+ 1 2))
(make-begin
  loc
  (list
    (make-app
      app1-loc
      (make-var disp1-loc "display")
      (list (make-const str1-loc 'String "Hello")))
    (make-app
      app2-loc
      (make-var disp2-loc "display")
      (list (make-const str2-loc 'String "World")))
    (make-app
      app3-loc
      (make-var plus-loc "+")
      (list
        (make-const one-loc 'Num 1)
        (make-const two-loc 'Num 2)))))
```

### And/Or Expressions

Logical operations with short-circuit evaluation.

```scheme
;; (and (> x 0) (< x 10))
(make-and
  loc
  (list
    (make-app
      test1-loc
      (make-var gt-loc ">")
      (list
        (make-var x1-loc "x")
        (make-const zero-loc 'Num 0)))
    (make-app
      test2-loc
      (make-var lt-loc "<")
      (list
        (make-var x2-loc "x")
        (make-const ten-loc 'Num 10)))))

;; (or (< x 0) (> x 10))
(make-or
  loc
  (list
    (make-app
      test1-loc
      (make-var lt-loc "<")
      (list
        (make-var x1-loc "x")
        (make-const zero-loc 'Num 0)))
    (make-app
      test2-loc
      (make-var gt-loc ">")
      (list
        (make-var x2-loc "x")
        (make-const ten-loc 'Num 10)))))
```

### Quote

Represents quoted expressions.

```scheme
;; 'x
(make-quote
  loc
  (make-var x-loc "x"))

;; '(1 2 3)
(make-quote
  loc
  (make-app
    list-loc
    (make-var list-loc "list")
    (list
      (make-const num1-loc 'Num 1)
      (make-const num2-loc 'Num 2)
      (make-const num3-loc 'Num 3))))
```

## Implementation Notes

1. The AST structure is designed to closely match Scheme's syntactic forms.
2. Some expression types use boolean flags in their args list to indicate variations (e.g., If with/without else).
3. Location information is preserved throughout for error reporting.
4. The parser module provides specialized constructor functions that handle the creation of properly structured AST nodes.
5. Each expression node is built using the generic `make-expr` function with the appropriate ExprHead enum value.
