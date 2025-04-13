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

| ExprHead | Description                                          |
| -------- | ---------------------------------------------------- |
| Const    | Constant value (number, boolean, string, empty list) |
| Var      | Variable reference                                   |
| App      | Function application                                 |
| Lambda   | Lambda expression (anonymous function)               |
| Quote    | Quoted expression                                    |
| Define   | Definition (variable or function)                    |
| If       | Conditional expression                               |
| Begin    | Sequence of expressions                              |
| Let      | Local variable binding                               |
| LetRec   | Recursive local variable binding                     |
| LetStar  | Sequential local variable binding                    |
| Cond     | Multi-way conditional                                |
| And      | Logical AND                                          |
| Or       | Logical OR                                           |
| Set!     | Variable assignment                                  |
| Load     | Load a file                                          |
| Dot      | Dotted pair notation                                 |
| Do       | Loop construct                                       |

## Args Structure by Expression Type

The following table summarizes the structure of the `args` field for each `ExprHead` type, including detailed type information:

| ExprHead | Args Structure                           | Description                                                                                                                                      |
| -------- | ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| Const    | `(list tag value)`                       | `tag`: Symbol ('Number, 'Boolean, 'String, 'Nil)<br>`value`: Number/Boolean/String/Empty-List - the actual value                                 |
| Var      | `(list identifier-name)`                 | `identifier-name`: String - name of the variable                                                                                                 |
| App      | `(list operator arg1 arg2 ...)`          | `operator`: Expr - the function to apply<br>`arg1, arg2, ...`: Expr - function arguments                                                         |
| Lambda   | `(list args-expr body-expr)`             | `args-expr`: Expr (App) - argument list or pattern<br>`body-expr`: Expr (usually Begin) - function body                                          |
| Quote    | `(list quoted-expr)`                     | `quoted-expr`: Expr - the quoted expression                                                                                                      |
| Define   | `(list var-expr value-expr)`             | `var-expr`: Expr (Var) - variable/function being defined<br>`value-expr`: Expr - value or function body                                          |
| If       | `(list test-expr then-expr [else-expr])` | `test-expr`: Expr - condition expression<br>`then-expr`: Expr - expression if true<br>`else-expr`: Expr (optional) - expression if false         |
| Begin    | `(list expr1 expr2 ...)`                 | `expr1, expr2, ...`: Expr - sequence of expressions                                                                                              |
| Let      | `(list bindings-expr body-expr)`         | `bindings-expr`: Expr (App) - list of bindings<br>`body-expr`: Expr - body expression                                                            |
| LetRec   | `(list bindings-expr body-expr)`         | Same as Let                                                                                                                                      |
| LetStar  | `(list bindings-expr body-expr)`         | Same as Let                                                                                                                                      |
| Cond     | `(list clause1 clause2 ...)`             | `clause1, clause2, ...`: List where:<br>- First element: Expr - test expression<br>- Rest elements: Expr - result expressions                    |
| And      | `(list expr1 expr2 ...)`                 | `expr1, expr2, ...`: Expr - expressions to be AND-ed                                                                                             |
| Or       | `(list expr1 expr2 ...)`                 | `expr1, expr2, ...`: Expr - expressions to be OR-ed                                                                                              |
| Set!     | `(list var-expr value-expr)`             | `var-expr`: Expr (Var) - variable to assign to<br>`value-expr`: Expr - value to assign                                                           |
| Load     | `(list filename)`                        | `filename`: String - path to file to load                                                                                                        |
| Dot      | `(list car-expr cdr-expr)`               | `car-expr`: Expr - expression before the dot<br>`cdr-expr`: Expr - expression after the dot                                                      |
| Do       | `(list vars-expr test-expr body-expr)`   | `vars-expr`: Expr (App) - variable definitions<br>`test-expr`: Expr (App) - termination test and result<br>`body-expr`: Expr (Begin) - loop body |

### Named Let Special Case

For named let (a special form of let with a name):

```
(make-expr Let (list name-expr bindings-expr body-expr) loc)
```

Where:
- `name-expr`: Expr (Var) - the loop name
- `bindings-expr`: Expr (App) - list of bindings
- `body-expr`: Expr - body expression

### Bindings Structure

Bindings in `Let`, `LetRec`, and `LetStar` have this structure:

```
(make-expr App 
          (list (list var-expr1 val-expr1)
                (list var-expr2 val-expr2)
                ...)
          bindings-loc)
```

Where:
- Each binding is a list of two Expr nodes: a variable and its value
- `var-expr`: Expr (Var) - the variable being bound
- `val-expr`: Expr - the value being bound

### Dotted List Notation

When a function has rest arguments, the `Dot` ExprHead is used to represent the dotted pair:

```
(make-expr Lambda 
          (list (make-expr App 
                          (list 
                                normal-arg1
                                normal-arg2
                                ...
                                (make-expr Dot
                                          (list last-normal-arg rest-arg)
                                          dot-loc))
                          args-loc)
                body-expr)
          lambda-loc)
```

Where:
- `normal-arg1, normal-arg2, ...`: Expr (Var) - regular parameters before the dot
- `last-normal-arg`: Expr (Var) - last regular parameter (if any)
- `rest-arg`: Expr (Var) - parameter after the dot

## Define Expression Args Structure

Define expressions have a more complex structure depending on whether they define a variable or a function. Here's a detailed explanation of their args structure:

### 1. Variable Definition: `(define variable-name value)`

```scheme
(make-expr Define (list var-expr val-expr) loc)
```

Where:
- `var-expr`: Expr (Var) - variable being defined
  - Contains a list with a single string element (the variable name)
- `val-expr`: Expr - value being assigned to the variable

**Example:**
```scheme
;; (define x 42)
(make-expr Define
          (list (make-expr Var (list "x") var-loc)
                (make-expr Const (list 'Number 42) val-loc))
          define-loc)
```

### 2. Function Definition: `(define (function-name param1 param2 ...) body)`

```scheme
(make-expr Define (list func-name-expr lambda-expr) loc)
```

Where:
- `func-name-expr`: Expr (Var) - function name
  - Contains a list with a single string element (the function name)
- `lambda-expr`: Expr (Lambda) - lambda expression for the function body
  - Contains a list with two elements:
    - `args-expr`: Expr (App) - the list of parameters
    - `body-expr`: Expr - the function body

**Example:**
```scheme
;; (define (add x y) (+ x y))
(make-expr Define
          (list (make-expr Var (list "add") func-name-loc)
                (make-expr Lambda
                          (list (make-expr App
                                          (list (make-expr Var (list "x") x-loc)
                                                (make-expr Var (list "y") y-loc))
                                          args-loc)
                                (make-expr App
                                          (list (make-expr Var (list "+") plus-loc)
                                                (make-expr Var (list "x") x-ref-loc)
                                                (make-expr Var (list "y") y-ref-loc))
                                          body-loc))
                          lambda-loc))
          define-loc)
```

### 3. Function Definition with Rest Parameters: `(define (function-name param1 param2 . rest) body)`

```scheme
(make-expr Define (list func-name-expr lambda-expr) loc)
```

Where:
- `func-name-expr`: Same as above
- `lambda-expr`: Expr (Lambda) - lambda expression
  - `args-expr`: Expr (App) - contains list of parameters with a `Dot` ExprHead for rest params
    - Regular parameters: Expr (Var) nodes
    - Rest parameter: Expr (Var) node
  - `body-expr`: Expr - function body

**Example:**
```scheme
;; (define (sum x . rest) (+ x (apply + rest)))
(make-expr Define
          (list (make-expr Var (list "sum") func-name-loc)
                (make-expr Lambda
                          (list (make-expr App
                                          (list (make-expr Var (list "x") x-loc)
                                                (make-expr Dot
                                                          (list (make-expr Var (list "rest") rest-loc))
                                                          dot-loc))
                                          args-loc)
                                (make-expr App
                                          (list (make-expr Var (list "+") plus-loc)
                                                (make-expr Var (list "x") x-ref-loc)
                                                (make-expr App
                                                          (list (make-expr Var (list "apply") apply-loc)
                                                                (make-expr Var (list "+") inner-plus-loc)
                                                                (make-expr Var (list "rest") rest-ref-loc))
                                                          apply-loc))
                                          body-loc))
                          lambda-loc))
          define-loc)
```

## AST Structure by Expression Type

### Const

Represents a constant value.

```
(make-expr Const (list value) loc)
```

Where `value` can be:
- `(list 'Number numeric-value)` for numbers
- `(list 'Boolean boolean-value)` for booleans
- `(list 'String string-value)` for strings
- `(list 'Nil '())` for the empty list

**Examples:**

```scheme
;; Number: 42
(make-expr Const (list 'Number 42) loc)

;; Boolean: #t
(make-expr Const (list 'Boolean #t) loc)

;; String: "hello"
(make-expr Const (list 'String "hello") loc)

;; Empty list: ()
(make-expr Const (list 'Nil '()) loc)
```

### Var

Represents a variable reference.

```
(make-expr Var (list identifier-name) loc)
```

**Example:**

```scheme
;; Variable reference: x
(make-expr Var (list "x") loc)
```

### App

Represents a function application.

```
(make-expr App (list operator arg1 arg2 ...) loc)
```

**Example:**

```scheme
;; Function call: (+ 1 2)
(make-expr App (list 
                (make-expr Var (list "+") op-loc)
                (make-expr Const (list 'Number 1) arg1-loc)
                (make-expr Const (list 'Number 2) arg2-loc))
           app-loc)
```

### Lambda

Represents a lambda expression (anonymous function).

```
(make-expr Lambda (list args-expr body-expr) loc)
```

Where:
- `args-expr`: Either a variable name (for a single argument) or an `App` expression containing the argument list
- `body-expr`: The function body (usually a `Begin` expression if multiple expressions)

**Examples:**

```scheme
;; Lambda with single argument: (lambda (x) (+ x 1))
(make-expr Lambda 
          (list (make-expr App (list (make-expr Var (list "x") arg-loc)) args-loc)
                (make-expr App (list 
                               (make-expr Var (list "+") op-loc)
                               (make-expr Var (list "x") var-loc)
                               (make-expr Const (list 'Number 1) num-loc))
                          body-loc))
          lambda-loc)

;; Lambda with rest arguments: (lambda (x . rest) body)
(make-expr Lambda 
          (list (make-expr App 
                          (list (make-expr Var (list "x") x-loc)
                                (make-expr Dot
                                          (list (make-expr Var (list "rest") rest-loc))
                                          dot-loc))
                          args-loc)
                body-expr)
          lambda-loc)
```

### Quote

Represents a quoted expression.

```
(make-expr Quote (list quoted-expr) loc)
```

**Example:**

```scheme
;; Quoted expression: 'x
(make-expr Quote 
          (list (make-expr Var (list "x") var-loc))
          quote-loc)

;; Quoted list: '(1 2 3)
(make-expr Quote 
          (list (make-expr App 
                          (list (make-expr Const (list 'Number 1) num1-loc)
                                (make-expr Const (list 'Number 2) num2-loc)
                                (make-expr Const (list 'Number 3) num3-loc))
                          list-loc))
          quote-loc)
```

### Define

Represents a definition (variable or function).

```
(make-expr Define (list var-expr val-expr) loc)
```

**Examples:**

```scheme
;; Variable definition: (define x 42)
(make-expr Define 
          (list (make-expr Var (list "x") var-loc)
                (make-expr Const (list 'Number 42) val-loc))
          define-loc)

;; Function definition: (define (f x) (+ x 1))
(make-expr Define 
          (list (make-expr Var (list "f") func-name-loc)
                (make-expr Lambda 
                          (list (make-expr App (list (make-expr Var (list "x") arg-loc)) args-loc)
                                (make-expr App (list 
                                              (make-expr Var (list "+") op-loc)
                                              (make-expr Var (list "x") var-loc)
                                              (make-expr Const (list 'Number 1) num-loc))
                                          body-loc))
                          lambda-loc))
          define-loc)
```

### If

Represents a conditional expression.

```
(make-expr If (list test-expr then-expr [else-expr]) loc)
```

Note: The else expression is optional.

**Examples:**

```scheme
;; If with else: (if (> x 0) "positive" "non-positive")
(make-expr If 
          (list (make-expr App 
                          (list (make-expr Var (list ">") op-loc)
                                (make-expr Var (list "x") var-loc)
                                (make-expr Const (list 'Number 0) zero-loc))
                          test-loc)
                (make-expr Const (list 'String "positive") then-loc)
                (make-expr Const (list 'String "non-positive") else-loc))
          if-loc)

;; If without else: (if (> x 0) "positive")
(make-expr If 
          (list (make-expr App 
                          (list (make-expr Var (list ">") op-loc)
                                (make-expr Var (list "x") var-loc)
                                (make-expr Const (list 'Number 0) zero-loc))
                          test-loc)
                (make-expr Const (list 'String "positive") then-loc))
          if-loc)
```

### Begin

Represents a sequence of expressions.

```
(make-expr Begin (list expr1 expr2 ...) loc)
```

**Example:**

```scheme
;; Begin sequence: (begin (set! x 1) (set! y 2) (+ x y))
(make-expr Begin 
          (list (make-expr Set! 
                          (list (make-expr Var (list "x") x-loc)
                                (make-expr Const (list 'Number 1) val1-loc))
                          set1-loc)
                (make-expr Set! 
                          (list (make-expr Var (list "y") y-loc)
                                (make-expr Const (list 'Number 2) val2-loc))
                          set2-loc)
                (make-expr App 
                          (list (make-expr Var (list "+") plus-loc)
                                (make-expr Var (list "x") var1-loc)
                                (make-expr Var (list "y") var2-loc))
                          sum-loc))
          begin-loc)
```

### Let, LetRec, LetStar

Represents local variable bindings.

```
(make-expr Let (list bindings body-expr) loc)
(make-expr LetRec (list bindings body-expr) loc)
(make-expr LetStar (list bindings body-expr) loc)
```

For named let (a special form of let with a name):

```
(make-expr Let (list name-expr bindings body-expr) loc)
```

Where `bindings` is an `App` expression containing pairs of variable and value expressions.

**Examples:**

```scheme
;; Let: (let ((x 1) (y 2)) (+ x y))
(make-expr Let 
          (list (make-expr App 
                          (list (list (make-expr Var (list "x") x-name-loc)
                                      (make-expr Const (list 'Number 1) x-val-loc))
                                (list (make-expr Var (list "y") y-name-loc)
                                      (make-expr Const (list 'Number 2) y-val-loc)))
                          bindings-loc)
                (make-expr App 
                          (list (make-expr Var (list "+") plus-loc)
                                (make-expr Var (list "x") var1-loc)
                                (make-expr Var (list "y") var2-loc))
                          body-loc))
          let-loc)

;; Named let: (let loop ((i 0)) (if (< i 10) (loop (+ i 1)) i))
(make-expr Let 
          (list (make-expr Var (list "loop") loop-name-loc)
                (make-expr App 
                          (list (list (make-expr Var (list "i") i-name-loc)
                                     (make-expr Const (list 'Number 0) i-val-loc)))
                          bindings-loc)
                (make-expr If 
                          (list (make-expr App 
                                          (list (make-expr Var (list "<") lt-loc)
                                                (make-expr Var (list "i") i-loc)
                                                (make-expr Const (list 'Number 10) ten-loc))
                                          test-loc)
                                (make-expr App 
                                          (list (make-expr Var (list "loop") loop-loc)
                                                (make-expr App 
                                                          (list (make-expr Var (list "+") plus-loc)
                                                                (make-expr Var (list "i") i-again-loc)
                                                                (make-expr Const (list 'Number 1) one-loc))
                                                          add-loc))
                                          then-loc)
                                (make-expr Var (list "i") else-loc))
                          if-loc))
          let-loc)
```

### Cond

Represents a multi-way conditional expression.

```
(make-expr Cond (list (list test-expr result-expr1 result-expr2 ...) ...) loc)
```

For the else clause:

```
... (list (make-expr Var (list "else") else-loc) result-expr1 result-expr2 ...) ...
```

**Example:**

```scheme
;; Cond: (cond ((< x 0) "negative") ((> x 0) "positive") (else "zero"))
(make-expr Cond 
          (list (list (make-expr App 
                               (list (make-expr Var (list "<") lt-loc)
                                     (make-expr Var (list "x") x1-loc)
                                     (make-expr Const (list 'Number 0) zero1-loc))
                               test1-loc)
                      (make-expr Const (list 'String "negative") neg-result-loc))
                (list (make-expr App 
                               (list (make-expr Var (list ">") gt-loc)
                                     (make-expr Var (list "x") x2-loc)
                                     (make-expr Const (list 'Number 0) zero2-loc))
                               test2-loc)
                      (make-expr Const (list 'String "positive") pos-result-loc))
                (list (make-expr Var (list "else") else-loc)
                      (make-expr Const (list 'String "zero") zero-result-loc)))
          cond-loc)
```

### And, Or

Represents logical operations.

```
(make-expr And (list expr1 expr2 ...) loc)
(make-expr Or (list expr1 expr2 ...) loc)
```

**Examples:**

```scheme
;; And: (and (> x 0) (< x 10))
(make-expr And 
          (list (make-expr App 
                          (list (make-expr Var (list ">") gt-loc)
                                (make-expr Var (list "x") x1-loc)
                                (make-expr Const (list 'Number 0) zero-loc))
                          test1-loc)
                (make-expr App 
                          (list (make-expr Var (list "<") lt-loc)
                                (make-expr Var (list "x") x2-loc)
                                (make-expr Const (list 'Number 10) ten-loc))
                          test2-loc))
          and-loc)

;; Or: (or (< x 0) (> x 10))
(make-expr Or 
          (list (make-expr App 
                          (list (make-expr Var (list "<") lt-loc)
                                (make-expr Var (list "x") x1-loc)
                                (make-expr Const (list 'Number 0) zero-loc))
                          test1-loc)
                (make-expr App 
                          (list (make-expr Var (list ">") gt-loc)
                                (make-expr Var (list "x") x2-loc)
                                (make-expr Const (list 'Number 10) ten-loc))
                          test2-loc))
          or-loc)
```

### Set!

Represents variable assignment.

```
(make-expr Set! (list var-expr val-expr) loc)
```

Where:
- `var-expr`: Expr (Var) - the variable to be assigned a new value
- `val-expr`: Expr - the value to assign to the variable

**Example:**

```scheme
;; Set!: (set! x 42)
(make-expr Set! 
          (list (make-expr Var (list "x") var-loc)
                (make-expr Const (list 'Number 42) val-loc))
          set-loc)
```

### Load

Represents loading a file.

```
(make-expr Load (list filename) loc)
```

**Example:**

```scheme
;; Load: (load "my-file.scm")
(make-expr Load 
          (list "my-file.scm")
          load-loc)
```

### Dot

Used for dotted pair notation.

```
(make-expr Dot (list car-expr cdr-expr) loc)
```

Where:
- `car-expr`: Expr - expression before the dot
- `cdr-expr`: Expr - expression after the dot

**Example:**

```scheme
;; Dotted pair in argument list: (lambda (x . rest) body)
(make-expr Lambda 
          (list (make-expr App 
                          (list (make-expr Var (list "x") x-loc)
                                (make-expr Dot
                                          (list (make-expr Var (list "rest") rest-loc))
                                          dot-loc))
                          args-loc)
                body-expr)
          lambda-loc)
```

### Do

Represents a loop construct. The `do` special form provides an iterative looping construct.

```
(make-expr Do (list vars-expr test-expr body-expr) loc)
```

Where:
- `vars-expr`: Expr (App) - contains a list of variable initializations and step expressions
  - Each variable has the form: `(list var-expr init-expr step-expr)`
  - `var-expr`: Expr (Var) - the loop variable
  - `init-expr`: Expr - initial value for the variable
  - `step-expr`: Expr - expression to update the variable on each iteration
- `test-expr`: Expr (App) - contains a test expression and result expressions
  - First element: Expr - the test expression that determines when to exit the loop
  - Rest elements: Expr - expressions to evaluate and return when test is true
- `body-expr`: Expr (Begin) - expressions to evaluate on each iteration

**Example:**

```scheme
;; Do: (do ((i 0 (+ i 1)) (sum 0 (+ sum i))) 
;;        ((= i 10) sum)
;;        (display i))
(make-expr Do
          (list (make-expr App
                          (list (list (make-expr Var (list "i") i-var-loc)
                                     (make-expr Const (list 'Number 0) i-init-loc)
                                     (make-expr App 
                                               (list (make-expr Var (list "+") plus1-loc)
                                                     (make-expr Var (list "i") i-step-loc)
                                                     (make-expr Const (list 'Number 1) one-loc))
                                               step1-loc))
                                (list (make-expr Var (list "sum") sum-var-loc)
                                     (make-expr Const (list 'Number 0) sum-init-loc)
                                     (make-expr App 
                                               (list (make-expr Var (list "+") plus2-loc)
                                                     (make-expr Var (list "sum") sum-step-loc)
                                                     (make-expr Var (list "i") i-sum-loc))
                                               step2-loc)))
                          vars-loc)
                (make-expr App
                          (list (make-expr App
                                          (list (make-expr Var (list "=") eq-loc)
                                                (make-expr Var (list "i") i-test-loc)
                                                (make-expr Const (list 'Number 10) ten-loc))
                                          test-cond-loc)
                                (make-expr Var (list "sum") result-loc))
                          test-loc)
                (make-expr App
                          (list (make-expr Var (list "display") display-loc)
                                (make-expr Var (list "i") i-body-loc))
                          body-loc))
          do-loc)
```

The `do` expression initializes variables to values, then repeatedly evaluates the body expressions, updates variables according to their step expressions, and tests if the loop should terminate. When the test expression evaluates to true, the result expressions are evaluated and their values returned as the result of the `do` expression.
