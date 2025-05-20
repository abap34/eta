# eta Language AST Structure Documentation

This document outlines the Abstract Syntax Tree (AST) structure for the eta language implementation. The AST represents the parsed structure of eta programs and is used by the evaluator to execute code.

Definitions and operation interfaces are defined in `parser/ast.rkt`.

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

| ExprHead        | Description                             |
| --------------- | --------------------------------------- |
| 'ConstHead      | Constant value                          |
| 'IdHead         | Identifier                              |
| 'AppHead        | Application                             |
| 'LambdaHead     | Lambda function                         |
| 'QuoteHead      | Quoted expression                       |
| 'DefineHead     | Define a variable or function           |
| 'IfHead         | If expression                           |
| 'CondHead       | Conditional expression                  |
| 'CondClauseHead | Conditional clause  (part of 'CondHead) |
| 'BeginHead      | Begin block                             |
| 'UnNamedLetHead | Unnamed let expression                  |
| 'NamedLetHead   | Named let expression                    |
| 'LetRecHead     | Recursive let expression                |
| 'LetStarHead    | Let star expression                     |
| 'AndHead        | AND expression                          |
| 'OrHead         | OR expression                           |
| 'LoadHead       | Load a file                             |
| 'SetHead        | Breaking assignment                     |
| 'DoHead         | Do expression                           |
| 'DoLetHead      | Let expression inside a Do expression   |
| 'DoFinalHead    | Final expression inside a Do expression |
| 'BodyHead       | Body of a function or block             |
| 'BindHead       | Binding expression                      |
| 'BindingsHead   | list of bindings                        |
| 'ArgHead        | Argument expression                     |
| 'S-ExprHead     | S-expression                            |

## Args Structure by Expression Type



The following table summarizes the structure of the `args` field for each `ExprHead` type:

| ExprHead        | Args Structure                       | Description                                                                                                                                                      |
| --------------- | ------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 'ConstHead      | `(list tag value)`                   | `tag`: Symbol ('IntConstNode, 'FloatConstNode, 'BoolConstNode, 'StringConstNode, 'NilConstNode,  'VoidConstNode, 'UndefinedCons)<br>`value`: The actual value    |
| 'IdHead         | `(list name)`                        | `name`: String - name of the variable                                                                                                                            |
| 'AppHead        | `(list operator args)`               | `operator`: Expr - function to apply<br>`args`: List of Expr - arguments                                                                                         |
| 'LambdaHead     | `(list args body)`                   | `args`: Expr (Arg) - argument pattern<br>`body`: Expr (Body) - function body                                                                                     |
| 'QuoteHead      | `(list expr)`                        | `expr`: Expr - quoted expression                                                                                                                                 |
| 'DefineHead     | `(list name value)`                  | `name`: Expr (Var) - variable name<br>`value`: Expr - value                                                                                                      |
| 'IfHead         | `(list test then else)`              | `test`: Expr<br>`then`: Expr<br>`else`: Expr (if there is no else expression, node that return void is inserted.)                                                |
| 'CondHead       | `(list expr1 expr2 ...)`             | `expr1`: Expr (CondClause)<br>`expr2`: List of Expr (CondClause) - clauses<br> `else`: Expr (if there is no else expression, node that return void is inserted.) |
| 'CondClauseHead | `(list bindings body)`               | `bindings`: Expr (Bindings)<br>`body`: Expr (Body)                                                                                                               |
| 'BeginHead      | `(list name bindings body)`          | `name`: Expr (Var)<br>`bindings`: Expr (Bindings)<br>`body`: Expr (Body)                                                                                         |
| 'UnNamedLetHead | `(list bindings body)`               | `bindings`: Expr (Bindings)<br>`body`: Expr (Body)                                                                                                               |
| 'NamedLetHead   | `(list name bindings body)`          | `name`: name (String) <br> `bindings`: Expr (Bindings)<br>`body`: Expr (Body)                                                                                    |
| 'LetRecHead     | `(list clauses [else-expr])`         | `has-else?`: Boolean - #t if else exists<br>`clauses`: List of Expr (CondClause)<br>`else-expr`: List of Expr (optional)                                         |
| 'LetStarHead    | `(list test body)`                   | `test`: Expr - condition<br>`body`: List of Expr - expressions if condition is true                                                                              |
| 'AndHead        | List of expressions                  | List of Expr objects to be AND-ed                                                                                                                                |
| 'OrHead         | List of expressions                  | List of Expr objects to be OR-ed                                                                                                                                 |
| 'LoadHead       | `(list var value)`                   | `var`: Expr (Var) - variable<br>`value`: Expr - new value                                                                                                        |
| 'SetHead        | `(list filename)`                    | `filename`: Expr (Const with String tag) - path to file                                                                                                          |
| 'DoHead         | `(list vars test body)`              | `vars`: List of Expr (DoLet)<br>`test`: Expr (DoFinal)<br>`body`: Expr (Body)                                                                                    |
| 'DoLetHead      | `(list name init step)`              | `name`: Expr (Var)<br>`init`: Expr - initial value<br>`step`: Expr - update expr                                                                                 |
| 'DoFinalHead    | `(list test result)`                 | `test`: Expr - termination condition<br>`result`: List of Expr - result expressions                                                                              |
| 'BodyHead       | `(list defines expressions)`         | `defines`: List of Expr (Define)<br>`expressions`: List of Expr - body expressions                                                                               |
| 'BindHead       | `(list name value)`                  | `name`: Expr (Var)<br>`value`: Expr - bound value                                                                                                                |
| 'BindingsHead   | List of binding expressions          | List of Expr (Bind) objects                                                                                                                                      |
| 'ArgHead        | `(list required-args variadic-args)` | `required-args`: List of required arguments<br>`variadic-args`: string of variadic variable. if no variadic, empty list                                                                  |
| 'S-ExprHead     | List of S-expressions                | List of Expr objects representing nested S-expressions                                                                                                           |


## Desugar

Some of the expressions in the AST are desugared to simpler forms. 
The desugaring process is defined in `desguar/desugar.rkt`. 
`devdocs/desugar.md` contains the detailed desugaring process for each expression type.