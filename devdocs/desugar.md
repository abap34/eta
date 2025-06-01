## eta Language Desugar Processor Documentation

In eta Language, some of the expressions in the AST are desugared to simpler forms. The desugaring process transforms complex language constructs into combinations of simpler ones. This document details how each expression type is desugared.

### Expression Desugaring Table

| ExprHead        | Desugar? | Target Form              | Survives Desugaring? |
| --------------- | -------- | ------------------------ | -------------------- |
| 'ConstHead      | No       | Same                     | Yes                  |
| 'IdHead         | No       | Same                     | Yes                  |
| 'AppHead        | Yes      | AppHead                  | Yes                  |
| 'LambdaHead     | Yes      | LambdaHead               | Yes                  |
| 'QuoteHead      | No       | Same                     | Yes                  |
| 'DefineHead     | Yes      | DefineHead               | Yes                  |
| 'IfHead         | No       | Same                     | Yes                  |
| 'CondHead       | Yes      | Nested IfHead            | No                   |
| 'CondClauseHead | Yes      | IfHead                   | No                   |
| 'BeginHead      | Yes      | BodyHead                 | No                   |
| 'UnNamedLetHead | Yes      | AppHead (lambda)         | No                   |
| 'NamedLetHead   | Yes      | LetRecHead               | No                   |
| 'LetRecHead     | Yes      | UnNamedLetHead+BeginHead | No                   |
| 'LetStarHead    | Yes      | Nested UnNamedLetHead    | No                   |
| 'AndHead        | Yes      | Nested IfHead            | No                   |
| 'OrHead         | Yes      | UnNamedLetHead+IfHead    | No                   |
| 'LoadHead       | No       | Same                     | Yes                  |
| 'SetHead        | Yes      | SetHead                  | Yes                  |
| 'BodyHead       | Yes      | BodyHead                 | Yes                  |
| 'BindHead       | Yes      | BindHead                 | Yes                  |
| 'BindingsHead   | Yes      | BindingsHead             | Yes                  |
| 'ArgHead        | No       | Same                     | Yes                  |
| 'S-ExprHead     | No       | Same                     | Yes                  |


### Basic Desugaring Rules

#### Conditional Expressions

- `cond`
- `and`
- `or`

are desugared to nested `if` expressions.

#### Let Expressions

- `let`
- `let*`

are desugared to lambda applications.

#### LetRec Expressions

- `letrec`

is desugared to `let` and `set!` expressions.

### Detailed Desugaring Rules


The following sections describe the desugaring rules for each expression type in detail.

Omit `loc` since it is not relevant in this case, in this document, 
`(Expr head args loc)` will be written as `(head args)`.
Also, each argument is annotated as `::A` when it always has a `Expr` with `A` head.

---

#### 'ConstHead 

```scheme
('ConstHead (list tag value))
```

Constant values are preserved as is.

---

#### 'IdHead 

```scheme
('IdHead (list name))
```

Identifiers are preserved as is.

---


#### 'AppHead 

```scheme
('AppHead (list operator args))
```

is desugared to:

```scheme
('AppHead (list (desugar operator) (map desugar args)))
```

---

#### 'LambdaHead

```scheme
('LambdaHead (list args::Arg body::Body))
```

is desugared to:

```scheme
('LambdaHead (list (desugar args) (desugar body)))
```

---

#### 'QuoteHead

```scheme
('QuoteHead (list expr))
```

preserves the quoted expression as is.

---

#### 'DefineHead

```scheme
('DefineHead (list name::Var value::Expr))
```

is desugared to:

```scheme
('DefineHead (list name (desugar value)))
```

---

#### 'IfHead

```scheme
('IfHead (list test::Expr then::Expr else::Expr))
```

If expressions are preserved as is.

---

#### 'CondHead

```scheme
('CondHead (list clauses::list of CondClause else::Expr))
```

is desugared to:

```scheme
('IfHead (list (desugar (first clauses))
               ('IfHead (list (desugar (second clauses))
                              ('IfHead ...
                                (desugar else))))))
```

---

#### 'CondClauseHead

```scheme
('CondClauseHead (list test::Expr body::Body))
```

is desugared to:

```scheme
('IfHead (list (desugar test)
               (desugar body)))
```

---

#### 'BeginHead

```scheme
('BeginHead (list exprs::list of Expr))
```

is desugared to:

```scheme
('BodyHead (list '() (map desugar exprs)))
```

---

#### 'UnNamedLetHead

```scheme
('UnNamedLetHead (list bindings::Bindings body::Body))
```

is desugared to:

```scheme
('AppHead (list
             ('LambdaHead (list (make-list-arg loc-of-bindings (map get-bind-name bindings) '())
                                (desugar body)))
             (map get-bind-value bindings)))
```

----


#### 'NamedLetHead

```scheme
('NamedLetHead (list name::Var bindings::Bindings body::Body))
```

is desugared to:

```scheme
('LetRecHead (
     (make-bindings
         (make-bind (name 
                    (let->lambda (bindings body)))))
         ('AppHead (list name (map get-bind-value bindings)))))
```


----

#### 'LetRecHead

```scheme
('LetRecHead (list bindings::Bindings body::Body))
```

is desugared to:

```scheme
('UnNamedLetHead 
       (map bindings->undefined bindings)
       ('BeginHead (list
                     (map bindings->set! bindings)
                     (desugar body))))
```

where `bindings->undefined` is a helper function that transforms each binding into a form that initializes the variable to `undefined`:
      `bindings->set!` is a helper function that transforms each binding into a form that sets the variable to its value breakingly.


---

#### 'LetStarHead

```scheme
('LetStarHead (list name::Var bindings::Bindings body::Body))
```


is desugared to:

```scheme
(desugar ('UnNamedLetHead (list (first bindings) 
            ('UnNamedLetHead (list (second (second bindings) 
                                ... 
                                (desugar body)))))))_
```

---

#### 'AndHead

```scheme
('AndHead (list exprs::list of Expr))

```

is desugared to:

```scheme
('IfHead (list (first exprs)
               ('IfHead (list (second exprs)
                              ('IfHead ...
                                (desugar (last exprs)))))))
```


---

#### 'OrHead

```scheme
('OrHead (list exprs::list of Expr))
```


is desugared to:

```scheme
('IfHead (list (first exprs)
               ('IfHead (list (second exprs)
                              ('IfHead ...
                                (desugar (last exprs)))))))
```

---

#### 'LoadHead

```scheme
('LoadHead (list filename::String))
```

preserves the filename as is. 

---

#### 'SetHead

```scheme
('SetHead (list name::Var value::Expr))
```

is desugared to:


```scheme
('SetHead (list name (desugar value)))
```


---

#### 'BodyHead

```scheme
('BodyHead (list defines::list of Define exprs::list of Expr))
```

is desugared to:

```scheme
('BodyHead (list (map desugar defines)
                 (map desugar exprs)))
```

---

#### 'BindHead

```scheme
('BindHead (list name::Var value::Expr))
```

is desugared to:

```scheme
('BindHead (list name (desugar value)))
```

---

#### 'BindingsHead

```scheme
('BindingsHead (list bindings::list of Bind))
```

is desugared to:
```scheme
('BindingsHead (list (map desugar bindings)))
```

---

#### 'ArgHead

```scheme
('ArgHead (list required-args::list of String variadic-args::list of String))
```
 
preserves the arguments as is.

---

#### 'S-ExprHead

```scheme
('S-ExprHead (list exprs::list of Expr))
```

preserves the expressions as is.

