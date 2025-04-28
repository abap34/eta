[
  "(" @punctuation.bracket
  ")" @punctuation.bracket
]


[
  "define" @keyword
  "load"   @keyword

  "lambda" @keyword
  "if"     @keyword
  "cond"   @keyword
  "let"    @keyword
  "let*"   @keyword
  "letrec" @keyword
  "and"    @keyword
  "or"     @keyword
  "begin"  @keyword
  "do"     @keyword

  "set!"   @keyword

  "quote"  @keyword
]

[
  (id)     @variable
]

[
  (number) @number
]

[
  (bool)   @boolean
]

[
  (string) @string
]
