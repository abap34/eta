const PREC = {
    EMPTY_LIST:   4,
    COND_CLAUSE:  3,
    COND_ELSE:    2,
    S_EXPR:       1,
  };
  
  module.exports = grammar({
    name: 'eta',
  
    extras: $ => [/\s+/],
  
    rules: {
      source_file: $ => repeat($._toplevel),
  
      _toplevel: $ => choice(
        $.define,
        $.exp,
        $.load,
      ),
  
      load: $ => seq('(', 'load', $.string, ')'),
  
      define: $ => choice(
        seq('(', 'define', $.id, $.exp, ')'),
        seq(
          '(',
            'define',
              '(',
                $.id,
                repeat($.id),
                optional(seq('.', $.id)),
              ')',
              $.body,
          ')'
        )
      ),
  
      body: $ => seq(
        repeat($.define),
        repeat1($.exp)
      ),
  
      exp: $ => choice(
        $.const,
        $.id,
        $.lambda,
        $.application,
        $.quote,
        $.quote_shorthand,
        $.set,
        $.let,
        $.let_star,
        $.letrec,
        $.if,
        $.cond,
        $.and,
        $.or,
        $.begin,
        $.do
      ),
  
      // ---- Literals ----
      const: $ => choice(
        $.number,
        $.bool,
        $.string
      ),
  
      empty_list: $ => prec(PREC.EMPTY_LIST, seq('(', ')')),
  
      number: $ => /[+-]?\d+(\.\d+)?/,
      bool:   $ => choice('#t', '#f'),
      string: $ => /"([^"\\]|\\.)*"/,
  
      id:     $ => /[0-9A-Za-z!$%&*+\-./<=>?@^_]+/,
  
      lambda: $ => seq('(', 'lambda', $.arg, $.body, ')'),
      application: $ => seq('(', $.exp, repeat($.exp), ')'),
  
      quote: $ => seq('(', 'quote', $.s_expr, ')'),
      quote_shorthand: $ => seq("'", $.s_expr),
  
      set: $ => seq('(', 'set!', $.id, $.exp, ')'),
  
      let:      $ => seq('(', 'let',      optional($.id), $.bindings, $.body, ')'),
      let_star: $ => seq('(', 'let*',     $.bindings,            $.body, ')'),
      letrec:   $ => seq('(', 'letrec',   $.bindings,            $.body, ')'),
  
      if: $ => seq('(', 'if', $.exp, $.exp, optional($.exp), ')'),
  
      cond: $ => seq(
        '(', 'cond',
          repeat($.cond_clause),
          optional($.cond_else),
        ')'
      ),
  
      cond_clause: $ => prec(PREC.COND_CLAUSE,
        seq($.exp, repeat1($.exp))
      ),
  
      cond_else: $ => prec(PREC.COND_ELSE,
        seq('else', repeat1($.exp))
      ),
  
      and:   $ => seq('(', 'and',   repeat($.exp), ')'),
      or:    $ => seq('(', 'or',    repeat($.exp), ')'),
      begin: $ => seq('(', 'begin', repeat($.exp), ')'),
  
      do: $ => seq(
        '(', 'do',
          seq('(', repeat(seq($.id, $.exp, $.exp)), ')'),
          seq('(', $.exp, repeat($.exp), ')'),
          $.body,
        ')'
      ),
  
      arg: $ => choice(
        $.id,
        seq('(', repeat($.id), optional(seq('.', $.id)), ')')
      ),
  
      bindings: $ => seq('(', repeat(seq($.id, $.exp)), ')'),
  

      s_expr: $ => prec(PREC.S_EXPR, choice(
        $.const,
        $.id,
        seq('(', repeat($.s_expr), optional(seq('.', $.s_expr)), ')')
      )),
    }
  });
  