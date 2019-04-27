+++
title = "Formal Grammar"
slug = "grammar"
weight = 1
sort_by = "weight"
in_search_index = true
+++
UCG Formal Grammar
------------------------

## Definitions

* WS is any non-visible utf-8 whitespace.
* DIGIT is any ascii number character.
* VISIBLE_CHAR is any visible utf-8 character.
* ASCII_CHAR is any visible ascii letter character.
* UTF8_CHAR is any utf8 character including ws.

## Tokens

```
ws: WS ;
dot: ".";
quot: '"' ;
pipe: '|' ;
percent: "%" ;
star: "*" ;
plus: "+" ;
minus: "-" ;
slash: "/" ;
equal: "=" ;
gtequal: ">=" ;
ltequal: "<=" ;
equalequal: "<=" ;
gt: ">" ;
lt: "<" ;
fatcomma: "=>" ;
comma: "," ;
integer: DIGIT+ ;
lbrace: "{" ;
rbrace: "}" ;
lbracket: "[" ;
rbracket: "]" ;
lparen: "(" ;
rparen: ")" ;
bareword: ASCII_CHAR, { DIGIT | VISIBLE_CHAR | "_" }  ;
let_keyword: "let" ;
import_keyword: "import" ;
include_keyword: "include" ; 
as_keyword: "as" ;
func_keyword: "func" ;
map_keyword: "map" ;
reduce_keyword: "map" ;
filter_keyword: "filter" ;
module_keyword: "module" ;
mod_keyword: "mod" ;
out_keyword: "out" ;
assert_keyword: "assert" ;
fail_keyword: "fail" ;
trace_keyword: "TRACE" ;
null_keyword: "NULL" ;
in_keyword: "in" ;
is_keyword: "in" ;
not_keyword: "module" ;
escaped: "\", VISIBLE_CHAR ;
str: quot, { escaped | UTF8_CHAR }, quot ;
float: (DIGIT+, dot, { DIGIT }) | (dot, DIGIT+) ;
number: ["-" | "+"](float | integer) ;
```

Whitespace is discarded before parsing the rest of the AST.

## Complex Values

### Lists

```
field: bareword | str ;
list_elements: expr, (comma, expr)*, [comma] ;
list: lbracket, [ list_elements ], rbracket ;
```

### Tuples

```
field_pair: field, equal, expr ;
field_list: field_pair, { comma, field_pair }, [comma]
tuple: lbrace, [ field_list ], rbrace;
```

## Expressions

### Simple Expressions

```
simple_expr: literal | bareword ;
```

#### Literals

```
literal: str | integer | float | list | tuple | null_keyword;
```

### Complex Expressions

#### Grouped Expression

```
grouped: lparen, expr, rparen ;
```

#### Function Definition

```
arglist: expr, { comma, expr }, [comma] ;
func_def: func_keyword, lparen, [ arglist ], rparen, fatcomma, tuple ;
```

#### Module Definition

```
module_def: module_keyword, tuple, fatcomma, [lparen, expr, rparen], lbrace, [ { statement } ], rbrace ;
```

#### Copy and Call Expression

```
copy_expr: bareword, tuple ;
call_expr: bareword, lparen, [arglist], rparen ;
```

#### Format Expression

```
format_arg_list: lparen, [arglist], rparen ;
foramt_expr_arg: expression ;
format_expr: str, percent, (format_arg_list | format_expr_arg) ;
```

### Functional processing expressions

```
func_op_kind: map_keyword | filter_keyword ;
map_or_filter_expr: func_op_kind, lparen, expr, expr, rparen ;
reduce_expr: reduce_keyword, lparen, expr, expr, expr, rparen ;
processing_expr: map_or_filter_expr | reduce_expr
```

### Range Expression

```
range_expr: expr, ':', [int, ':'], expr ;
```

#### Include Expression

```
include_expr: include_keyword, bareword, str ; 
```

#### Import expression

```
import_expr: import_keyword, str ;
```

#### Fail expressions

```
fail_expr: fail_keyword, (str | format_expr) ;
```

#### Not Expression

```
not_expr: not_keyword, expr ;
```

#### Not Expression

```
trace_expr: trace_keyword, expr ;
```

#### Non Operator Expression

```
non_operator_expr: literal
                   | grouped
                   | import_expr
                   | funcdef
                   | module_def
                   | fail_expr
                   | not_expr
                   | trace_expr
                   | format_expr
                   | range_expr
                   | include_expr
                   | copy_expr
                   | processing_expr
                   | call_expr ;
```

#### Operator Expressions

```
sum_op: plus | minus ;
product_op: start | slash ;
compare_op: equalequal | gtequal | ltequal | gt | lt | in_keyword | is_keyword ;
binary_op: sum_op | product_op | dot | compare_op ;
binary_expr: non_operator_expr, binary_op, expr ;
```

Operator expressions have a defined precedence order for evaluation:

* First the `dot` operator binds the tightest of all the operators.
* Next the `product_op` is the next tightest binding of the operators.
* Next the `sum_op` is the next tightest binding of the operators.
* And lastly the `compare_op` is the least tightest binding of the operators.

### Any Expression

```
expr: binary_expr | non_operator_expr ;
```

## Statements

```
let_statement: let_keyword, bareword, equal, expr ;
out_statement: out_keyword, bareword, str ;
assert_statement: assert_keyword, pipe, { statement }, pipe ;
simple_statement: expr ;

statement: ( let_statement
             | out_statement
             | assert_statement
             | simple_statement ), semicolon ;
```

## UCG File

```
grammar: { statement } ;
```