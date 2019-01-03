+++
title = "UCG Formal Grammar"
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
let_keyword: "let" [WS] ;
import_keyword: "import" [WS];
as_keyword: "as" [WS];
macro_keyword: "macro" ;
module_keyword: "module" ;
mod_keyword: "mod" ;
out_keyword: "out" ;
assert_keyword: "assert" ;
null_keyword: "NULL" ;
escaped: "\", VISIBLE_CHAR ;
str: quot, { escaped | UTF8_CHAR }, quot ;
```

Whitespace is discarded before parsing the rest of the AST.

## Simple Scalar Values

```
str:
float: (DIGIT+, dot, { DIGIT }) | (dot, DIGIT+) ;
number: ["-" | "+"](float | integer) ;
```

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

#### Macro Definition

```
arglist: expr, { comma, expr }, [comma] ;
macro_def: macro_keyword, lparen, [ arglist ], rparen, fatcomma, tuple ;
```

#### Module Definition

```
module_def: module_keyword, tuple, fatcomma, lbrace, [ { statement } ], rbrace ;
```

#### Copy and Call Expression

```
copy_expression: bareword, tuple ;
call_expression: bareword, lparen, [arglist], rparen ;
```

#### Non Operator Expression

```
non_operator_expr: literal
                   | grouped
                   | macrodef
                   | module_def
                   | copy_expression
                   | call_expression ;
```

#### Operator Expressions

```
sum_op: plus | minus ;
product_op: start | slash ;
compare_op: equalequal | gtequal | ltequal | gt | lt ;
binary_op: sum_op | product_op | dot | compare_op;
binary_expr: non_operator_expr, binary_op, expr ;
```

Operator expressions have a defined precedence order for evaluation:

* First the `dot` operator binds the tightest of all the operators.
* Next the `product_op` is the next tightest binding of the operators.
* Next the `sum_op` is the next tightest binding of the operators.
* And lastly the `compare_op` is the least tightest binding of the operators.

### Any Expression

```
expr: operator_expression | non_operator_expression ;
```

## Statements

```
let_statement: let_keyword, bareword, equal, expr ;
import_statement: import_keyword, str, as_keyword, bareword ;
out_statement: out_keyword, bareword, str ;
assert_statement: assert_keyword, pipe, { statement }, pipe ;
simple_statement: expr ;

statement: ( let_statement
             | import_statement
             | out_statement
             | assert_statement
             | simple_statement ), semicolon ;
```

## UCG File

```
grammar: { statement } ;
```