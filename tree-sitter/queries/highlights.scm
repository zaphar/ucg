; Keywords
"let" @keyword
"assert" @keyword
"constraint" @keyword
"out" @keyword
"import" @keyword.import
"include" @keyword.import
"func" @keyword.function
"module" @keyword
"select" @keyword.conditional
"fail" @keyword.exception
"TRACE" @keyword.debug
"not" @keyword.operator
"convert" @keyword

; Functional builtins
"map" @function.builtin
"filter" @function.builtin
"reduce" @function.builtin

; Type cast calls (int, float, str, bool used as functions)
(call_expression
  function: (identifier) @type.builtin
  (#match? @type.builtin "^(int|float|str|bool)$"))

; Include types
(include_type) @type

; Literals
(integer) @number
(float) @number.float
(string) @string
(string_content) @string
(escape_sequence) @string.escape
(boolean) @boolean
(null) @constant.builtin

; Identifiers
(identifier) @variable

; Let binding names
(let_statement
  name: (identifier) @variable.definition)

; Constraint binding names
(constraint_statement
  name: (identifier) @variable.definition)

; Field names in tuples
(field
  name: (identifier) @property)
(field
  name: (string) @property)

; Function definitions
(func_expression) @function
(func_arg
  name: (identifier) @variable.parameter)

; Function calls
(call_expression
  function: (identifier) @function.call)

; Copy expression base
(copy_expression
  base: (simple_expression
    (identifier) @type))

; Module export
(module_export) @variable

; Import/include paths
(import_expression
  path: (string) @string.special.path)
(include_expression
  path: (string) @string.special.path)

; Out/convert converter names
(out_statement
  converter: (identifier) @type)
(convert_expression
  converter: (identifier) @type)

; Binary operators
(binary_expression
  operator: _ @operator)

; Other operators
"%" @operator
"in" @keyword.operator
"is" @keyword.operator

; Punctuation
"=>" @punctuation.special
"::" @punctuation.special
".." @punctuation.special
":" @punctuation.delimiter
"=" @punctuation.delimiter
"," @punctuation.delimiter
";" @punctuation.delimiter
"|" @punctuation.delimiter
"(" @punctuation.bracket
")" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket

; Comments
(comment) @comment.line
