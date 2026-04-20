/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: 'ucg',

  extras: $ => [/\s/, $.comment],

  word: $ => $.identifier,

  conflicts: $ => [
    [$._non_op_expression, $.copy_expression],
    [$._non_op_expression, $._range_operand],
    [$.range_expression],
    [$.grouped_expression, $.format_args],
    [$.copy_expression, $._range_operand],
  ],

  rules: {
    source_file: $ => repeat($._statement),

    // ==================== STATEMENTS ====================

    _statement: $ => choice(
      $.let_statement,
      $.assert_statement,
      $.constraint_statement,
      $.out_statement,
      $.expression_statement,
    ),

    let_statement: $ => seq(
      'let',
      field('name', $.identifier),
      optional($.constraint_annotation),
      '=',
      field('value', $._expression),
      ';',
    ),

    assert_statement: $ => seq('assert', $._expression, ';'),

    constraint_statement: $ => seq(
      'constraint',
      field('name', $.identifier),
      '=',
      field('value', $.constraint_expression),
      ';',
    ),

    out_statement: $ => seq(
      'out',
      field('converter', $.identifier),
      field('value', $._expression),
      ';',
    ),

    expression_statement: $ => seq($._expression, ';'),

    // ==================== CONSTRAINTS ====================

    constraint_annotation: $ => seq('::', $.constraint_expression),

    constraint_expression: $ => seq(
      $.constraint_arm,
      repeat(seq('|', $.constraint_arm)),
    ),

    constraint_arm: $ => choice(
      $.constraint_range,
      $._non_op_expression,
    ),

    constraint_range: $ => seq(
      'in',
      optional($._non_op_expression),
      '..',
      optional($._non_op_expression),
    ),

    // ==================== EXPRESSIONS ====================

    _expression: $ => choice(
      $.binary_expression,
      $._non_op_expression,
    ),

    binary_expression: $ => choice(
      // Precedence 6 - dot selector (tightest)
      prec.left(6, seq(
        field('left', $._expression),
        field('operator', '.'),
        field('right', $._expression),
      )),
      // Precedence 5 - boolean
      prec.left(5, seq(
        field('left', $._expression),
        field('operator', choice('&&', '||')),
        field('right', $._expression),
      )),
      // Precedence 4 - multiplication/division/modulo
      prec.left(4, seq(
        field('left', $._expression),
        field('operator', choice('*', '/', '%%')),
        field('right', $._expression),
      )),
      // Precedence 3 - addition/subtraction
      prec.left(3, seq(
        field('left', $._expression),
        field('operator', choice('+', '-')),
        field('right', $._expression),
      )),
      // Precedence 2 - membership/type
      prec.left(2, seq(
        field('left', $._expression),
        field('operator', choice('in', 'is')),
        field('right', $._expression),
      )),
      // Precedence 1 - comparison (loosest)
      prec.left(1, seq(
        field('left', $._expression),
        field('operator', choice('==', '!=', '~', '!~', '<', '>', '<=', '>=')),
        field('right', $._expression),
      )),
    ),

    _non_op_expression: $ => choice(
      $.func_op_expression,
      $.func_expression,
      $.import_expression,
      $.trace_expression,
      $.not_expression,
      $.fail_expression,
      $.convert_expression,
      $.module_expression,
      $.select_expression,
      $.range_expression,
      $.grouped_expression,
      $.include_expression,
      $._unprefixed_expression,
    ),

    _unprefixed_expression: $ => choice(
      $.format_expression,
      $.simple_expression,
      $.call_expression,
      $.copy_expression,
    ),

    simple_expression: $ => $._value,

    // ==================== VALUES ====================

    _value: $ => choice(
      $.identifier,
      $.tuple,
      $.list,
      $.boolean,
      $.null,
      $.integer,
      $.float,
      $.string,
    ),

    tuple: $ => seq(
      '{',
      optional($.field_list),
      '}',
    ),

    field_list: $ => seq(
      $.field,
      repeat(seq(',', $.field)),
      optional(','),
    ),

    field: $ => seq(
      field('name', $._field_name),
      optional($.constraint_annotation),
      '=',
      field('value', $._expression),
    ),

    _field_name: $ => choice(
      $.identifier,
      $.string,
    ),

    list: $ => seq(
      '[',
      optional(seq(
        $._expression,
        repeat(seq(',', $._expression)),
        optional(','),
      )),
      ']',
    ),

    boolean: $ => choice('true', 'false'),

    null: $ => 'NULL',

    integer: $ => /\d+/,

    float: $ => token(seq(/\d+/, '.', /\d+/)),

    string: $ => seq(
      '"',
      repeat(choice(
        $.escape_sequence,
        $.string_content,
      )),
      '"',
    ),

    string_content: $ => token.immediate(prec(-1, /[^"\\]+/)),

    escape_sequence: $ => token.immediate(seq('\\', /[\\nrt"@]/)),

    // ==================== COMPLEX EXPRESSIONS ====================

    grouped_expression: $ => seq('(', $._expression, ')'),

    copy_expression: $ => prec.dynamic(1, seq(
      field('base', $._unprefixed_expression),
      '{',
      optional($.field_list),
      '}',
    )),

    call_expression: $ => prec.dynamic(2, seq(
      field('function', $.identifier),
      '(',
      optional(seq(
        $._expression,
        repeat(seq(',', $._expression)),
        optional(','),
      )),
      ')',
    )),


    _range_operand: $ => choice(
      $._unprefixed_expression,
      $.grouped_expression,
    ),

    range_expression: $ => choice(
      prec(7, seq(
        field('start', $._range_operand),
        ':',
        field('step', $._range_operand),
        ':',
        field('end', $._range_operand),
      )),
      prec(7, seq(
        field('start', $._range_operand),
        ':',
        field('end', $._range_operand),
      )),
    ),

    format_expression: $ => prec(7, seq(
      field('template', $.string),
      '%',
      field('args', choice($.format_args, $._expression)),
    )),

    format_args: $ => seq(
      '(',
      $._expression,
      repeat(seq(',', $._expression)),
      optional(','),
      ')',
    ),

    import_expression: $ => seq('import', field('path', $.string)),

    include_expression: $ => seq(
      'include',
      field('type', $.include_type),
      field('path', $.string),
    ),

    include_type: $ => choice('str', 'b64', 'b64urlsafe', 'json', 'yaml', 'toml'),

    func_expression: $ => seq(
      'func',
      '(',
      optional($.func_arglist),
      ')',
      '=>',
      field('body', $._expression),
    ),

    func_arglist: $ => seq(
      $.func_arg,
      repeat(seq(',', $.func_arg)),
      optional(','),
    ),

    func_arg: $ => seq(
      field('name', $.identifier),
      optional($.constraint_annotation),
    ),

    module_expression: $ => seq(
      'module',
      '{',
      optional($.field_list),
      '}',
      '=>',
      optional($.module_export),
      '{',
      repeat($._statement),
      '}',
    ),

    module_export: $ => seq('(', $._expression, optional($.constraint_annotation), ')'),

    select_expression: $ => seq(
      'select',
      '(',
      field('value', $._expression),
      optional(seq(',', field('default', $._expression))),
      optional(','),
      ')',
      '=>',
      '{',
      $.field_list,
      '}',
    ),

    not_expression: $ => seq('not', $._expression),

    fail_expression: $ => seq('fail', $._expression),

    trace_expression: $ => seq('TRACE', $._expression),

    convert_expression: $ => seq(
      'convert',
      field('converter', $.identifier),
      field('value', $._expression),
    ),

    func_op_expression: $ => choice(
      $.map_expression,
      $.filter_expression,
      $.reduce_expression,
    ),

    map_expression: $ => seq(
      'map',
      '(',
      field('function', $._expression),
      ',',
      field('collection', $._expression),
      optional(','),
      ')',
    ),

    filter_expression: $ => seq(
      'filter',
      '(',
      field('function', $._expression),
      ',',
      field('collection', $._expression),
      optional(','),
      ')',
    ),

    reduce_expression: $ => seq(
      'reduce',
      '(',
      field('function', $._expression),
      ',',
      field('accumulator', $._expression),
      ',',
      field('collection', $._expression),
      optional(','),
      ')',
    ),

    // ==================== TOKENS ====================

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    comment: $ => token(seq('//', /[^\n]*/)),
  },
});
