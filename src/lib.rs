// Copyright 2017 Jeremy Wall <jeremy@marzhillstudios.com>
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
// #![feature(trace_macros,log_syntax)]

//! # ucg, A universal configuration grammar.
//!
//! Ucg defines a common grammar for describing a collection of configuration values.
//! ucg allows you to specify configuration values with a syntax that that is immutable,
//! comoposable with copy-on-write semantics, and safe.
//!
//! ## Example
//!
//! ```ucg
//! // named bindings
//! let host = "mysql.internal.net";
//! let port = 8080
//!
//! // format strings
//! let connstr = "mysql://@:@" % (host, port);
//!
//! // tuples
//! let dbconf = {
//!     connstr = connstr,
//!     database = "mydb",
//!     // lists
//!     tables = ["posts", "comments", "users"],
//! };
//! ```
//!
//! ## Syntax
//!
//! ucg is a safe language with type inference that tries to guarantee that it will halt.
//! A valid ucg file is composesed of a series of statements. Statements are any valid
//! ucg expression terminated by a semicolon.
//!
//! ### Reserved words
//!
//! The following words are reserved in ucg and can't be used as named bindings.
//!
//! * true
//! * false
//! * let
//! * import
//! * as
//! * select
//! * macro
//! * env
//! * map
//! * filter
//! * NULL
//!
//! ### Primitive types
//!
//! ucg has a relatively simple syntax with a few primitive types, Null, Boolean, Int, Float, and String.
//!
//! ### Boolean
//!
//! A Boolean is either `true` or `false`.
//!
//! ```uct
//! true;
//! false;
//! ```
//!
//! #### Int
//!
//! An Int is any integer number.
//!
//! ```ucg
//! 1; // a single Integer
//! ```
//!
//! #### Float
//!
//! A Float is any number with a decimal point.
//!
//! ```ucg
//! 1.0; // A typical float.
//! 1. // You can leave off the 0 after the decimal point.
//! .1 // the leading 0 is also optional.
//! ```
//!
//! #### String
//!
//! A String is any quoted text. Backslashes within a string escape the next preceding
//! character.
//!
//! ``` ucg
//! "foo"; // a simple string
//! "I'm a \"fine\" looking string"; // escaped quotes in a string.
//! ```
//!
//! #### NULL or the Empty type.
//!
//!  A NULL is an empty type. It represents no value.
//!
//! ```ucg
//! let empty = NULL;
//! ```
//!
//! ### Complex types
//!
//! ucg has two complex data types, Lists and Tuples.
//!
//! #### Lists
//!
//! Lists are surrounded with square brackets `[ ]` and have comma separated elements.
//!
//! ```ucg
//! [1, 2, 3]; // A simple list of numbers.
//!
//! [[1, 2], [3, 4]] // A deep list with embedded lists inside.
//! ```
//!
//! Lists are 0 indexed and you can index into them using the dotted selector syntax.
//!
//! ```ucg
//! let mylist = [0, 1, 2, 3];
//!
//! let zero = mylist.0;
//! ```
//!
//! ##### List macros
//!
//! ucg supports a couple of ways to use macros for mapping or filtering a list to a new list.
//!
//! A map expression starts with the map keyword followed by the name of a macro with exactly
//! one argument, a `.`, and the name of the output field for the macro. ucg will apply the macro
//! to each element of the list and then take the output field from the resulting tuple and add append
//! it to the resulting list. If the output field does not exist in the macro it will be a compile
//! error.
//!
//! ```ucg
//! let list = [1, 2, 3, 4];
//! let mapper = macro(item) => { result = item + 1 };
//!
//! // results in: [2, 3, 4, 5]
//! let mapped = map mapper.result list;
//! ```
//!
//
//! A filter expression starts with the filter keyword followed by the name of a macro with exactly
//! one argument, a `.`, and the name of the output field for the macro. The filter will apply the
//! macro to each element of the list and if the output field is a Value that is not NULL then the
//! list element is appended to the output list. If the output field returns a NULL Value then the
//! element is not appended to the output list. If the output field does not exist in the macro it
//! will be a compile error.
//!
//! ```ucg
//! let list = ["foo", "bar", "foo", "bar"];
//! let filtrator = macro(item) => {
//!   ok = select item NULL {
//!     foo = 1
//!   }
//! };
//!
//! // results in: ["foo", "foo"]
//! let filtered = filter filtrator.ok list;
//! ```
//!
//! #### Tuple
//!
//! Tuple's are an ordered collection of name, value pairs. They are bounded by curly braces `{ }`
//! and contain name = value pairs separated by commas. Trailing commas are permitted. The name must
//! be a bareword without quotes.
//!
//! ```ucg
//! let mytuple = {
//!     field1 = "value1",
//!     field2 = "value2",
//! };
//! ```
//!
//! Tuples can be indexed using dotted selector syntax.
//!
//! ```ucg
//! let field = mytuple.fields1;
//! ```
//!
//! ### Expressions
//!
//! #### Selectors
//!
//! Selectors are references to a bound value in ucg. They can index arbitrarily deep into either tuples or lists.
//! The head of a selector can be any expression that resolves to a tuple or list. Optionally a selector can also be
//! followed by either a bareword to index a tuple field or an integer to index a list position.
//!
//! The simplest selector is just a reference to a bound value by name.
//!
//! ```ucg
//! let mytuple = {
//!     field1 = "a string",
//!     field2 = [{
//!         subfield1 = 1,
//!     }];
//! };
//!
//! mytuple.field2.0; // descend into a deeply nested tuple and array.
//! ```
//!
//! The `env` variable is a reserved variable that always contains a tuple with any environment
//! variables in it.
//!
//! Attempting to reference an enviroment variable that does not exist is a compile error.
//!
//! #### Binary operators
//!
//! ##### Numeric operators
//!
//! ucg supports the following numeric operators, `+`, `-`, `*`, `/` Each one is type safe and infers the types
//! from the values they operate on. The operators expect both the left and right operands to be of the same
//! type.
//!
//! ```ucg
//! 1 + 1; // result is 2
//! ```
//!
//! ##### Concatenation
//!
//! ucg supports concatenation using the `+` operator. It is typesafe expecting both sides to be of the same type.
//! You can concatenate strings or lists but not tuples.
//!
//! ```ucg
//! "foo " + "bar" // result is "foo bar"
//! [1,2] + [3,4]; // result is [1,2,3,4]
//! ```
//!
//! ##### Comparison
//!
//! ucg supports comparison using the `==`, `!=`, `>`, `<`, `>=`, `<=` operators. They are type safe and expect both
//! sides to be of the same type.
//!
//! The `>`, `<`, `>=`, and `>=` operators are only supported on numeric types.
//!
//! ```ucg
//! 1 > 2; // result is false
//! 2 < 3; // result is true
//! 10 > "9"; // This is a compile error.
//! (1+2) == 3
//! ```
//! The comparison operators expect either a simple value or a grouped expression as their left operand.
//!
//! The equality operators `==` and `!=` are supported for all types and will perform deep equal comparisons on complex
//! types.
//!
//! ```ucg
//! let tpl1 = {
//!   foo = "bar",
//!   one = 1
//! };
//! let tpl2 = tpl1{}; // copy the tpl1 tuple
//! tpl1 == tpl2; // returns true
//! let tpl3 = tpl1{duck="quack"};
//! tpl1 == tpl3; // returns false
//! ```
//!
//! Note that tuple fields are ordered so a tuple will only be equal if the fields are both in the same order and
//! have the same values in them.
//!
//! ##### Operator Precedence
//!
//! UCG binary operators follow the typical operator precedence for math. `*` and `/` are higher precendence than
//! `+` and `-` which are higher precedence than any of the comparison operators.
//!
//! #### Copy expressions
//!
//! ucg Tuples support a form of reuse with copy on write semantics. You can copy a tuple and selectively overwrite fields or add new
//! fields to it with the copy expression. To perform a copy first reference the tuple by a bound name and then use `{ field = value, ... }`
//! syntax to copy with overridden fields  or add completely new fields. When replacing a preexisting field with a new value you cannot
//! change the type of the field. This allows you to define a typed shape for a tuple with default values and then provide new values for
//! some or all of the fields while still enforcing the same types for those fields. Adding completely new fields has no such restriction.
//!
//! ```ucg
//! let base = {
//!     field1 = "value1",
//!     field2 = 100,
//!     field3 = 5.6,
//! };
//!
//! let overridden = base{
//!     field1 = "new value"
//! };
//!
//! let expanded = base{
//!     field2 = 200,
//!     field3 = "look ma a new field",
//! };
//! ```
//!
//! The following will cause an error because the overriden field's value does not match the original.
//!
//! ```ucg
//! let bad = base{
//!     field1 = 300, // Error!!! must be a string.
//! };
//!
//! ```
//!
//! #### Conditional data
//!
//! ucg supports a limited form of conditional data selection of using the select expression. A select expression starts with the `select`
//! keyword and is followed by a an expression resolving to a string naming the field to select, an expression resolving to the default value,
//! and a tuple to select the field from. If the field selected is not in the tuple then the default value will be used.
//!
//! ```ucg
//! let want = "baz";
//!
//! //     field  default
//! select want, "quux", {
//!     baz = "foo",
//!     fuzz = "bang",
//! }; // result will be "foo"
//!
//! //     field    default
//! select "quack", "quux", {
//!     baz = "foo",
//!     fuzz = "bang",
//! }; // result will be "quux"
//! ```
//!
//! #### Macros
//!
//! Macros look like functions but they are resolved at compile time and configurations don't execute so they never appear in output.
//! They are useful for constructing tuples of a certain shape or otherwise promoting data reuse. You define a macro with the `macro`
//! keyword followed by the arguments in parentheses, a `=>`, and then a tuple.
//!
//! ```ucg
//! let myfunc = macro (arg1, arg2) => {
//!     host = arg1,
//!     port = arg2,
//!     connstr = "couchdb://@:@" % (arg1, arg2),
//! }
//!
//! let my_dbconf = myfunc("couchdb.example.org", "9090");
//!
//! let my_dbhost = dbconf.host;
//! ```
//!
//! macros always resolve to a tuple. If you want to get a single value out you can use selector syntax to retrieve it.
//!
//! ### Statements
//!
//! There are 3 kinds of statements in a ucg configuration file. expression statements, let statements, and import statements.
//! All ucg statements must be terminated by a semicolon.
//!
//! * expression statements
//!
//! The simplest and least useful is the expression statement. It is any valid expression followed by a semicolon.
//!
//! ```ucg
//! 1;
//! 4 / 2;
//! "foo";
//! "foo" + "bar";
//! ```
//!
//! Despite the fact that these are valid the results are thrown away and can essentially be considered a noop. If we
//! ever create a repl for ucg statements they may prove more useful.
//!
//!  * Let statements
//!
//! The let expression binds the result of any valid expression to a name. It starts with the `let` keyword and is followed by
//! the name of the binding, an `=`, and a valid ucg expression.
//!
//! ```ucg
//! let name = "foo";
//! ```
//!
//! * Import statement
//!
//! The import statement imports the contents of another ucg file into the current file with a name. The imported files bound
//! values are exposed as a tuple in the referencing file. It starts with the `import` keyword and is followed by a quoted path
//! to the ucg file, the keyword `as`, and a name for the imported values.
//!
//! ```ucg
//! import "dbconfigs.ucg" as dbconfigs;
//!
//! let mysqlconf = dbconfigs.mysql;
//! ```

// The following is necessary to allow the macros in tokenizer and parse modules
// to succeed.
#![recursion_limit = "128"]
#[macro_use]
extern crate nom;
#[macro_use]
extern crate nom_locate;
extern crate serde_json;

#[macro_use]
pub mod ast;
#[macro_use]
pub mod tokenizer;
pub mod build;
pub mod convert;
pub mod error;
pub mod parse;

mod format;

pub use ast::Expression;
pub use ast::Statement;
pub use ast::Value;

pub use build::Builder;
pub use build::Val;
pub use parse::parse;
