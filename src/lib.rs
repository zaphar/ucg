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
//! composable, with copy-on-write semantics, and safe.
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
//! A valid ucg file is composed of a series of statements. Statements start with an
//! optional keyword and terminate with a semicolon.
//!
//! ### Reserved words
//!
//! The following words are reserved in ucg and can't be used as named bindings.
//!
//! * self
//! * assert
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
//! * out
//!
//! ## Types
//!
//! ### Primitive types
//!
//! ucg has a relatively simple syntax with just a few primitive types, Null, Boolean, Int, Float, and String.
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
//! Lists are surrounded with square brackets `[ ]` and have comma separated elements. Trailing
//! commas are permitted in lists.
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
//! #### Tuple
//!
//! Tuple's are an ordered collection of name, value pairs. They are bounded by curly braces `{ }`
//! and contain name = value pairs separated by commas. Trailing commas are permitted. The name must
//! be either a bareword without quotes or a quoted string.
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
//! The head of a selector can be a tuple or list or symbol. Optionally a selector can also be followed by either a
//! bareword or string to index a tuple field or an integer to index a list position.
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
//! ##### The Environment Selector
//!
//! The `env` selector is a reserved selector that always contains a tuple with any environment
//! variables in it.
//!
//! Attempting to reference an enviroment selector field that does not exist is a compile error.
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
//! The `>`, `<`, `>=`, and `>=` operators are only supported on numeric types (i.e. int, and float).
//!
//! ```ucg
//! 1 > 2; // result is false
//! 2 < 3; // result is true
//! 10 > "9"; // This is a compile error.
//! (1+2) == 3
//! ```
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
//! When you are copying a tuple you can use the special symbol  `self` to refer to the tuple you are copying.
//! This allows you to override fields in nested tuples with an concise syntax. Self always refers
//! to the innermost tuple you are copying.
//!
//! ```ucg
//! let nestedtpl = {
//!     field1 = "value1",
//!     inner = {
//!         field2 = 2
//!         inner = {
//!             field3 = "three",
//!         },
//!     },
//! };
//!
//! let copiedtpl = nestedtpl{
//!     inner = self.inner{
//!         inner = self.inner{
//!             field4 = 4,
//!         },
//!     },
//! };
//! ```
//!
//! #### Conditional data
//!
//! ucg supports a limited form of conditional data selection of using the select expression. A select expression
//! starts with the `select` keyword and is followed by a an expression resolving to a string naming the field to
//! select, an expression resolving to the default value, and a tuple to select the field from. If the field selected
//! is not in the tuple then the default value will be used.
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
//! Macros do not close over their environment so they can only reference values defined in their arguments. They can't refer to bindings
//! or other macros defined elsewhere. They are useful for constructing tuples of a certain shape or otherwise promoting data reuse.
//! You define a macro with the `macro` keyword followed by the arguments in parentheses, a `=>`, and then a tuple.
//!
//! ```ucg
//! let mymacro = macro (arg1, arg2) => {
//!     host = arg1,
//!     port = arg2,
//!     connstr = "couchdb://@:@" % (arg1, arg2),
//! }
//!
//! let my_dbconf = mymacro("couchdb.example.org", "9090");
//!
//! let my_dbhost = dbconf.host;
//! ```
//!
//! macros always resolve to a tuple. If you want to get a single value out you can use selector syntax to retrieve it.
//!
//! ##### List macros
//!
//! ucg supports a couple of ways to use macros for mapping or filtering a list to a new list.
//!
//! A map expression starts with the map keyword followed by the name of a macro that takes exactly
//! one argument, a `.`, and the name of the output field for the macro. ucg will apply the macro
//! to each element of the list and then take the output field from the resulting tuple and append
//! it to the resulting list. If the output field does not exist in the macro output tuple it will
//! be a compile error.
//!
//! ```ucg
//! let list = [1, 2, 3, 4];
//! let mapper = macro(item) => { result = item + 1 };
//!
//! // results in: [2, 3, 4, 5]
//! let mapped = map mapper.result list;
//! ```
//
//! A filter expression starts with the filter keyword followed by the name of a macro with exactly
//! one argument, a `.`, and the name of the output field for the macro. The filter will apply the
//! macro to each element of the list and if the output field is a value that is not NULL then the
//! list element is appended to the output list. If the output field returns a NULL Value or Boolean
//! false then the element is not appended to the output list. If the output field does not exist in
//! the macro it will be a compile error.
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
//! ### Statements
//!
//! There are several kinds of statements in a ucg file. expression statements, let statements, import statements,
//! assert statements, and out statements. All ucg statements must be terminated by a semicolon.
//!
//! #### Expression statements
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
//! #### Named value statements
//!
//! There are two statements that can introduce a named value for a given ucg file. Let statnements and import statements.
//! Any collisions in binding names inside a file are treated as compile errors. Bindings are immutable and once bound they
//! can't be modified.
//!
//!  * Let statements
//!
//! The let statement binds the result of any valid expression to a name. It starts with the `let` keyword and is followed by
//! the name of the binding, an `=`, and a valid ucg expression.
//!
//! ```ucg
//! let name = "foo";
//! ```
//!
//! * Import statement
//!
//! The import statement imports the contents of another ucg file into the current file with a name. The imported files named
//! values are exposed as a tuple in the referencing file. It starts with the `import` keyword and is followed by a quoted path
//! to the ucg file, the keyword `as`, and a name for the imported values.
//!
//! ```ucg
//! import "dbconfigs.ucg" as dbconfigs;
//!
//! let mysqlconf = dbconfigs.mysql;
//! ```
//!
//! #### Statements to generate output.
//!
//! Some statements in ucg exist to generate an output. Either a compiled configuration or the results of test assertions.
//!
//! * Assert statement
//!
//! The assert statement defines an expression that must evaluate to either true or false. Assert statements are noops except
//! during a validation compile. They give you a way to assert certains properties about your data and can be used as a form
//! of unit testing for your configurations. It starts with the assert keyword followed by a valid block of ucg statements
//! delimited by `|` characters. The final statement in the in the block must evaluate to a boolean expression.
//!
//! ```ucg
//! assert |
//!     host == "www.example.com";
//! |;
//! assert |
//!     select qa, 443, {
//!       qa = 80,
//!       prod = 443,
//!     } == 443;
//! |;
//! ```
//!
//! When _test.ucg files are run in a validation run then ucg will output a log of all the assertions
//! to stdout. Giving you a simple test harness for your ucg configs.
//!
//! * Out statement
//!
//! The out statement defines the output for a UCG file. It identifies the output converter type and an
//! expression that will be output. The output converter type is expected to be one of the registered converters
//! (e.g. json, exec) and the artifact file will take the same name as the ucg file with the extension replaced by
//! the defined extension for that converter.
//!
//! For a file named api_config.ucg with the following contents:
//!
//! ```ucg
//! let myconf = {
//!     api_url = "https://example.org/api/v1/",
//!     api_token = env.API_TOKEN,
//! };
//! out json myconf;
//! ```
//!
//! ucg will output the myconf tuple as json to a file called api_config.json
//!
//! ### Converters
//!
//! Converters convert the ucg intermediate format into an output artifact. Converters define two different things.
//! A conversion for UCG's Intermediate Representation Val's and a file extension for output artifacts.
//!
//! * json: `json`, Serialized json datastructure.
//! * flags: `txt`, command line flags suitable for appending to a command line as arguments.
//! * env: `env`, a list of environment variables posix shell style.
//! * exec: `sh`, a bash script that will `exec` an executable with arguments and environment variables set.

// The following is necessary to allow the macros in tokenizer and parse modules
// to succeed.
#![recursion_limit = "128"]
#[macro_use]
extern crate abortable_parser;
extern crate serde_json;
extern crate serde_yaml;
extern crate simple_error;
extern crate toml;

#[macro_use]
pub mod ast;
#[macro_use]
pub mod tokenizer;
pub mod build;
pub mod convert;
pub mod error;
pub mod iter;
pub mod parse;

mod format;

pub use ast::Expression;
pub use ast::Statement;
pub use ast::Value;

pub use build::Builder;
pub use build::Val;
pub use parse::parse;
