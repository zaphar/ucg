# Universal Configuration Grammar - Working Title.

This is an experiment in configuration management. The approach is **not**
to create a "parsable" config file format.  We have plenty of
those. Instead we try to specify a grammar for describing
configuration values that can then target various configuration
formats to output to.

In theory this could support anything from command line flags to json
to yaml or toml or even xml.

The goal is to allow a global shared configuration repository that can
be version controlled, enforce _some_ typesafety, and output
configuration for any application regardless of that applications
preferred format.

## Examples

#### Base Types

UCG has 3 simple types Integer, Float, and String as well as Tuple for complex
types.

    1;
    
    1.0;
    
    "string";
    
    {
        field1 = 1,
        field2 = "strawberry",
    };

To specify that a number is a Float you must include a decimal point. Otherwise
the number will be an integer. UCG does type inference for tuple fields based
off of the value assigned.

### Simple Expressions

UCG supports simple math expressions using `+`, `-`, `*`, `/`) and string
concatenation using `+`. The expressions enforce the same type between operands.

    1 + 1;
    
    1.0 + 1.0;
    
    "foo" + "bar";

### String formatting

UCG supports some string interpolation using format strings. The syntax is
shamelessly ripped off from python.

    "foo @ @ \@" % (1, "bar")

This gets turned into "foo 1 bar {"

### Bindings and Tuples.

Let statements introduce a new name in a UCG file. Most configurations
will be a tuple like below. Tuples are delimited by braces and have a list
of named fields in them.

    let mysql_conn_base = {
      host = "db1.local.net",
      port = 3306,
      database = "place-holder",
    };

Tuple fields have no ordering guarantees. All bindings are immutable and
can not be reassigned to once defined.

### Variables

UCG can reference a binding using variables. Any named value using
a let statement can be referred to with that name within the file it
is introduced. You can descend into a tuple using a dotted syntax.

    let var = "My value";
    let tplvar = {
        field = var,
    }
    let field_var = tplvar.field;

### Copying and modifying Tuples.

Even though all bindings are immutable, Tuples have a copy on modify syntax.
You can use a previously defined tuple as the basis for a new tuple. Doing this
will make a copy of the source tuple and allow you to add new fields
or override an already existing field.

    let mysql_app_conn = mysql_conn_base{
       database = "appdb",
       timeout = 30,
    };

Types are inferred for tuple fields. We enforce type consistency when
overriding a field in a base tuple. The port field below expects an
Integer not a String so you will get a TypeFail error.


    let bad_mysql_conn = mysql_conn_base{
       port = "3307",
     }

### Conditional Values

The grammar has limited support for conditionals using the select expression.

    let my_sql_app_conn = mysql_conn_base{
      port = select "prod", 33007 {
          prod = 3307,
          qa = 3308,
      }
    };

The first argument to the select call is the key you wish to
select. This argument must be a string or an expression that outputs a
string. The second argument is a default value to use if the key
doesn't exist. The third is a set of fields to choose from.

### Macros

We also support a limited macro facility with the macro expression.

    let conn_string_macro = macro (host, port) {
      conn_str = "mysql://" + host + ":" + port,
    }
    
    let web_conn = conn_string_macro ("proddb", "3307");
    let conn_string = web_conn.conn_str;

Macro's always output a tuple whose fields are evaluated at the location they
are called from. You can acccess the generated fields from the resulting tuple
like usual.
