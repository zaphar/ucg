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

### Bindings and Tuples.
Let statements introduce a new name in a ucg file. Most configurations
will be a tuple like below. Tuples are delimited by braces and have a list
of named fields in them.

    let mysql_conn_base = {
      host = "db1.local.net",
      port = 3306,  // knows the difference between strings and numbers.
      database = "place-holder",
    };

Tuple fields have no ordering guarantees.

### Copying and modifying Tuples.

You can use a previously defined tuple as the basis for a new tuple. Doing
this will make a copy of the source tuple and allow you to add new fields
or override an already existing field.

    let mysql_app_conn = mysql_conn_base{
       database = "appdb",
       timeout = 30,
    };

### Limited Types safety

Types are inferred for tuple fields. We enforce type consistency when
overriding a field in a base tuple. The port field below expects a
number not a string so you will get a TypeFail error.


    let bad_mysql_conn = mysql_conn_base{
       port = "3307",
     }

### Conditional Values

The grammar has limited support for conditionals using the select expression.


    let my_sql_app_conn = mysql_conn_base{
      port = select prod, 33007 {
          prod = 3307,
          qa = 3308,
      }
    };

The first argument to the select call is the key you wish to select. The second
argument is a default value to use if the key doesn't exist. The third is a set
of fields to choose from.

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
