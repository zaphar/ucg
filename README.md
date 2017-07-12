# Universal Configuration Generator. - A working title.

This is an experiment in configuration management. The approach is not
to create a "parsable" config file format.  We have plenty of
those. Instead we try to specify a grammar for describing
configuration values that can then target various configuration
formats to output to.

In theory this could support anything from command line flags to json
to yaml or toml or even xml.

The goal is to allow a global shared configuration repository that can
be version controlled, enforce some typesafety, and output
configuration for any application regardless of that applications
preferred format.

## Examples

Let statements introduce a new name in a ucg file. Most configurations
will be a tuple like below. Tuples are delimited by braces and have a list
of named fields in them.

    let mysql_conn_base = {
      host = "db1.local.net",
      port = 3306,  // knows the difference between strings and numbers.
      database = "place-holder",
    };

You can use a previously defined tuple as the basis for a new tuple. Doing
this will make a copy of the source tuple and allow you to add new fields
or override an already existing field.

    let mysql_app_conn = mysql_conn_base{
       database = "appdb",
       timeout = 30,
    };

Types are inferred for tuple fields. We enforce type consistency when
overriding a field in a base tuple. The port field below expects a
number not a string so you will get a TypeFail error.


    let bad_mysql_conn = mysql_conn_base{
       port = "3307",
     }
