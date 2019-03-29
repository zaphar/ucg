+++
title = "A simple JSON configuration"
slug = "a-json-config"
weight = 1
in_search_index = true
+++

UCG is a pure functional language, with immutable values, and forward type
inference. It can be used to translate it's various datatypes into a number of
common configuration file formats as well as create some environment variables
and command line flags.

One of it's intended use cases is to create json configuration files safely.
Lets imagine a webservice that uses json as it's configuration format. It needs
to know a few things to operate properly.

* The port it should listen on
* The ip address it should listen on
* The database connection information
* The host to check for it's dynamic configuration service (i.e. Consul or Etcd)

The service expects the config file to look something like this

```json
{
    "port": 8888,
    "addr": "0.0.0.0",
    "db": {
        "host": "mydbhost",
        "port": 3306,
        "database": "myservicedb",
    },
    "config_svc": "https://consul.internal:8080"
}
```

To get started we need to create file config.ucg that to hold our services
configuration.

```sh
touch config.ucg
```

Then we create our configuration starting with a UCG tuple.

```
let conf = {
    port = 8888,
    addr = "0.0.0.0",
};
```

This is our first UCG statement. Statements in UCG start with an optional
keyword, in this case `let` and are terminated by a semicolon (`;`).

`let` introduces a named binding, in this case, a tuple. Inside the tuple we
have two keys: `port` and `addr`. Quoting a key is optional in UCG if the key
is a single word with all word characters or `_` and no spaces. If you need
dashes or spaces you can quote the key. The first key `port` is a number,
specifically an integer. the second `addr` is a string.

Now we need to add our database and dynamic configuration service values to our
tuple.

```
let db_confs = import "db/mysql/hosts.ucg";
let consul_hosts = import "services/consul/hosts.ucg".host_pool;

let conf = {
    port = 8888,
    addr = "0.0.0.0",
    db = {
        host = db_confs.host_pool.addr,
        port = db_confs.host_pool.port,
        database = "myservicedb",
    },
    config_svc = consul_hosts.url,
};
```

There are a couple new items here. The first is the UCG import expression.
Import expressions start with the keyword `import` and are followed by a string
specifying the relative or absolute path to a UCG file. Relative import paths
are relative to the file they appear in or if not there then relative the path
specified in the `UCG_IMPORT_PATH` variable. When you import a file all of it's
named bindings are exposed as a UCG tuple. We select specific values from the
`services/consul/hosts.ucg` file as part of the import statement using dot
selectors.

This file assumes that the `db/mysql/hosts.ucg` import contains a named binding
called `host_pool` that is a tuple with an `addr` field and a `port` field. It
also assumes the `services/consul/hosts.ucg` has a host_pool named binding with
a url field.

Imports allow us to share our configuration values for shared services among
all of the configurations for their consumers regardless of the format they
expect.

Let's create those two files now.

```sh
mkdir -p db/mysql/
mkdir -p services/consul
touch db/mysql/hosts.ucg
touch db/services/consul/hosts.ucg
```

In the `db/mysql/hosts.ucg` file add the following.

```
let host_pool = {
    addr = "mysql-pool.internal,
    port = 3306,
};
```

In the `services/consul/hosts.ucg` file add the following.

```
let make_pool = func (addr, port) => {
    addr = addr,
    port = port,
    url = "https://@:@" % (addr, port),
};

let host_pool = make_pool("consul.internal", 8080);
```

Our `services/consul/hosts.ucg` file is a little more interesting that our
previous files. We introduce UCG functions as well as format strings. A UCG
function starts with the `func` keyword a set of argments the fatcomma (`=>`)
and a valid UCG expression. Functions can close over their environment and they
can reference the variables defined in the argument in the expression. This one
constructs a tuple for us.

The function also uses a format string to construct our URL for the consul
service. Format strings start with a template string followed by the `%`
character. They are then followed by a parenthesized list with the items to
substitute in the string.

Format strings also have an alternate form that you can read about in the
reference.

We have now constructed our config format but we don't have a json file yet.
For that we need an `out` statement. Back in our `config.ucg` file add the
following statement at the bottom: `out json conf;`.


```
let db_confs = import "db/mysql/hosts.ucg";
let consul_hosts = import "services/consul/hosts.ucg".host_pool;

let conf = {
    port = 8888,
    addr = "0.0.0.0",
    db = {
        host = db_confs.host_pool.addr,
        port = db_confs.host_pool.port,
        database = "myservicedb",
    },
    config_svc = consul_hosts.url,
};

out json conf;
```

The out statement converts the UCG intermediate representation into the desired
output format. Each output format has a defined translation and in some cases
expected rules that the value should satisfy. Any errors in the translation
will result in a compile error and no output will be generated. In this case we
specify the `json` compile target and the `conf` value we generated.

Running the following will generate a `config.json` file that looks like our
example above.

```sh
ucg build config.ucg
```