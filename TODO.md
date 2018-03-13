# Major Planned Features

## List processing

* Map over lists
* Filtering lists
* Flags could automatically expand a list of values into a list of flags.
* Joining a lists elements. (i.e. folds)

## Query Language (Experimental)

You should be able to ask the compiler to tell you any value or set of values in the
compiled configuration.

## Translation Language (Experimental)

For some configuration file formats we need a way to specify a particular
organiztion for a given configuration structure (i.e. xml attribute or tag?).

Some options here could be:

* Simple data export (json) 
* A Functional Transform similar to xslt or css transforms.
* A Templating language
* Annotations.

# Minor Fixes and Polish

* Better error messages.
* Allow trailing commas.
* Flags should allow different seperators for prefixed flags.
* YAML export