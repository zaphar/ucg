+++
title = "Creating a launch script for a docker container"
weight = 2
sort_by = "weight"
in_search_index = true
+++
Let's say we wanted to use UCG generate a launch script for a local jupyter datascience
notebook container. This HowTo walks you through creating a UCG file that can generate
a script to launch the container.

First lets define some docker configuration values that we'll use later.

```
let container_conf = {
    port = 8888,
    hostMount = "~/iJulia/notebooks",
    mountPoint = "/var/iJulia/notebooks",
};
```

Then we can define a helper function for creating our host and port mappings

```
// A little helper func for creating our host and port mappings.
let map_to_container = func (host, container) => {
    result = "@:@" % (host, container)
};

let publish = map_to_container(container_conf.port, container_conf.port);
let volumes = map_to_container(container_conf.hostMount, container_conf.mountPoint);
```

Now we set up our docker run flags.

```
let docker_flags = {
    // d because we want this container to daemonize.
    d = NULL, // no value for this one.
    // We provide the container name from the environment.
    name = env.CONTAINER_NAME,
    // We use our generated publish and volume mappings from above.
    publish = publish.result,
    volume = volumes.result,
};
```

Finally we are ready to define our jupyter specific flags.

```
let jupyter_flags = {
    notebook-dir = container_conf.mountPoint
};
```

Finally we tie it all together into a script and output it as a bash script using
the exec converter.

```
let script = {
    command = "docker",
    args = [
        "run", "--rm", docker_flags,
        "jupyter/datascience-notebook",
        "jupyter", "notebook", jupyter_flags,
    ],
};

out exec script;
```

The script tuple above will generate the following bash script:

```sh
#!/usr/bin/env bash
# Turn on unofficial Bash-Strict-Mode
set -euo pipefail

exec docker run --rm -d --name '<from-env>' --publish '8888:8888' --volume '~/iJulia/notebooks:/var/iJulia/notebooks' jupyter/datascience-notebook jupyter notebook --notebook-dir '/var/iJulia/notebooks'
```

The items in the args should be either strings or tuples. The tuples are turned into
flags using the builtin flag converter.