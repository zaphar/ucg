#!/bin/bash
# AUTHOR:  Jeremy Wall (jw), jeremy@marzhillstudios.com
set -x

cmd=$1

cargo build --release
sudo dtrace -c "target/release/ucg ${cmd}" -o out.stacks -n 'profile-997 /execname == "ucg"/ { @[ustack(100)] = count(); }'
stackcollapse.pl out.stacks > collapsed.stacks
cat collapsed.stacks | flamegraph.pl --minwidth 2.5 > perf_graph.svg
rm -f out.stacks collapsed.stacks
