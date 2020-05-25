SHELL = /bin/bash -o pipefail

# some platform compatibility hacks
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))
execsuffix := $(if $(findstring Windows_NT,$(OS)),.exe, )
pathchar := $(if $(findstring Windows_NT,$(OS)),\,/)

sourcefiles := $(call rwildcard,src,*.rs)

all: test build

debugtarget := target$(pathchar)debug$(pathchar)ucg$(execsuffix)
releasetarget := target$(pathchar)release$(pathchar)ucg$(execsuffix)

$(debugtarget): $(sourcefiles)
	cargo build

$(releasetarget): $(sourcefiles)
	cargo build --release

build: $(debugtarget)
	cargo build

buildrelease: $(releasetarget)

test: unit integration stdlibtest 

unit:
	cargo test

integration:
	cargo run -- test -r integration_tests

stdlibtest:
	cargo run -- test -r std$(pathchar)tests

install: test
	cargo install --path . --force

publish: build test
	cargo publish
	(cd docsite; make deploysite)

bench: buildrelease
	hyperfine --warmup=5 --runs=50 "target$(pathchar)release$(pathchar)ucg$(execsuffix) test -r integration_tests"

clean:
	rm -f integration.log stdlibtest.log unittest.log
	cargo clean