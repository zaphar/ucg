SHELL = /bin/bash -o pipefail

all: test build

build:
	cargo build

test: unit integration stdlibtest 

rustfiles := $(find . -type f -name '*.rs')
stdlibfiles := $(find std -type f -name '*.ucg')


unit:
	cargo test

integration:
	cargo run -- test -r integration_tests

stdlibtest:
	cargo run -- test -r std/tests

install: test
	cargo install --path . --force

publish: build test
	cargo publish
	(cd docsite; make deploysite)

clean:
	rm -f integration.log stdlibtest.log unittest.log