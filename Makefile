SHELL = /bin/bash -o pipefail

all: test build

build:
	cargo build

test: stdlibtest

rustfiles := $(find . -type f -name '*.rs')
stdlibfiles := $(find std -type f -name '*.ucg)

unittest.log: $(rustfiles)
	cargo test | tee unittest.log

unit: unittest.log

integration.log: unit
	cargo run -- test -r integration_tests | tee integration.log

integration: integration.log

stdlibtest.log: $(stdlibfiles)
	cargo run -- test -r std/tests | tee stdlibtest.log

stdlibtest: stdlibtest.log integration

install: test
	cargo install --path . --force

publish: build test
	cargo publish
	(cd docsite; make deploysite)

clean:
	rm -f integration.log stdlibtest.log unittest.log