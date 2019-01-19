all: test build

build:
	cargo build

test: stdlibtest

unit: 
	cargo test 

integration: unit
	cargo run -- test -r integration_tests 

stdlibtest: integration
	cargo run -- test -r std/tests

install: test
	cargo install --path . --force

publish: build test
	cargo publish