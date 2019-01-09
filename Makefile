all: test build

build:
	cargo build

test: unit integration stdlibtest

unit: 
	cargo test 

integration: unit
	cargo run -- test -r integration_tests 

stdlibtest: integration
	cargo run -- test -r std/tests 