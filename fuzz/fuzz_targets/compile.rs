#![no_main]
use libfuzzer_sys::fuzz_target;
use std::cell::RefCell;
use std::io;
use ucglib::build::opcode::Environment;
use ucglib::build::FileBuilder;
use ucglib::iter::OffsetStrIter;
use ucglib::parse::parse;

fuzz_target!(|data: &str| {
    // Parse first — bail early on syntax errors so the fuzzer focuses
    // on inputs that reach the compiler.
    let iter = OffsetStrIter::new(data);
    let stmts = match parse(iter, None) {
        Ok(stmts) => stmts,
        Err(_) => return,
    };

    // Set up a minimal compilation environment with no filesystem access.
    // stdout and stderr go to sink so output expressions don't cause I/O.
    let env = RefCell::new(Environment::new(io::sink(), io::sink()));
    let import_paths = vec![];
    let mut builder = FileBuilder::new(".", &import_paths, &env);

    // Run the compiler. We don't care about errors — only panics,
    // stack overflows, and infinite loops are bugs.
    let _ = builder.eval_stmts(stmts, None);
});
