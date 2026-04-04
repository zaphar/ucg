#![no_main]
use libfuzzer_sys::fuzz_target;
use ucglib::iter::OffsetStrIter;
use ucglib::tokenizer::tokenize;

fuzz_target!(|data: &str| {
    let iter = OffsetStrIter::new(data);
    let _ = tokenize(iter, None);
});
