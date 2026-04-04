#![no_main]
use libfuzzer_sys::fuzz_target;
use ucglib::iter::OffsetStrIter;
use ucglib::parse::parse;

fuzz_target!(|data: &str| {
    let iter = OffsetStrIter::new(data);
    let _ = parse(iter, None);
});
