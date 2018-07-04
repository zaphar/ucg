#![allow(unused_must_use)]

#[macro_use]
extern crate bencher;
extern crate cpuprofiler;
extern crate nom_locate;
extern crate ucglib;

use bencher::Bencher;
//use cpuprofiler::PROFILER;

use ucglib::parse::*;

fn do_parse(i: &str) {
    parse(nom_locate::LocatedSpan::new(i));
}

fn parse_int(b: &mut Bencher) {
    b.iter(|| do_parse("1;"));
}

fn parse_whole_float(b: &mut Bencher) {
    b.iter(|| do_parse("1.0;"));
}

fn parse_pre_partial_float(b: &mut Bencher) {
    b.iter(|| do_parse("1.;"));
}

fn parse_post_partial_float(b: &mut Bencher) {
    b.iter(|| do_parse(".0;"));
}

fn parse_bareword(b: &mut Bencher) {
    b.iter(|| do_parse("foo;"));
}

fn parse_string(b: &mut Bencher) {
    b.iter(|| do_parse("\"foo\";"));
}

fn parse_simple_tuple(b: &mut Bencher) {
    b.iter(|| do_parse("{foo = 1, bar = \"2\",};"));
}

fn parse_complex_tuple(b: &mut Bencher) {
    b.iter(|| do_parse("{foo = 1, { bar = \"2\",}};"));
}

fn parse_simple_list(b: &mut Bencher) {
    //PROFILER
    //    .lock()
    //    .unwrap()
    //    .start("./my-parse_list.profile")
    //    .unwrap();
    b.iter(|| do_parse("[1, 2, 3];"));
    //PROFILER.lock().unwrap().stop().unwrap();
}

fn parse_complex_tuple_list(b: &mut Bencher) {
    //PROFILER
    //    .lock()
    //    .unwrap()
    //    .start("./my-parse_list.profile")
    //    .unwrap();
    let input = "[{foo=1}, {bar=2}, {quux=4}];";
    b.iter(|| do_parse(input));
    //PROFILER.lock().unwrap().stop().unwrap();
}

fn parse_complex_nested_list(b: &mut Bencher) {
    //PROFILER
    //    .lock()
    //    .unwrap()
    //    .start("./my-parse_list.profile")
    //    .unwrap();
    let input = "[[1,2], [3,4], [4,5]];";
    b.iter(|| do_parse(input));
    //PROFILER.lock().unwrap().stop().unwrap();
}

fn parse_selector_tuple_head(b: &mut Bencher) {
    b.iter(|| do_parse("{foo=1}.foo;"));
}

fn parse_selector_symbol_head(b: &mut Bencher) {
    b.iter(|| do_parse("bar.foo;"));
}

benchmark_group!(
    benches,
    parse_int,
    parse_whole_float,
    parse_pre_partial_float,
    parse_post_partial_float,
    parse_bareword,
    parse_string,
    parse_simple_tuple,
    parse_complex_tuple,
    parse_simple_list,
    parse_complex_tuple_list,
    parse_complex_nested_list,
    parse_selector_tuple_head,
    parse_selector_symbol_head,
);

benchmark_main!(benches);
