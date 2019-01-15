use std::collections::HashMap;

pub fn get_libs() -> HashMap<String, &'static str> {
    let mut stdlib = HashMap::new();
    include!(concat!(env!("OUT_DIR"), "/stdlib_generated.rs"));
    stdlib
}
