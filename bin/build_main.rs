extern crate walkdir;

use walkdir::WalkDir;

fn generate_rust_module() -> String {
    let mut rust_lib = String::new();
    rust_lib.push_str("use std::collections::HashMap;\n");
    rust_lib.push_str("pub fn get_libs() -> HashMap<String, &'static str> {\n");
    rust_lib.push_str("\tlet mut stdlib = HashMap::new();\n");
    for entry in WalkDir::new("std").into_iter().filter_map(|e| e.ok()) {
        // Okay we want to add these as include bytes in a simulated
        // filesystem for our binary to use.
        let path = entry.into_path();
        // We only include files that are not test files.
        if path.is_file() && !path.ends_with("_test.ucg") {
            let path_str = path.to_string_lossy();
            let include = format!(
                "\tstdlib.insert(\n\t\t\"{}\".to_string(),\n\t\tinclude_str!(\"../../{}\"));\n",
                path_str, path_str
            );
            rust_lib.push_str(&include);
        }
    }
    rust_lib.push_str("\tstdlib\n");
    rust_lib.push_str("}");
    println!("{}", rust_lib);
    rust_lib
}

fn main() {
    let contents = generate_rust_module();
    std::fs::write("src/build/stdlib.rs", contents.as_bytes()).unwrap();
}
