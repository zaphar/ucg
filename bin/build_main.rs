extern crate walkdir;

use std::path::PathBuf;

use walkdir::WalkDir;

fn generate_rust_module() -> String {
    let mut rust_lib = String::new();
    let out_dir = std::env::var("OUT_DIR").unwrap();
    // NOTE(jwall): Since the generated file will be included using the include! macro
    // This has to be an expression or item. This means we need to enclose it with
    // braces to force it to be a single expression instead of multiple.
    rust_lib.push_str("{");
    for entry in WalkDir::new("std").into_iter().filter_map(|e| e.ok()) {
        // Okay we want to add these as include bytes in a simulated
        // filesystem for our binary to use.
        let path = entry.into_path();
        // We only include files that are not test files.
        let path_str = path.to_string_lossy().to_string();
        if path.is_file() && !path_str.ends_with("_test.ucg") {
            println!("Adding lib file: {}", path_str);
            let out_path = PathBuf::from(format!("{}/{}", out_dir, path_str));
            // We have to copy the file into out out directory to ensure that we
            // have a reliable way for the stdlib.rs module file to include them
            // from.
            std::fs::create_dir_all(out_path.parent().unwrap()).unwrap();
            std::fs::copy(&path_str, &out_path).unwrap();
            let include = format!(
                "\tstdlib.insert(\n\t\t\"{}\".to_string(),\n\t\tinclude_str!(\"{}/{}\"));\n",
                path_str, out_dir, path_str
            );
            rust_lib.push_str(&include);
            rust_lib.push_str("\n");
        }
    }
    rust_lib.push_str("}");
    println!("Finished Adding lib files");
    rust_lib
}

fn main() {
    let contents = generate_rust_module();
    let out_dir = std::env::var("OUT_DIR").unwrap();
    std::fs::write(
        format!("{}/stdlib_generated.rs", out_dir),
        contents.as_bytes(),
    )
    .unwrap();
}
