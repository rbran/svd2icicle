use std::path::{Path, PathBuf};

use svd2icicle::gen_empty_implementation;

fn main() {
    let mut args = std::env::args();
    let _exec_file = args.next().unwrap();
    let output_file = args.next().expect("Output file missing");
    let files: Vec<_> = args.map(PathBuf::from).collect();
    if files.is_empty() {
        panic!("Input file(s) missing")
    }
    gen_empty_implementation(
        files.iter().map(PathBuf::as_path),
        Path::new(&output_file),
    );
}
