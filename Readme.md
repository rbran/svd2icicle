# Svd2Icicle

This project convert multiple [svd](https://www.keil.com/pack/doc/CMSIS/SVD/html/index.html)
files into a single peripheral struct compatible with [icicle-emu](https://github.com/icicle-emu/icicle-emu).

## How to use

To generate the files use the compiled binary, the first arguments should be
the output `lib.rs` file, all other arguments are svd files used to generate
the peripheral struct.

The output file is not formatted, is possible prettify it using
[cargo-fmt](https://github.com/rust-lang/rustfmt), it's also possible to split
the file using the [form](https://github.com/djmcgill/form) command.

```
cargo run "YOUR_PROJECT_HERE/src/lib.rs" "svd_file1.svd" "svd_file2.svd" "svd_fileN.svd"
cd YOUR_PROJECT_HERE
form -i src/lib.rs -o src
cargo fmt
```

The result files should can be edited afterwards to implemented the required
behavior.

## Example

The project [icicle-nrf52832](https://github.com/rbran/icicle-nrf52832/)
is an example of a partial implementation of a nrf52832 VM generated using this
tool.
