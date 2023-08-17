use std::path::Path;

use svd2icicle::gen_empty_implementation;

fn main() {
    let svds = [
        // core peripherals for cortex-m4
        Path::new("/home/rbran/src/arm-generic-svd/Cortex-M4.svd"),
        // the DWT
        Path::new("/home/rbran/src/arm-generic-svd/Cortex-M4-DWT.svd"),
        // peripherals for nrf52
        Path::new("/home/rbran/src/nrf-pacs/svds/nrf52832.svd"),
    ];
    gen_empty_implementation(
        &svds,
        Path::new("/home/rbran/src/icicle-nrf52832/src/lib.rs"),
    );
    assert_eq!(
        std::process::Command::new("rustfmt")
            .arg("/home/rbran/src/icicle-nrf52832/src/lib.rs")
            .status()
            .unwrap()
            .code()
            .unwrap(),
        0
    );
}
