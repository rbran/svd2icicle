use std::path::Path;

use svd2icicle::gen_empty_implementation;

fn main() {
    let svds = [
        // core peripherals for cortex-m4
        Path::new("/home/rbran/src/arm-generic-svd/Cortex-M4.svd"),
        // peripherals for nrf51
        Path::new("/home/rbran/src/nrf-pacs/svds/nrf51.svd"),
    ];
    gen_empty_implementation(
        &svds,
        Path::new("/home/rbran/src/icicle-nrf51/src/lib.rs"),
    )
    .unwrap();
    assert_eq!(
        std::process::Command::new("rustfmt")
            .arg("/home/rbran/src/icicle-nrf51/src/lib.rs")
            .status()
            .unwrap()
            .code()
            .unwrap(),
        0
    );
}
