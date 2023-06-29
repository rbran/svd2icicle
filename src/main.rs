use svd2icicle::gen_empty_implementation;

fn main() {
    gen_empty_implementation(
        "/home/rbran/src/nrf-pacs/svds/nrf51.svd",
        "/home/rbran/src/icicle-nrf51/src/lib.rs",
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
