use proc_macro2::Ident;
use quote::format_ident;

pub fn snake_case(name: &str) -> Ident {
    format_ident!("{}", name.to_lowercase())
}

pub fn camel_case(name: &str) -> Ident {
    format_ident!(
        "{}{}",
        name.chars().next().unwrap().to_uppercase().to_string(),
        name[1..].to_lowercase(),
    )
}

pub fn dim_to_N(value: &str) -> String {
    let output = value.replace("%s", "n");
    output.replace(&['[', ']'], "")
}
