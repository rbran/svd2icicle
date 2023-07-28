pub fn snake_case(name: &str) -> String {
    //TODO add '_' between uppercase and lowercase
    name.to_lowercase()
}

#[allow(dead_code)]
pub fn camel_case(name: &str) -> String {
    //TODO remove '_' and similar
    format!(
        "{}{}",
        name.chars().next().unwrap().to_uppercase(),
        name[1..].to_lowercase(),
    )
}

pub fn dim_to_n(value: &str) -> String {
    let output = value.replace("%s", "n");
    output.replace(['[', ']'], "")
}
