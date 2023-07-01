pub fn snake_case(name: &str) -> String {
    //TODO add '_' between uppercase and lowercase
    name.to_lowercase()
}

pub fn camel_case(name: &str) -> String{
    //TODO remove '_' and similar
    format!(
        "{}{}",
        name.chars().next().unwrap().to_uppercase().to_string(),
        name[1..].to_lowercase(),
    )
}

pub fn dim_to_n(value: &str) -> String {
    let output = value.replace("%s", "n");
    output.replace(&['[', ']'], "")
}
