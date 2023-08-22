pub fn snake_case(name: &str) -> String {
    // input is all uppercase letters
    let all_upper = name.chars().all(|c| {
        if c.is_alphabetic() {
            c.is_uppercase()
        } else {
            true
        }
    });
    // if the input is all uppercase letters, just convert directly into
    // lowercase
    if all_upper {
        return name.to_lowercase();
    }

    let mut output = String::new();
    let mut chars = name.chars();
    if let Some(first) = chars.next() {
        output.extend(first.to_lowercase());
    }

    for letter in chars {
        if letter.is_uppercase() {
            output.push('_');
            output.extend(letter.to_lowercase());
        } else {
            output.push(letter)
        }
    }

    output
}

#[allow(dead_code)]
pub fn camel_case(name: &str) -> String {
    let mut output = String::new();
    let mut chars = name.chars();
    if let Some(first) = chars.next() {
        output.extend(first.to_uppercase());
    }

    let mut last_sep = false;
    for letter in chars {
        if letter == '_' {
            last_sep = true;
            continue;
        }
        if last_sep {
            output.extend(letter.to_uppercase())
        } else {
            output.push(letter)
        }
        last_sep = false;
    }

    output
}

pub fn dim_to_n(value: &str) -> String {
    let output = value.replace("%s", "n");
    output.replace(['[', ']'], "")
}
