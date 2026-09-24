pub fn owned_first_word(input: &str, admitted: bool) -> String {
    if admitted { input.split_whitespace().next().unwrap_or("").to_owned() } else { String::new() }
}
