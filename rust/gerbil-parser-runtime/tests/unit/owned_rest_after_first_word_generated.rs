pub fn owned_rest_after_first_word(input: &str) -> String {
    input
        .trim()
        .split_once(char::is_whitespace)
        .unwrap_or_default()
        .1
        .trim()
        .to_owned()
}
