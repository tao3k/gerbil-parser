pub fn classify_first_word(input: &str, active: &[&str], complete: &[&str]) -> &'static str {
    let candidate = input.split_whitespace().next().unwrap_or("");
    if active.contains(&candidate) { "active" } else if complete.contains(&candidate) { "complete" } else { "" }
}
