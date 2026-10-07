pub fn classify_or_unknown(input: &str, active: &[&str], complete: &[&str]) -> &'static str {
    let classification = classify_first_word(input, active, complete);
    if classification.is_empty() { "unknown" } else { classification }
}
