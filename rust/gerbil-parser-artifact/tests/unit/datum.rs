use super::read;
#[test]
fn metadata_reader_is_inert_and_rejects_duplicate_keys_trailing_and_deep_forms() {
    for source in [
        "(eval 1)",
        "(object (\"x\" 1) (\"x\" 2))",
        "(list 1) 2",
        "(list #.(evil))",
        "(list",
        "\"\\q\"",
    ] {
        assert!(read(source.as_bytes()).is_err(), "{source}");
    }
    let deep = format!("{}0{}", "(list ".repeat(66), ")".repeat(66));
    assert!(read(deep.as_bytes()).is_err());
}
#[test]
fn metadata_reader_preserves_scheme_strings_lists_numbers_and_booleans() {
    let value = read(br#"(object ("s" "a\n\"\x3b1;\\") ("v" (list #t #f 42)))"#).unwrap();
    assert_eq!(value["s"], "a\n\"α\\");
    assert_eq!(value["v"], serde_json::json!([true, false, 42]));
}

#[test]
fn native_gambit_writer_control_character_spellings_are_preserved() {
    for (wire, expected) in [
        (r#""\x0;""#, '\0'),
        (r#""\a""#, '\u{7}'),
        (r#""\b""#, '\u{8}'),
        (r#""\v""#, '\u{b}'),
        (r#""\f""#, '\u{c}'),
        (r#""\x1b;""#, '\u{1b}'),
        (r#""\x7f;""#, '\u{7f}'),
    ] {
        assert_eq!(read(wire.as_bytes()).unwrap(), expected.to_string());
    }
}
