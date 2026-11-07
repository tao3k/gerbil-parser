use super::{ModuleTextProfile, PreparedModuleSource};
static UNIT: ModuleTextProfile = ModuleTextProfile {
    header_border: "~~~",
    header_word: "UNIT",
    end_border: "!!!",
    block_open: "(*",
    block_close: "*)",
    line_comment: "\\*",
    name_extra: "_",
};
#[test]
fn numeric_extra_character_is_an_explicit_name_role() {
    static NUMERIC: ModuleTextProfile = ModuleTextProfile {
        name_extra: "0",
        ..UNIT
    };
    assert_eq!(
        PreparedModuleSource::new("~~~ UNIT 000 ~~~", &NUMERIC).end(0),
        None
    );
    assert_eq!(
        PreparedModuleSource::new("~~~ UNIT 111 ~~~", &NUMERIC).end(0),
        Some(16)
    );
}
#[test]
fn long_border_run_has_bounded_records_and_independent_source_ownership() {
    let source = format!("{} UNIT 字 ~~~\n!!!\nfooter", "~".repeat(100_000));
    let prepared = PreparedModuleSource::new(&source, &UNIT);
    assert_eq!(prepared.headers.len(), 1);
    assert_eq!(prepared.events.len(), 2);
    assert_eq!(prepared.end(0), None);
    assert_eq!(prepared.end(source.len() - 6), Some(source.len()));
    std::thread::scope(|scope| {
        for input in ["preamble\n~~~ UNIT Demo ~~~", "~~~ UNIT 字 ~~~\n!!!\nλ"] {
            scope.spawn(move || {
                let own = PreparedModuleSource::new(input, &UNIT);
                for _ in 0..100 {
                    assert_eq!(
                        own.end(0),
                        if input.starts_with("preamble") {
                            Some(9)
                        } else {
                            None
                        }
                    );
                }
            });
        }
    });
}
