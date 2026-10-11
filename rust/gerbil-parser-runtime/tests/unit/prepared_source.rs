use super::{LexicalExpr, PreparedLexicalSource, lexical_end_prepared};
const FIELD: LexicalExpr = LexicalExpr::HeaderDelimiter {
    prefix: "記",
    count: 2,
    index: 0,
};
const BODY: LexicalExpr = LexicalExpr::Choice(&[
    LexicalExpr::HeaderData {
        prefix: "記",
        count: 2,
        stops: ";",
    },
    LexicalExpr::HeaderDelimiter {
        prefix: "記",
        count: 2,
        index: 1,
    },
]);
#[test]
fn prepared_headers_deduplicate_nested_rules_and_cache_invalid_results() {
    let good = PreparedLexicalSource::new("記|§α|", [&FIELD, &BODY]);
    assert_eq!(good.headers.len(), 1);
    assert_eq!(good.header("記", 2), Some("|§"));
    assert_eq!(lexical_end_prepared(&FIELD, 3, &good), Some(4));
    assert_eq!(lexical_end_prepared(&BODY, 6, &good), Some(8));
    let bad = PreparedLexicalSource::new("記||α|", [&FIELD, &BODY]);
    assert_eq!(bad.headers.len(), 1);
    assert_eq!(bad.header("記", 2), None);
    assert_eq!(lexical_end_prepared(&FIELD, 3, &bad), None);
}
#[test]
fn concurrent_prepared_sources_keep_borrowed_captures_independent() {
    std::thread::scope(|scope| {
        for source in ["記|§α|", "記*$α*", "記||α|"] {
            scope.spawn(move || {
                let prepared = PreparedLexicalSource::new(source, [&FIELD, &BODY]);
                for _ in 0..100 {
                    let end = lexical_end_prepared(&FIELD, 3, &prepared);
                    assert_eq!(end, (!source.starts_with("記||")).then_some(4));
                }
            });
        }
    });
}
