use super::{lexer::lexical_end, model::LexicalExpr};

const NUMBER: LexicalExpr = LexicalExpr::NumberLiteral {
    prefixes: &["0x", "0o", "0b"],
    separator: "_",
    suffixes: &["M", "F", "D"],
    leading_period: true,
    trailing_period: true,
};

#[test]
fn profiled_numbers_match_the_scheme_boundaries() {
    assert_eq!(lexical_end(&NUMBER, "0xCA_FE", 0), Some(7));
    assert_eq!(lexical_end(&NUMBER, "0o7_55", 0), Some(6));
    assert_eq!(lexical_end(&NUMBER, "0b10_01", 0), Some(7));
    assert_eq!(lexical_end(&NUMBER, "1_000.5E+2F", 0), Some(11));
    assert_eq!(lexical_end(&NUMBER, ".5M", 0), Some(3));
    assert_eq!(lexical_end(&NUMBER, "0x", 0), Some(1));
}

#[test]
fn quoted_strings_preserve_doubled_and_backslash_escapes() {
    let expression = LexicalExpr::QuotedString(&["\"", "'", "`"]);
    assert_eq!(lexical_end(&expression, "`a``b`", 0), Some(6));
    assert_eq!(lexical_end(&expression, "'Ada''s graph'", 0), Some(14));
    assert_eq!(lexical_end(&expression, r#""a\"b""#, 0), Some(6));
    assert_eq!(lexical_end(&expression, "`unterminated", 0), None);
}

#[test]
fn escaped_quoted_strings_leave_adjacent_strings_separate() {
    let expression = LexicalExpr::EscapedQuotedString(&["\""]);
    assert_eq!(lexical_end(&expression, "\"a\"\"b\"", 0), Some(3));
    assert_eq!(lexical_end(&expression, r#""a\"b""#, 0), Some(6));
    assert_eq!(lexical_end(&expression, "\"unterminated", 0), None);
}

#[test]
fn comments_and_choices_take_the_longest_complete_match() {
    let expression = LexicalExpr::Choice(&[
        LexicalExpr::LineComment(&["//", "#"]),
        LexicalExpr::BlockComment {
            opening: "/*",
            closing: "*/",
        },
    ]);
    assert_eq!(lexical_end(&expression, "// note\nnext", 0), Some(7));
    assert_eq!(lexical_end(&expression, "/* note */next", 0), Some(10));
    assert_eq!(lexical_end(&expression, "/* open", 0), None);
}

#[test]
fn nested_comments_and_heredocs_close_losslessly() {
    let nested = LexicalExpr::NestedBlockComment {
        opening: "/*",
        closing: "*/",
    };
    assert_eq!(lexical_end(&nested, "/* /* */ */", 0), Some(11));
    assert_eq!(
        lexical_end(&LexicalExpr::Heredoc, "<<EOF\nvalue\nEOF", 0),
        Some(15)
    );
}

#[test]
fn line_primitive_preserves_crlf_lf_cr_and_utf8_boundaries() {
    let expression = LexicalExpr::Line;
    assert_eq!(lexical_end(&expression, "é\r\nnext", 0), Some(4));
    assert_eq!(lexical_end(&expression, "é\r\nnext", 4), Some(8));
    assert_eq!(lexical_end(&expression, "one\ntwo", 0), Some(4));
    assert_eq!(lexical_end(&expression, "one\rtwo", 0), Some(4));
    assert_eq!(lexical_end(&expression, "last", 0), Some(4));
    assert_eq!(lexical_end(&expression, "", 0), None);
}

#[test]
fn delimiter_bounded_atom_preserves_utf8_without_consuming_syntax() {
    let atom = LexicalExpr::UntilDelimiters(" \t\r\n();\"");
    assert_eq!(lexical_end(&atom, ":scope) tail", 0), Some(6));
    assert_eq!(lexical_end(&atom, "π-link; note", 0), Some(7));
    assert_eq!(lexical_end(&atom, "π\u{a0}next", 0), Some(2));
    assert_eq!(lexical_end(&atom, ")", 0), None);
}

#[test]
fn character_run_uses_maximal_extent_and_enforces_minimum() {
    let hyphens = LexicalExpr::CharacterRun {
        character: "-",
        minimum: 4,
    };
    assert_eq!(lexical_end(&hyphens, "---- MODULE", 0), Some(4));
    assert_eq!(lexical_end(&hyphens, "------- MODULE", 0), Some(7));
    assert_eq!(lexical_end(&hyphens, "--- MODULE", 0), None);
    assert_eq!(lexical_end(&hyphens, "x----", 1), Some(5));
    let unicode = LexicalExpr::CharacterRun {
        character: "é",
        minimum: 2,
    };
    assert_eq!(lexical_end(&unicode, "ééx", 0), Some(4));
}

#[test]
fn strict_quoted_profile_validates_escape_width_and_byte_offsets() {
    let profile = LexicalExpr::QuotedStringProfile {
        delimiter: "'",
        escapes: "`'\\/fnrt",
        unicode_width: 4,
    };
    for (source, end) in [
        ("'α'next", Some(4)),
        ("'a''b'", Some(3)),
        ("'a\nb'", Some(5)),
        ("'\\n'", Some(4)),
        ("'\\u0041'", Some(8)),
        ("'\\u١٢٣٤'", Some(12)),
        ("'\\u00411'", Some(9)),
        ("'\\x'", None),
        ("'\\u123'", None),
        ("'\\uGGGG'", None),
        ("'\\u²Ⅻ½４'", None),
        ("'\\", None),
        ("'", None),
    ] {
        assert_eq!(lexical_end(&profile, source, 0), end, "{source:?}");
    }
    assert_eq!(lexical_end(&profile, "x'α'", 1), Some(5));
    let unicode_quote = LexicalExpr::QuotedStringProfile {
        delimiter: "§",
        escapes: "",
        unicode_width: 0,
    };
    assert_eq!(lexical_end(&unicode_quote, "§α§tail", 0), Some(6));
    let no_unicode = LexicalExpr::QuotedStringProfile {
        delimiter: "'",
        escapes: "",
        unicode_width: 0,
    };
    assert_eq!(lexical_end(&no_unicode, "'\\u0041'", 0), None);
}

#[path = "../fixtures/quoted_profile_generated.rs"]
mod quoted_profile_generated;

#[test]
fn scheme_generated_profile_grammar_accepts_and_rejects_strict_escapes() {
    for source in ["'α'", "`name`", "'\\u0041'", "'\\u١٢٣٤'", "'a\nb'"] {
        let parsed = crate::parse(&quoted_profile_generated::LANGUAGE, source).unwrap();
        assert_eq!(parsed.syntax().to_string(), source);
    }
    for source in ["'\\q'", "'\\u123'", "'a''b'", "`unterminated"] {
        assert!(
            crate::parse(&quoted_profile_generated::LANGUAGE, source).is_err(),
            "{source:?}"
        );
    }
}

#[test]
fn malformed_quoted_profiles_fail_product_admission() {
    for (delimiter, unicode_width) in [("", 4), ("''", 4), ("'", 9)] {
        let rules = Box::leak(Box::new([crate::LexicalRule {
            terminal: "string",
            expression: LexicalExpr::QuotedStringProfile {
                delimiter,
                escapes: "",
                unicode_width,
            },
            precedence: 0,
            extra: false,
        }]));
        let language = Box::leak(Box::new(crate::LanguageSpec {
            lexical_rules: rules,
            ..quoted_profile_generated::LANGUAGE
        }));
        let error = crate::parse(language, "''").unwrap_err();
        assert_eq!(error.diagnostic.reason_kind, "invalid-aot-artifact");
    }
}
