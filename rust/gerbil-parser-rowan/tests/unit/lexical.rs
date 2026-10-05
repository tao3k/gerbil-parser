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

#[path = "../fixtures/generated/fhirpath_profile.rs"]
mod fhirpath_profile_generated;

#[test]
fn scheme_generated_text_profiles_preserve_character_endpoints_as_byte_spans() {
    for &(terminal, source, expected) in fhirpath_profile_generated::PROFILE_CASES {
        let rule = fhirpath_profile_generated::LANGUAGE
            .lexical_rules
            .iter()
            .find(|rule| rule.terminal == terminal)
            .unwrap();
        let byte_end = expected.map(|end| source.chars().take(end).map(char::len_utf8).sum());
        assert_eq!(
            lexical_end(&rule.expression, source, 0),
            byte_end,
            "{terminal}: {source:?}"
        );
        let prefixed = format!("α{source}");
        assert_eq!(
            lexical_end(&rule.expression, &prefixed, "α".len()),
            byte_end.map(|end| end + "α".len()),
            "{terminal}: {prefixed:?}"
        );
    }
}

#[test]
fn text_profile_offsets_reject_out_of_bounds_and_partial_utf8_characters() {
    let profile = LexicalExpr::TextProfile(&crate::TextProfile::Run {
        class: crate::TextClass::Numeric,
        minimum: 1,
        maximum: None,
    });
    assert_eq!(lexical_end(&profile, "α1", 1), None);
    assert_eq!(lexical_end(&profile, "1", 2), None);
    assert_eq!(lexical_end(&profile, "1", 1), None);
}

#[test]
fn full_fhirpath_aot_uses_shared_profiles_for_lossless_acceptance_and_rejection() {
    for source in [
        "Patient.name",
        "12.34",
        "@2024-12-31",
        "@2024T12:34:56Z",
        "@T12:34:56.12",
        "'α'",
        "١٢.٣٤",
    ] {
        let parsed = crate::parse(&fhirpath_profile_generated::LANGUAGE, source).unwrap();
        assert_eq!(parsed.syntax().to_string(), source);
    }
    for source in ["@2024-", "@2024T1", "@T12:", "@2024T12+0:00", "1.", "'\\q'"] {
        assert!(
            crate::parse(&fhirpath_profile_generated::LANGUAGE, source).is_err(),
            "{source:?}"
        );
    }
}

#[test]
fn malformed_text_profiles_fail_product_admission_before_execution() {
    use crate::{TextClass, TextProfile};
    const INVALID: &[TextProfile] = &[
        TextProfile::RunContaining {
            class: TextClass::Numeric,
            required: TextClass::Union(&[]),
            minimum: 1,
            maximum: None,
        },
        TextProfile::RunContaining {
            class: TextClass::Numeric,
            required: TextClass::Numeric,
            minimum: 2,
            maximum: Some(1),
        },
        TextProfile::EndsIn {
            class: TextClass::Numeric,
            body: &TextProfile::Optional(&TextProfile::Literal("x")),
            positive: true,
        },
        TextProfile::Literal(""),
        TextProfile::Sequence(&[]),
        TextProfile::Run {
            class: TextClass::Numeric,
            minimum: 2,
            maximum: Some(1),
        },
        TextProfile::Run {
            class: TextClass::Characters(""),
            minimum: 1,
            maximum: None,
        },
        TextProfile::Run {
            class: TextClass::Union(&[]),
            minimum: 1,
            maximum: None,
        },
        TextProfile::Sequence(&[
            TextProfile::Run {
                class: TextClass::Numeric,
                minimum: u32::MAX as usize,
                maximum: None,
            },
            TextProfile::Literal("x"),
        ]),
        TextProfile::Optional(&TextProfile::Literal("x")),
        TextProfile::NotNext(TextClass::Numeric),
        TextProfile::IfNext {
            class: TextClass::Numeric,
            body: &TextProfile::Literal("x"),
            otherwise: None,
        },
    ];
    for profile in INVALID {
        let rules = Box::leak(Box::new([crate::LexicalRule {
            terminal: "string",
            expression: LexicalExpr::TextProfile(profile),
            precedence: 0,
            extra: false,
        }]));
        let language = Box::leak(Box::new(crate::LanguageSpec {
            lexical_rules: rules,
            ..quoted_profile_generated::LANGUAGE
        }));
        let error = crate::parse(language, "x").unwrap_err();
        assert_eq!(error.diagnostic.reason_kind, "invalid-aot-artifact");
    }
}

#[test]
fn text_profile_depth_rejects_before_aot_execution() {
    let mut profile: &'static crate::TextProfile =
        Box::leak(Box::new(crate::TextProfile::Literal("x")));
    for _ in 0..64 {
        profile = Box::leak(Box::new(crate::TextProfile::Optional(profile)));
    }
    let profile = Box::leak(Box::new(crate::TextProfile::Sequence(Box::leak(Box::new(
        [crate::TextProfile::Literal("x"), *profile],
    )))));
    let rules = Box::leak(Box::new([crate::LexicalRule {
        terminal: "string",
        expression: LexicalExpr::TextProfile(profile),
        precedence: 0,
        extra: false,
    }]));
    let language = Box::leak(Box::new(crate::LanguageSpec {
        lexical_rules: rules,
        ..quoted_profile_generated::LANGUAGE
    }));
    assert_eq!(
        crate::parse(language, "x")
            .unwrap_err()
            .diagnostic
            .reason_kind,
        "invalid-aot-artifact"
    );
}

#[test]
fn header_delimiter_rules_support_independent_prefixes_and_utf8_offsets() {
    let field = LexicalExpr::HeaderDelimiter {
        prefix: "MSH",
        count: 5,
        index: 0,
    };
    let data = LexicalExpr::HeaderData {
        prefix: "MSH",
        count: 5,
        stops: "\r\n",
    };
    assert_eq!(lexical_end(&field, "MSH|^~\\&|", 3), Some(4));
    assert_eq!(lexical_end(&data, "MSH|^~\\&α x|", 8), Some(12));
    assert_eq!(lexical_end(&data, "MSH|^~\\&\r", 8), None);
    for invalid in ["MSH|^~||", "MSH1^~\\&", "MSHα^~\\&", "MSH|^", "REC|^~\\&"] {
        assert_eq!(lexical_end(&field, invalid, 3), None, "{invalid}");
    }
    let separator = LexicalExpr::HeaderDelimiter {
        prefix: "記",
        count: 2,
        index: 1,
    };
    let record = LexicalExpr::HeaderData {
        prefix: "記",
        count: 2,
        stops: ";",
    };
    assert_eq!(lexical_end(&separator, "記|§α§", 4), Some(6));
    assert_eq!(lexical_end(&record, "記|§α§", 6), Some(8));
    assert_eq!(lexical_end(&record, "記|§α§", 5), None);
    assert_eq!(lexical_end(&record, "記|§α§", usize::MAX), None);
    let invalid = LexicalExpr::HeaderDelimiter {
        prefix: "記",
        count: 2,
        index: 2,
    };
    assert_eq!(lexical_end(&invalid, "記|§", 3), None);
}

#[path = "../fixtures/generated/structured_lexical.rs"]
mod structured_lexical_generated;

#[test]
fn scheme_generated_structured_profiles_preserve_unicode_spans() {
    for &(terminal, source, expected) in structured_lexical_generated::PROFILE_CASES {
        let rule = structured_lexical_generated::LANGUAGE
            .lexical_rules
            .iter()
            .find(|rule| rule.terminal == terminal)
            .unwrap();
        let byte_end = expected.map(|end| source.chars().take(end).map(char::len_utf8).sum());
        assert_eq!(
            lexical_end(&rule.expression, source, 0),
            byte_end,
            "{terminal}: {source:?}"
        );
        let prefixed = format!("字{source}");
        assert_eq!(
            lexical_end(&rule.expression, &prefixed, 3),
            byte_end.map(|end| end + 3)
        );
    }
}

#[test]
fn structured_profiles_execute_in_a_standalone_lossless_parser() {
    for source in ["α٣_字", "<1>1.", "<١>字", "<+>*."] {
        let parsed = crate::parse(&structured_lexical_generated::LANGUAGE, source).unwrap();
        assert_eq!(parsed.syntax().text().to_string(), source);
    }
    for source in ["123", "<+1>x.", "<>."] {
        assert!(crate::parse(&structured_lexical_generated::LANGUAGE, source).is_err());
    }
}
