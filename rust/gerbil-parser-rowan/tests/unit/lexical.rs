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
    assert_eq!(lexical_end(&atom, ")", 0), None);
}
