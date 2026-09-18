use gerbil_parser_rowan::generated::arithmetic_v1::LANGUAGE;

#[test]
fn generated_arithmetic_parser_builds_a_lossless_rowan_tree() {
    let source = "  alpha + 2 * (beta - -3)  \n";
    let parsed = gerbil_parser_rowan::parse(&LANGUAGE, source).expect("accepted arithmetic");
    assert_eq!(parsed.syntax().to_string(), source);
    assert_eq!(parsed.receipt().language, "arithmetic");
    assert_eq!(parsed.receipt().version, "v1");
    assert_eq!(parsed.receipt().contract, "arithmetic-expression.v1");
    assert!(parsed.receipt().grammar_digest.starts_with("sha256:"));
    assert!(parsed.receipt().source_digest.starts_with("sha256:"));
}

#[test]
fn generated_arithmetic_parser_preserves_precedence_shape() {
    let parsed = gerbil_parser_rowan::parse(&LANGUAGE, "1 + 2 * 3").expect("accepted arithmetic");
    let root = parsed.syntax();
    let names = root
        .descendants()
        .map(|node| parsed.kind_name(node.kind()).expect("generated kind"))
        .collect::<Vec<_>>();
    assert_eq!(
        names,
        [
            "SourceFile",
            "Expression",
            "NumberExpression",
            "Expression",
            "NumberExpression",
            "NumberExpression",
        ]
    );
}

#[test]
fn generated_arithmetic_parser_fails_closed() {
    let error = gerbil_parser_rowan::parse(&LANGUAGE, "1 + * 2").expect_err("rejected arithmetic");
    assert_eq!(error.diagnostic.reason_kind, "parse-rejected");
    assert_eq!(error.receipt.grammar_digest, LANGUAGE.grammar_digest);

    let joined = gerbil_parser_rowan::parse(&LANGUAGE, "123abc")
        .expect_err("number and identifier cannot be fused by the Rust lexer");
    assert_eq!(joined.diagnostic.reason_kind, "parse-rejected");
}

#[test]
fn generated_arithmetic_identifier_matches_the_declared_scanner() {
    let source = "λ-value_2 + ٣";
    let parsed = gerbil_parser_rowan::parse(&LANGUAGE, source).expect("Unicode arithmetic");
    assert_eq!(parsed.syntax().to_string(), source);
}
