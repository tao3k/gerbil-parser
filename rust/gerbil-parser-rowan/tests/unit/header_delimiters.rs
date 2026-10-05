//! Execute two Scheme-generated grammars through the shared Rust scanner rules.
#[path = "../fixtures/generated/hl7_header.rs"]
mod hl7;
#[path = "../fixtures/generated/header_record.rs"]
mod record;
use gerbil_parser_rowan::{LanguageSpec, LexicalExpr, LexicalRule, parse};

static INVALID_DATA_RULES: [LexicalRule; 3] = [
    record::LANGUAGE.lexical_rules[0],
    record::LANGUAGE.lexical_rules[1],
    LexicalRule {
        expression: LexicalExpr::HeaderData {
            prefix: "記",
            count: 0,
            stops: ";",
        },
        ..record::LANGUAGE.lexical_rules[2]
    },
];
static INVALID_DATA: LanguageSpec = LanguageSpec {
    lexical_rules: &INVALID_DATA_RULES,
    ..record::LANGUAGE
};

#[test]
fn header_aot_two_scheme_languages_parse_and_reject() {
    for source in ["記|§α|", "記*$α*", "記|§a b|"] {
        let parsed = parse(&record::LANGUAGE, source).expect("independent header record");
        assert_eq!(parsed.syntax().to_string(), source);
        assert_eq!(
            parsed.receipt().grammar_digest,
            record::LANGUAGE.grammar_digest
        );
    }
    for source in ["記||α|", "記a§αa", "記|§|", "記|§α;|"] {
        assert!(parse(&record::LANGUAGE, source).is_err(), "{source}");
    }
    for source in [
        include_str!("../../../../languages/hl7/corpus/adt-a08-patient.hl7"),
        "MSH*$%!?*LEGACY*AU*FHIR*AU*202609170900**ADT$A08*1*P*2.5.1\r",
    ] {
        let parsed = parse(&hl7::LANGUAGE, source).expect("HL7 shared header rules");
        assert_eq!(parsed.syntax().to_string(), source);
        assert_eq!(
            parsed.receipt().grammar_digest,
            hl7::LANGUAGE.grammar_digest
        );
    }
    for source in ["PID|1\r", "MSH|^^\\&|\r", "MSH1^~\\&|\r"] {
        assert!(parse(&hl7::LANGUAGE, source).is_err(), "{source}");
    }
}

#[test]
fn header_aot_admission_rejects_invalid_closed_recipes() {
    static INVALID_RULES: [LexicalRule; 3] = [
        LexicalRule {
            expression: LexicalExpr::HeaderDelimiter {
                prefix: "記",
                count: 2,
                index: 2,
            },
            ..record::LANGUAGE.lexical_rules[0]
        },
        record::LANGUAGE.lexical_rules[1],
        record::LANGUAGE.lexical_rules[2],
    ];
    static INVALID: LanguageSpec = LanguageSpec {
        lexical_rules: &INVALID_RULES,
        ..record::LANGUAGE
    };
    let failure = parse(&INVALID, "記|§α|").expect_err("invalid recipe must reject at admission");
    assert!(failure.diagnostic.message.contains("header delimiter"));
    let failure = parse(&INVALID_DATA, "記|§α|").expect_err("invalid header data must reject");
    assert!(failure.diagnostic.message.contains("header data"));
}
