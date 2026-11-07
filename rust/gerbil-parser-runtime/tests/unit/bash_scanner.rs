//! Production Bash profile executes Scheme token/span and rejection controls.
#[path = "../fixtures/generated/bash_scanner.rs"]
mod generated;
use gerbil_parser_runtime::scanner::ContextualScanner;
#[test]
fn production_profile_matches_scheme_control_traces() {
    assert!(generated::TRACES.len() >= 40);
    assert!(generated::REJECTED.len() >= 4);
    for (source, expected) in generated::TRACES {
        let scanner =
            ContextualScanner::new(&generated::SCANNER, source).expect("admitted profile");
        assert_eq!(
            scanner.scan("source").expect("complete scan"),
            *expected,
            "{source:?}"
        );
    }
    for source in generated::REJECTED {
        assert!(
            ContextualScanner::new(&generated::SCANNER, source)
                .expect("profile")
                .scan("source")
                .is_err(),
            "{source:?}"
        );
    }
}
#[test]
fn production_profile_checkpoints_replay_utf8_and_reject_foreign_owners() {
    let source = "cat <<A <<B\nα\nA\nβ\nB\n";
    let scanner = ContextualScanner::new(&generated::SCANNER, source).expect("profile");
    let other = ContextualScanner::new(&generated::SCANNER, source).expect("profile");
    let mut state = scanner.initial_state();
    loop {
        let (token, next) = scanner.step(&state, "source").expect("step");
        assert_eq!(scanner.step(&state, "source").expect("replay").0, token);
        assert!(other.step(&state, "source").is_err());
        state = next;
        if token.is_none() {
            break;
        }
    }
    assert_eq!(state.byte_offset(), source.len());
}

#[test]
fn profiles_reject_recursive_guards_nonprogressing_prefixes_and_unknown_modes() {
    use gerbil_parser_runtime::scanner::{ScannerAction, ScannerMatcher, ScannerRule, ScannerSpec};
    static NESTED: ScannerSpec = ScannerSpec {
        rules: &[ScannerRule {
            name: "nested",
            mode: "command",
            form: "word",
            rank: 0,
            matcher: ScannerMatcher::UnlessPrefix {
                prefixes: &["#"],
                exceptions: &[],
                child: &ScannerMatcher::UnlessPrefix {
                    prefixes: &["#"],
                    exceptions: &[],
                    child: &ScannerMatcher::Literal("α"),
                },
            },
            action: ScannerAction::Keep,
        }],
        ..generated::SCANNER
    };
    static LINE: ScannerSpec = ScannerSpec {
        rules: &[ScannerRule {
            name: "line",
            mode: "command",
            form: "word",
            rank: 0,
            matcher: ScannerMatcher::LinePrefix {
                prefix: "#\n",
                separator: '\n',
            },
            action: ScannerAction::Keep,
        }],
        ..generated::SCANNER
    };
    static MODE: ScannerSpec = ScannerSpec {
        rules: &[ScannerRule {
            name: "mode",
            mode: "command",
            form: "word",
            rank: 0,
            matcher: ScannerMatcher::Literal("α"),
            action: ScannerAction::ExpectMarkerIn {
                strip_tabs: false,
                mode: "missing",
            },
        }],
        ..generated::SCANNER
    };
    for spec in [&NESTED, &LINE, &MODE] {
        assert!(ContextualScanner::new(spec, "α").is_err());
    }
}
