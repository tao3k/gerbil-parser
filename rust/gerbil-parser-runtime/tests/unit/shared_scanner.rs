//! Scheme-generated scanner IR executes identical UTF-8 token traces in Rust.
#[path = "../fixtures/shared_scanner_generated.rs"]
mod generated;
use gerbil_parser_runtime::scanner::ContextualScanner;
#[test]
fn scheme_traces_execute_without_language_callbacks() {
    for (source, expected) in generated::TRACES {
        let scanner = ContextualScanner::new(&generated::SCANNER, source).expect("compiled spec");
        assert_eq!(scanner.scan("command").expect("complete scan"), *expected);
    }
}
#[test]
fn checkpoints_are_immutable_source_and_scanner_bound() {
    let scanner = ContextualScanner::new(&generated::SCANNER, "cat <<A\nα\nA\n").expect("spec");
    let initial = scanner.initial_state();
    let (first, state) = scanner.step(&initial, "command").expect("step");
    assert_eq!(first.expect("token").end, 3);
    assert_eq!(initial.byte_offset(), 0);
    assert_eq!(scanner.step(&initial, "command").expect("repeat").0, first);
    let other = ContextualScanner::new(&generated::SCANNER, "cat <<A\nα\nA\n").expect("spec");
    assert!(other.step(&state, "command").is_err());
    assert!(scanner.step(&state, "unknown").is_err());
}
#[test]
fn unfinished_input_fails_closed() {
    for source in ["cat <<A\nα\n", "cat <<\n", "echo \"${x}", "cat <<A"] {
        assert!(
            ContextualScanner::new(&generated::SCANNER, source)
                .expect("spec")
                .scan("command")
                .is_err()
        );
    }
}

#[test]
fn opcode_admission_and_equal_rank_ambiguity_fail_closed() {
    use gerbil_parser_runtime::scanner::{
        ScannerAction, ScannerCell, ScannerMatcher, ScannerRule, ScannerSpec,
    };
    static EMPTY: ScannerSpec = ScannerSpec {
        rules: &[ScannerRule {
            name: "bad",
            mode: "command",
            form: "word",
            matcher: ScannerMatcher::Literal(""),
            rank: 0,
            action: ScannerAction::Keep,
        }],
        ..generated::SCANNER
    };
    static TIE: ScannerSpec = ScannerSpec {
        modes: &["command"],
        positions: &["command"],
        cells: &[
            ScannerCell {
                mode: "command",
                position: "command",
                form: "word",
                terminal: "word",
            },
            ScannerCell {
                mode: "command",
                position: "command",
                form: "space",
                terminal: "space",
            },
        ],
        rules: &[
            ScannerRule {
                name: "word",
                mode: "command",
                form: "word",
                matcher: ScannerMatcher::Literal("x"),
                rank: 0,
                action: ScannerAction::Keep,
            },
            ScannerRule {
                name: "space",
                mode: "command",
                form: "space",
                matcher: ScannerMatcher::Literal("x"),
                rank: 0,
                action: ScannerAction::Keep,
            },
        ],
        ..generated::SCANNER
    };
    assert!(ContextualScanner::new(&EMPTY, "x").is_err());
    assert!(
        ContextualScanner::new(&TIE, "x")
            .expect("admitted rules")
            .scan("command")
            .is_err()
    );
}
#[test]
fn large_fifo_preserves_old_checkpoints_and_drains_in_order() {
    use std::fmt::Write;
    let mut source = String::from("cat");
    for i in 0..2000 {
        write!(source, " <<M{i}").expect("string");
    }
    source.push('\n');
    for i in 0..2000 {
        writeln!(source, "M{i}").expect("string");
    }
    let scanner = ContextualScanner::new(&generated::SCANNER, &source).expect("spec");
    let initial = scanner.initial_state();
    assert!(scanner.scan("command").is_ok());
    assert_eq!(initial.pending_markers(), 0);
    assert_eq!(initial.byte_offset(), 0);
}

#[test]
fn shared_plan_keeps_parallel_inputs_and_obligations_independent() {
    std::thread::scope(|scope| {
        let workers: Vec<_> = (0..8)
            .map(|_| {
                scope.spawn(|| {
                    for _ in 0..8 {
                        for (source, expected) in generated::TRACES {
                            let scanner =
                                ContextualScanner::new(&generated::SCANNER, source).expect("spec");
                            assert_eq!(scanner.scan("command").expect("complete scan"), *expected);
                        }
                    }
                })
            })
            .collect();
        for worker in workers {
            worker.join().expect("independent parser thread");
        }
    });
}

#[test]
fn declared_regions_handle_deep_nesting_and_reject_partial_suffixes() {
    let source = format!("echo {}α{}\n", "$(".repeat(5000), ")".repeat(5000));
    let tokens = ContextualScanner::new(&generated::SCANNER, &source)
        .expect("region spec")
        .scan("command")
        .expect("complete nested word");
    assert_eq!(tokens[2].start, 5);
    assert_eq!(tokens[2].end, source.len() - 1);
    for source in ["echo $((1)", "echo <(cat", "echo `opaque", "echo \"${x}"] {
        assert!(
            ContextualScanner::new(&generated::SCANNER, source)
                .expect("region spec")
                .scan("command")
                .is_err()
        );
    }
}

#[test]
fn region_declarations_control_initial_stops_and_scalar_depth_admission() {
    use gerbil_parser_runtime::scanner::{
        RegionPair, ScannerAction, ScannerMatcher, ScannerRule, ScannerSpec,
    };
    static INITIAL: ScannerSpec = ScannerSpec {
        rules: &[ScannerRule {
            name: "initial",
            mode: "command",
            form: "word",
            rank: 0,
            matcher: ScannerMatcher::RegionWord {
                stops: &[";"],
                quotes: &[],
                pairs: &[],
                consume_initial_stop: true,
            },
            action: ScannerAction::Keep,
        }],
        ..generated::SCANNER
    };
    static BAD_DEPTH: ScannerSpec = ScannerSpec {
        rules: &[ScannerRule {
            name: "invalid-depth",
            mode: "command",
            form: "word",
            rank: 0,
            matcher: ScannerMatcher::RegionWord {
                stops: &[";"],
                quotes: &[],
                pairs: &[RegionPair {
                    prefix: "α(",
                    opening: '(',
                    closing: ')',
                    depth: 3,
                }],
                consume_initial_stop: false,
            },
            action: ScannerAction::Keep,
        }],
        ..generated::SCANNER
    };
    let tokens = ContextualScanner::new(&INITIAL, ";α")
        .expect("consume initial stop")
        .scan("command")
        .expect("one word");
    assert_eq!(tokens.len(), 1);
    assert_eq!((tokens[0].start, tokens[0].end), (0, 3));
    assert!(ContextualScanner::new(&BAD_DEPTH, "α(x)))").is_err());
}
