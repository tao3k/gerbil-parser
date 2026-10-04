// @generated from Scheme contextual Scanner IR; do not edit.
#[allow(unused_imports)]
use gerbil_parser_rowan::scanner::{
    BalancedPair, MarkerPolicy, ScannerAction, ScannerCell, ScannerMatcher, ScannerRule,
    ScannerSpec,
};
pub static SCANNER: ScannerSpec = ScannerSpec {
    opcode_contract: "gerbil-parser.contextual-scanner-opcodes.v1",
    digest: "sha256:e7610dab1af5a0d24694bc88f33641e35a13a00d634e5ce064b561a65f434e81",
    base_grammar_digest: None,
    initial_mode: "command",
    modes: &["command", "body"],
    positions: &["command", "argument"],
    rules: &[
        ScannerRule {
            name: "strip",
            mode: "command",
            form: "open",
            matcher: ScannerMatcher::Literal("<<-"),
            rank: 30,
            action: ScannerAction::ExpectMarker(true),
        },
        ScannerRule {
            name: "plain",
            mode: "command",
            form: "open",
            matcher: ScannerMatcher::Literal("<<"),
            rank: 30,
            action: ScannerAction::ExpectMarker(false),
        },
        ScannerRule {
            name: "word",
            mode: "command",
            form: "word",
            matcher: ScannerMatcher::BalancedWord {
                stops: &["<<", "<<-"],
                quotes: &["'", "\""],
                pairs: &[
                    BalancedPair {
                        prefix: "${",
                        opening: '\u{7b}',
                        closing: '\u{7d}',
                    },
                    BalancedPair {
                        prefix: "$(",
                        opening: '\u{28}',
                        closing: '\u{29}',
                    },
                ],
            },
            rank: 0,
            action: ScannerAction::EnqueueIfExpecting(MarkerPolicy::ShellQuoteRemoval),
        },
        ScannerRule {
            name: "space",
            mode: "command",
            form: "space",
            matcher: ScannerMatcher::HorizontalWhitespace,
            rank: 0,
            action: ScannerAction::Keep,
        },
        ScannerRule {
            name: "newline",
            mode: "command",
            form: "newline",
            matcher: ScannerMatcher::NewlineOne,
            rank: 0,
            action: ScannerAction::ActivateNext("body"),
        },
        ScannerRule {
            name: "marker",
            mode: "body",
            form: "end",
            matcher: ScannerMatcher::MarkerLine,
            rank: 10,
            action: ScannerAction::FinishMarker {
                base: "command",
                body: "body",
            },
        },
        ScannerRule {
            name: "body",
            mode: "body",
            form: "body",
            matcher: ScannerMatcher::BodyLine,
            rank: 0,
            action: ScannerAction::Keep,
        },
    ],
    cells: &[
        ScannerCell {
            mode: "command",
            position: "command",
            form: "open",
            terminal: "here-open",
        },
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
        ScannerCell {
            mode: "command",
            position: "command",
            form: "newline",
            terminal: "newline",
        },
        ScannerCell {
            mode: "command",
            position: "command",
            form: "body",
            terminal: "body",
        },
        ScannerCell {
            mode: "command",
            position: "command",
            form: "end",
            terminal: "end",
        },
        ScannerCell {
            mode: "command",
            position: "argument",
            form: "open",
            terminal: "here-open",
        },
        ScannerCell {
            mode: "command",
            position: "argument",
            form: "word",
            terminal: "word",
        },
        ScannerCell {
            mode: "command",
            position: "argument",
            form: "space",
            terminal: "space",
        },
        ScannerCell {
            mode: "command",
            position: "argument",
            form: "newline",
            terminal: "newline",
        },
        ScannerCell {
            mode: "command",
            position: "argument",
            form: "body",
            terminal: "body",
        },
        ScannerCell {
            mode: "command",
            position: "argument",
            form: "end",
            terminal: "end",
        },
        ScannerCell {
            mode: "body",
            position: "command",
            form: "open",
            terminal: "here-open",
        },
        ScannerCell {
            mode: "body",
            position: "command",
            form: "word",
            terminal: "word",
        },
        ScannerCell {
            mode: "body",
            position: "command",
            form: "space",
            terminal: "space",
        },
        ScannerCell {
            mode: "body",
            position: "command",
            form: "newline",
            terminal: "newline",
        },
        ScannerCell {
            mode: "body",
            position: "command",
            form: "body",
            terminal: "body",
        },
        ScannerCell {
            mode: "body",
            position: "command",
            form: "end",
            terminal: "end",
        },
        ScannerCell {
            mode: "body",
            position: "argument",
            form: "open",
            terminal: "here-open",
        },
        ScannerCell {
            mode: "body",
            position: "argument",
            form: "word",
            terminal: "word",
        },
        ScannerCell {
            mode: "body",
            position: "argument",
            form: "space",
            terminal: "space",
        },
        ScannerCell {
            mode: "body",
            position: "argument",
            form: "newline",
            terminal: "newline",
        },
        ScannerCell {
            mode: "body",
            position: "argument",
            form: "body",
            terminal: "body",
        },
        ScannerCell {
            mode: "body",
            position: "argument",
            form: "end",
            terminal: "end",
        },
    ],
};
pub static TRACES: &[(&str, &[gerbil_parser_rowan::ScannedToken])] = &[
    (
        "cat <<'A' <<-B\r\n$x α\r\nA\r\n\tβ $x\n\tB\n",
        &[
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "space",
                start: 3,
                end: 4,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "here-open",
                start: 4,
                end: 6,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 6,
                end: 9,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "space",
                start: 9,
                end: 10,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "here-open",
                start: 10,
                end: 13,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 13,
                end: 14,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "newline",
                start: 14,
                end: 16,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "body",
                start: 16,
                end: 23,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "end",
                start: 23,
                end: 26,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "body",
                start: 26,
                end: 33,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "end",
                start: 33,
                end: 36,
            },
        ],
    ),
    (
        "cat <<\\\\EOF\ntext\n\\EOF\n",
        &[
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "space",
                start: 3,
                end: 4,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "here-open",
                start: 4,
                end: 6,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 6,
                end: 11,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "newline",
                start: 11,
                end: 12,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "body",
                start: 12,
                end: 17,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "end",
                start: 17,
                end: 22,
            },
        ],
    ),
    (
        "cat <<\"a\\qb\"\na\\qb\n",
        &[
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "space",
                start: 3,
                end: 4,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "here-open",
                start: 4,
                end: 6,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 6,
                end: 12,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "newline",
                start: 12,
                end: 13,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "end",
                start: 13,
                end: 18,
            },
        ],
    ),
    (
        "cat <<''\n\n",
        &[
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "space",
                start: 3,
                end: 4,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "here-open",
                start: 4,
                end: 6,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 6,
                end: 8,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "newline",
                start: 8,
                end: 9,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "end",
                start: 9,
                end: 10,
            },
        ],
    ),
    (
        "printf %s \"${x:-$(printf '%s' '}')}\"\n",
        &[
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 0,
                end: 6,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "space",
                start: 6,
                end: 7,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 7,
                end: 9,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "space",
                start: 9,
                end: 10,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 10,
                end: 36,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "newline",
                start: 36,
                end: 37,
            },
        ],
    ),
    (
        "echo α\n\n",
        &[
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 0,
                end: 4,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "space",
                start: 4,
                end: 5,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "word",
                start: 5,
                end: 7,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "newline",
                start: 7,
                end: 8,
            },
            gerbil_parser_rowan::ScannedToken {
                terminal: "newline",
                start: 8,
                end: 9,
            },
        ],
    ),
];
