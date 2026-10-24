// @generated from Scheme contextual Scanner IR; do not edit.
#[allow(unused_imports)]
use gerbil_parser_runtime::scanner::{
    BalancedPair, MarkerPolicy, ScannerAction, ScannerCell, ScannerMatcher, ScannerRule,
    ScannerSpec,
};
pub static SCANNER: ScannerSpec = ScannerSpec {
    opcode_contract: "gerbil-parser.contextual-scanner-opcodes.v1",
    digest: "sha256:350c02574541fb278cf653584eb79c1eb2b5f326ad06dc92a43f8daffbb099a3",
    base_grammar_digest: None,
    initial_mode: "command",
    modes: &["command", "marker", "body"],
    positions: &["source"],
    rules: &[
        ScannerRule {
            name: "space",
            mode: "command",
            form: "horizontal-whitespace",
            matcher: ScannerMatcher::HorizontalWhitespace,
            rank: 0,
            action: ScannerAction::Keep,
        },
        ScannerRule {
            name: "newline",
            mode: "command",
            form: "newline",
            matcher: ScannerMatcher::Literal("\n"),
            rank: 0,
            action: ScannerAction::ActivateNext("body"),
        },
        ScannerRule {
            name: "continuation",
            mode: "command",
            form: "line-continuation",
            matcher: ScannerMatcher::Literal("\\\n"),
            rank: 10,
            action: ScannerAction::Keep,
        },
        ScannerRule {
            name: "comment",
            mode: "command",
            form: "comment",
            matcher: ScannerMatcher::LinePrefix {
                prefix: "#",
                separator: '\u{a}',
            },
            rank: 0,
            action: ScannerAction::Keep,
        },
        ScannerRule {
            name: "strip",
            mode: "command",
            form: "operator",
            matcher: ScannerMatcher::Literal("<<-"),
            rank: 30,
            action: ScannerAction::ExpectMarkerIn {
                strip_tabs: true,
                mode: "marker",
            },
        },
        ScannerRule {
            name: "plain",
            mode: "command",
            form: "operator",
            matcher: ScannerMatcher::Literal("<<"),
            rank: 30,
            action: ScannerAction::ExpectMarkerIn {
                strip_tabs: false,
                mode: "marker",
            },
        },
        ScannerRule {
            name: "operator",
            mode: "command",
            form: "operator",
            matcher: ScannerMatcher::Literals(&[
                ";;&", "&>>", "<<<", "&&", "||", "|&", ";;", ";&", ">>", "<>", "<&", ">&", ">|",
                "&>", ";", "&", "|", "(", ")", "<", ">",
            ]),
            rank: 0,
            action: ScannerAction::Keep,
        },
        ScannerRule {
            name: "word",
            mode: "command",
            form: "word",
            matcher: ScannerMatcher::UnlessPrefix {
                prefixes: &[
                    " ", "\t", "\n", "#", "\\\n", ";", "&", "|", "(", ")", "<", ">",
                ],
                exceptions: &["<(", ">("],
                child: &ScannerMatcher::RegionWord {
                    stops: &[
                        ";;&", "&>>", "<<-", "<<<", "&&", "||", "|&", ";;", ";&", "<<", ">>", "<>",
                        "<&", ">&", ">|", "&>", ";", "&", "|", "(", ")", "<", ">",
                    ],
                    quotes: &[
                        gerbil_parser_runtime::scanner::RegionQuote {
                            delimiter: '\u{27}',
                            escaped: false,
                            pairs: &[],
                        },
                        gerbil_parser_runtime::scanner::RegionQuote {
                            delimiter: '\u{22}',
                            escaped: true,
                            pairs: &["${", "$(", "$(("],
                        },
                        gerbil_parser_runtime::scanner::RegionQuote {
                            delimiter: '\u{60}',
                            escaped: true,
                            pairs: &[],
                        },
                    ],
                    pairs: &[
                        gerbil_parser_runtime::scanner::RegionPair {
                            prefix: "${",
                            opening: '\u{7b}',
                            closing: '\u{7d}',
                            depth: 1,
                        },
                        gerbil_parser_runtime::scanner::RegionPair {
                            prefix: "$(",
                            opening: '\u{28}',
                            closing: '\u{29}',
                            depth: 1,
                        },
                        gerbil_parser_runtime::scanner::RegionPair {
                            prefix: "$((",
                            opening: '\u{28}',
                            closing: '\u{29}',
                            depth: 2,
                        },
                        gerbil_parser_runtime::scanner::RegionPair {
                            prefix: "<(",
                            opening: '\u{28}',
                            closing: '\u{29}',
                            depth: 1,
                        },
                        gerbil_parser_runtime::scanner::RegionPair {
                            prefix: ">(",
                            opening: '\u{28}',
                            closing: '\u{29}',
                            depth: 1,
                        },
                    ],
                    consume_initial_stop: true,
                },
            },
            rank: 0,
            action: ScannerAction::Keep,
        },
        ScannerRule {
            name: "marker-space",
            mode: "marker",
            form: "horizontal-whitespace",
            matcher: ScannerMatcher::HorizontalWhitespace,
            rank: 0,
            action: ScannerAction::Keep,
        },
        ScannerRule {
            name: "marker-continuation",
            mode: "marker",
            form: "line-continuation",
            matcher: ScannerMatcher::Literal("\\\n"),
            rank: 10,
            action: ScannerAction::Keep,
        },
        ScannerRule {
            name: "marker-newline",
            mode: "marker",
            form: "newline",
            matcher: ScannerMatcher::Literal("\n"),
            rank: 10,
            action: ScannerAction::ActivateNext("body"),
        },
        ScannerRule {
            name: "marker-word",
            mode: "marker",
            form: "heredoc-marker",
            matcher: ScannerMatcher::UnlessPrefix {
                prefixes: &[" ", "\t", "\n", "\\\n"],
                exceptions: &[],
                child: &ScannerMatcher::RegionWord {
                    stops: &[
                        ";;&", "&>>", "<<-", "<<<", "&&", "||", "|&", ";;", ";&", "<<", ">>", "<>",
                        "<&", ">&", ">|", "&>", ";", "&", "|", "(", ")", "<", ">",
                    ],
                    quotes: &[
                        gerbil_parser_runtime::scanner::RegionQuote {
                            delimiter: '\u{27}',
                            escaped: false,
                            pairs: &[],
                        },
                        gerbil_parser_runtime::scanner::RegionQuote {
                            delimiter: '\u{22}',
                            escaped: true,
                            pairs: &["${", "$(", "$(("],
                        },
                        gerbil_parser_runtime::scanner::RegionQuote {
                            delimiter: '\u{60}',
                            escaped: true,
                            pairs: &[],
                        },
                    ],
                    pairs: &[
                        gerbil_parser_runtime::scanner::RegionPair {
                            prefix: "${",
                            opening: '\u{7b}',
                            closing: '\u{7d}',
                            depth: 1,
                        },
                        gerbil_parser_runtime::scanner::RegionPair {
                            prefix: "$(",
                            opening: '\u{28}',
                            closing: '\u{29}',
                            depth: 1,
                        },
                        gerbil_parser_runtime::scanner::RegionPair {
                            prefix: "$((",
                            opening: '\u{28}',
                            closing: '\u{29}',
                            depth: 2,
                        },
                        gerbil_parser_runtime::scanner::RegionPair {
                            prefix: "<(",
                            opening: '\u{28}',
                            closing: '\u{29}',
                            depth: 1,
                        },
                        gerbil_parser_runtime::scanner::RegionPair {
                            prefix: ">(",
                            opening: '\u{28}',
                            closing: '\u{29}',
                            depth: 1,
                        },
                    ],
                    consume_initial_stop: true,
                },
            },
            rank: 0,
            action: ScannerAction::EnqueueMarkerIn {
                policy: MarkerPolicy::ShellQuoteRemoval,
                mode: "command",
            },
        },
        ScannerRule {
            name: "end",
            mode: "body",
            form: "heredoc-end",
            matcher: ScannerMatcher::MarkerLineAt('\u{a}'),
            rank: 10,
            action: ScannerAction::FinishMarker {
                base: "command",
                body: "body",
            },
        },
        ScannerRule {
            name: "body",
            mode: "body",
            form: "heredoc-content",
            matcher: ScannerMatcher::BodyLineAt('\u{a}'),
            rank: 0,
            action: ScannerAction::Keep,
        },
    ],
    cells: &[
        ScannerCell {
            mode: "command",
            position: "source",
            form: "horizontal-whitespace",
            terminal: "horizontal-whitespace",
        },
        ScannerCell {
            mode: "command",
            position: "source",
            form: "newline",
            terminal: "newline",
        },
        ScannerCell {
            mode: "command",
            position: "source",
            form: "line-continuation",
            terminal: "line-continuation",
        },
        ScannerCell {
            mode: "command",
            position: "source",
            form: "comment",
            terminal: "comment",
        },
        ScannerCell {
            mode: "command",
            position: "source",
            form: "word",
            terminal: "word",
        },
        ScannerCell {
            mode: "command",
            position: "source",
            form: "operator",
            terminal: "operator",
        },
        ScannerCell {
            mode: "command",
            position: "source",
            form: "heredoc-marker",
            terminal: "heredoc-marker",
        },
        ScannerCell {
            mode: "command",
            position: "source",
            form: "heredoc-content",
            terminal: "heredoc-content",
        },
        ScannerCell {
            mode: "command",
            position: "source",
            form: "heredoc-end",
            terminal: "heredoc-end",
        },
        ScannerCell {
            mode: "marker",
            position: "source",
            form: "horizontal-whitespace",
            terminal: "horizontal-whitespace",
        },
        ScannerCell {
            mode: "marker",
            position: "source",
            form: "newline",
            terminal: "newline",
        },
        ScannerCell {
            mode: "marker",
            position: "source",
            form: "line-continuation",
            terminal: "line-continuation",
        },
        ScannerCell {
            mode: "marker",
            position: "source",
            form: "comment",
            terminal: "comment",
        },
        ScannerCell {
            mode: "marker",
            position: "source",
            form: "word",
            terminal: "word",
        },
        ScannerCell {
            mode: "marker",
            position: "source",
            form: "operator",
            terminal: "operator",
        },
        ScannerCell {
            mode: "marker",
            position: "source",
            form: "heredoc-marker",
            terminal: "heredoc-marker",
        },
        ScannerCell {
            mode: "marker",
            position: "source",
            form: "heredoc-content",
            terminal: "heredoc-content",
        },
        ScannerCell {
            mode: "marker",
            position: "source",
            form: "heredoc-end",
            terminal: "heredoc-end",
        },
        ScannerCell {
            mode: "body",
            position: "source",
            form: "horizontal-whitespace",
            terminal: "horizontal-whitespace",
        },
        ScannerCell {
            mode: "body",
            position: "source",
            form: "newline",
            terminal: "newline",
        },
        ScannerCell {
            mode: "body",
            position: "source",
            form: "line-continuation",
            terminal: "line-continuation",
        },
        ScannerCell {
            mode: "body",
            position: "source",
            form: "comment",
            terminal: "comment",
        },
        ScannerCell {
            mode: "body",
            position: "source",
            form: "word",
            terminal: "word",
        },
        ScannerCell {
            mode: "body",
            position: "source",
            form: "operator",
            terminal: "operator",
        },
        ScannerCell {
            mode: "body",
            position: "source",
            form: "heredoc-marker",
            terminal: "heredoc-marker",
        },
        ScannerCell {
            mode: "body",
            position: "source",
            form: "heredoc-content",
            terminal: "heredoc-content",
        },
        ScannerCell {
            mode: "body",
            position: "source",
            form: "heredoc-end",
            terminal: "heredoc-end",
        },
    ],
};
pub static TRACES: &[(&str, &[gerbil_parser_runtime::ScannedToken])] = &[
    ("", &[]),
    (
        "echo α",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 4,
                end: 5,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 5,
                end: 7,
            },
        ],
    ),
    (
        "# unterminated '\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "comment",
                start: 0,
                end: 16,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 16,
                end: 17,
            },
        ],
    ),
    (
        "# $(unterminated\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "comment",
                start: 0,
                end: 16,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 16,
                end: 17,
            },
        ],
    ),
    (
        "echo \\\nα",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 4,
                end: 5,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "line-continuation",
                start: 5,
                end: 7,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 7,
                end: 9,
            },
        ],
    ),
    (
        "echo <(printf α) >(cat)",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 4,
                end: 5,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 5,
                end: 17,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 17,
                end: 18,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 18,
                end: 24,
            },
        ],
    ),
    (
        "echo \rα",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 4,
                end: 5,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 5,
                end: 8,
            },
        ],
    ),
    (
        "echo \r\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 4,
                end: 5,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 5,
                end: 6,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 6,
                end: 7,
            },
        ],
    ),
    (
        "\rfoo",
        &[gerbil_parser_runtime::ScannedToken {
            terminal: "word",
            start: 0,
            end: 4,
        }],
    ),
    (
        "α",
        &[gerbil_parser_runtime::ScannedToken {
            terminal: "word",
            start: 0,
            end: 4,
        }],
    ),
    (
        "cat <<A <<B\nα\nA\nβ\nB\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 3,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 4,
                end: 6,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 6,
                end: 7,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 7,
                end: 8,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 8,
                end: 10,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 10,
                end: 11,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 11,
                end: 12,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 12,
                end: 15,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 15,
                end: 17,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 17,
                end: 20,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 20,
                end: 22,
            },
        ],
    ),
    (
        "cat <<-A\n\tα\n\tA\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 3,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 4,
                end: 7,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 7,
                end: 8,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 8,
                end: 9,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 9,
                end: 13,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 13,
                end: 16,
            },
        ],
    ),
    (
        "cat <<'A'\n$α\nA\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 3,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 4,
                end: 6,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 6,
                end: 9,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 9,
                end: 10,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 10,
                end: 14,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 14,
                end: 16,
            },
        ],
    ),
    (
        "cat <<\"a\\q\"\nα\na\\q\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 3,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 4,
                end: 6,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 6,
                end: 11,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 11,
                end: 12,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 12,
                end: 15,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 15,
                end: 19,
            },
        ],
    ),
    (
        "cat <<''\nα\n\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 3,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 4,
                end: 6,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 6,
                end: 8,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 8,
                end: 9,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 9,
                end: 12,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 12,
                end: 13,
            },
        ],
    ),
    (
        "cat <<A\\\nB\nα\nAB\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 3,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 4,
                end: 6,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 6,
                end: 10,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 10,
                end: 11,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 11,
                end: 14,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 14,
                end: 17,
            },
        ],
    ),
    (
        "cat <<A \\\n <<B\nα\nA\nβ\nB\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 3,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 4,
                end: 6,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 6,
                end: 7,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 7,
                end: 8,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "line-continuation",
                start: 8,
                end: 10,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 10,
                end: 11,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 11,
                end: 13,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 13,
                end: 14,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 14,
                end: 15,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 15,
                end: 18,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 18,
                end: 20,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 20,
                end: 23,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 23,
                end: 25,
            },
        ],
    ),
    (
        "cat <<A\nα\nA",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 3,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 4,
                end: 6,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 6,
                end: 7,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 7,
                end: 8,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 8,
                end: 11,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 11,
                end: 12,
            },
        ],
    ),
    (
        "cat <<#x\nα\n#x\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 3,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 4,
                end: 6,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 6,
                end: 8,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 8,
                end: 9,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 9,
                end: 12,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 12,
                end: 15,
            },
        ],
    ),
    (
        "cat <<;\nα\n;\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 3,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 4,
                end: 6,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 6,
                end: 7,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 7,
                end: 8,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 8,
                end: 11,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 11,
                end: 13,
            },
        ],
    ),
    (
        "cat <<$(x)\nα\n$(x)\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 3,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 4,
                end: 6,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 6,
                end: 10,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 10,
                end: 11,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-content",
                start: 11,
                end: 14,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 14,
                end: 19,
            },
        ],
    ),
    (
        "echo '${x'",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 4,
                end: 5,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 5,
                end: 10,
            },
        ],
    ),
    (
        "echo \"${α:-$(x)}\"",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 4,
                end: 5,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 5,
                end: 18,
            },
        ],
    ),
    (
        "echo <<<A",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 4,
                end: 5,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 5,
                end: 8,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 8,
                end: 9,
            },
        ],
    ),
    (
        "echo <<-A\nA\n",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "horizontal-whitespace",
                start: 4,
                end: 5,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 5,
                end: 8,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-marker",
                start: 8,
                end: 9,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "newline",
                start: 9,
                end: 10,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "heredoc-end",
                start: 10,
                end: 12,
            },
        ],
    ),
    (
        "x;;&y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 4,
                end: 5,
            },
        ],
    ),
    (
        "x&>>y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 4,
                end: 5,
            },
        ],
    ),
    (
        "x<<<y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 4,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 4,
                end: 5,
            },
        ],
    ),
    (
        "x&&y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 3,
                end: 4,
            },
        ],
    ),
    (
        "x||y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 3,
                end: 4,
            },
        ],
    ),
    (
        "x|&y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 3,
                end: 4,
            },
        ],
    ),
    (
        "x;;y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 3,
                end: 4,
            },
        ],
    ),
    (
        "x;&y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 3,
                end: 4,
            },
        ],
    ),
    (
        "x>>y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 3,
                end: 4,
            },
        ],
    ),
    (
        "x<>y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 3,
                end: 4,
            },
        ],
    ),
    (
        "x<&y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 3,
                end: 4,
            },
        ],
    ),
    (
        "x>&y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 3,
                end: 4,
            },
        ],
    ),
    (
        "x>|y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 3,
                end: 4,
            },
        ],
    ),
    (
        "x&>y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 3,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 3,
                end: 4,
            },
        ],
    ),
    (
        "x;y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 2,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 2,
                end: 3,
            },
        ],
    ),
    (
        "x&y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 2,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 2,
                end: 3,
            },
        ],
    ),
    (
        "x|y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 2,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 2,
                end: 3,
            },
        ],
    ),
    (
        "x(y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 2,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 2,
                end: 3,
            },
        ],
    ),
    (
        "x)y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 2,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 2,
                end: 3,
            },
        ],
    ),
    (
        "x<y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 2,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 2,
                end: 3,
            },
        ],
    ),
    (
        "x>y",
        &[
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 0,
                end: 1,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "operator",
                start: 1,
                end: 2,
            },
            gerbil_parser_runtime::ScannedToken {
                terminal: "word",
                start: 2,
                end: 3,
            },
        ],
    ),
];
pub static REJECTED: &[&str] = &["cat <<A\r\nα\r\nA\r\n", "cat <<\n", "cat <<A", "cat <<A\n"];
