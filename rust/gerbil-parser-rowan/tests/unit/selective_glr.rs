use super::model::{
    ActionEntry, GotoEntry, KindCategory, KindSpec, LanguageSpec, LexicalExpr, LexicalRule,
    Operand, OperandAction, ParserAction, Production, Reduction, Symbol, Terminal, TerminalSpec,
};
use super::parser::parse;

const DIGEST: &str = "sha256:0000000000000000000000000000000000000000000000000000000000000000";
static KINDS: &[KindSpec] = &[
    KindSpec {
        name: "RootA",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "RootB",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "Identifier",
        category: KindCategory::Token,
    },
];
static TERMINALS: &[TerminalSpec] = &[TerminalSpec {
    name: "identifier",
    syntax_kind: 2,
}];
static LEXICAL_RULES: &[LexicalRule] = &[LexicalRule {
    terminal: "identifier",
    expression: LexicalExpr::Identifier,
    precedence: 0,
    extra: false,
}];
static ACTION_0: &[ActionEntry] = &[ActionEntry {
    terminal: Terminal::Token("identifier"),
    action: ParserAction::Shift(1),
}];
static ACTION_1: &[ActionEntry] = &[ActionEntry {
    terminal: Terminal::Eof,
    action: ParserAction::Fork(&[ParserAction::Reduce(3), ParserAction::Reduce(4)]),
}];
static ACTION_2: &[ActionEntry] = &[ActionEntry {
    terminal: Terminal::Eof,
    action: ParserAction::Reduce(1),
}];
static ACTION_3: &[ActionEntry] = &[ActionEntry {
    terminal: Terminal::Eof,
    action: ParserAction::Reduce(2),
}];
static ACTION_4: &[ActionEntry] = &[ActionEntry {
    terminal: Terminal::Eof,
    action: ParserAction::Accept,
}];
static ACTIONS: &[&[ActionEntry]] = &[ACTION_0, ACTION_1, ACTION_2, ACTION_3, ACTION_4];
static GOTO_0: &[GotoEntry] = &[
    GotoEntry {
        nonterminal: "A",
        state: 2,
    },
    GotoEntry {
        nonterminal: "B",
        state: 3,
    },
    GotoEntry {
        nonterminal: "S",
        state: 4,
    },
];
static GOTOS: &[&[GotoEntry]] = &[GOTO_0, &[], &[], &[], &[]];
static AUGMENTED_RHS: &[Operand] = &[Operand {
    symbol: Symbol::Nonterminal("S"),
    actions: &[],
}];
static S_A_RHS: &[Operand] = &[Operand {
    symbol: Symbol::Nonterminal("A"),
    actions: &[],
}];
static S_B_RHS: &[Operand] = &[Operand {
    symbol: Symbol::Nonterminal("B"),
    actions: &[],
}];
static A_RHS: &[Operand] = &[Operand {
    symbol: Symbol::Terminal(Terminal::Token("identifier")),
    actions: &[OperandAction::Alias(0)],
}];
static B_EQUIVALENT_RHS: &[Operand] = &[Operand {
    symbol: Symbol::Terminal(Terminal::Token("identifier")),
    actions: &[OperandAction::Alias(0)],
}];
static B_DISTINCT_RHS: &[Operand] = &[Operand {
    symbol: Symbol::Terminal(Terminal::Token("identifier")),
    actions: &[OperandAction::Alias(1)],
}];

macro_rules! productions {
    ($name:ident, $b_rhs:ident, $a_score:expr, $b_score:expr) => {
        static $name: &[Production] = &[
            Production {
                lhs: "$accept",
                rhs: AUGMENTED_RHS,
                reduction: Reduction::Pass,
                dynamic_precedence: 0,
            },
            Production {
                lhs: "S",
                rhs: S_A_RHS,
                reduction: Reduction::Pass,
                dynamic_precedence: 0,
            },
            Production {
                lhs: "S",
                rhs: S_B_RHS,
                reduction: Reduction::Pass,
                dynamic_precedence: 0,
            },
            Production {
                lhs: "A",
                rhs: A_RHS,
                reduction: Reduction::Pass,
                dynamic_precedence: $a_score,
            },
            Production {
                lhs: "B",
                rhs: $b_rhs,
                reduction: Reduction::Pass,
                dynamic_precedence: $b_score,
            },
        ];
    };
}

productions!(EQUIVALENT_PRODUCTIONS, B_EQUIVALENT_RHS, 0, 0);
productions!(DYNAMIC_PRODUCTIONS, B_DISTINCT_RHS, 1, 2);
productions!(DISTINCT_PRODUCTIONS, B_DISTINCT_RHS, 0, 0);

const BASE_LANGUAGE: LanguageSpec = LanguageSpec {
    language: "selective-glr-test",
    version: "v1",
    contract: "selective-glr-test.v1",
    grammar_digest: DIGEST,
    case_insensitive: false,
    root_kind: 0,
    kinds: KINDS,
    terminals: TERMINALS,
    lexical_rules: LEXICAL_RULES,
    actions: ACTIONS,
    gotos: GOTOS,
    productions: EQUIVALENT_PRODUCTIONS,
};
static EQUIVALENT_LANGUAGE: LanguageSpec = BASE_LANGUAGE;
static DYNAMIC_LANGUAGE: LanguageSpec = LanguageSpec {
    root_kind: 1,
    productions: DYNAMIC_PRODUCTIONS,
    ..BASE_LANGUAGE
};
static DISTINCT_LANGUAGE: LanguageSpec = LanguageSpec {
    productions: DISTINCT_PRODUCTIONS,
    ..BASE_LANGUAGE
};

#[test]
fn equivalent_forks_merge_with_a_complete_receipt() {
    let parsed = parse(&EQUIVALENT_LANGUAGE, "x").expect("equivalent fork");
    assert_eq!(parsed.kind_name(parsed.syntax().kind()), Some("RootA"));
    let receipt = parsed.selective_glr_receipt();
    assert_eq!(receipt.branches_explored, 2);
    assert_eq!(receipt.speculative_branches_explored, 1);
    assert_eq!(receipt.merged_branches, 1);
    assert_eq!(receipt.successful_completions, 2);
    assert_eq!(receipt.distinct_completions, 1);
    assert_eq!(receipt.winner_reason, "equivalent-merge");
}

#[test]
fn dynamic_precedence_selects_the_highest_complete_branch() {
    let parsed = parse(&DYNAMIC_LANGUAGE, "x").expect("ranked fork");
    assert_eq!(parsed.kind_name(parsed.syntax().kind()), Some("RootB"));
    let receipt = parsed.selective_glr_receipt();
    assert_eq!(receipt.successful_completions, 2);
    assert_eq!(receipt.distinct_completions, 2);
    assert_eq!(receipt.winner_reason, "dynamic-precedence");
    assert_eq!(receipt.dynamic_score, 2);
}

#[test]
fn equal_score_distinct_forks_fail_closed() {
    let error = parse(&DISTINCT_LANGUAGE, "x").expect_err("ambiguous fork");
    assert_eq!(error.diagnostic.reason_kind, "selective-glr-ambiguity");
    let receipt = error.selective_glr.expect("ambiguity evidence");
    assert_eq!(receipt.successful_completions, 2);
    assert_eq!(receipt.distinct_completions, 2);
    assert_eq!(receipt.winner_reason, "ambiguous");
}
