use super::model::{
    BlockLineRule, HeadingLineRule, KindCategory, KindSpec, LanguageSpec, LineStructureSpec,
    UnclosedBlockPolicy,
};
use super::structural_lines::parse_structural_lines;

static KINDS: &[KindSpec] = &[
    KindSpec {
        name: "Document",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "Section",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "Headline",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "SourceBlock",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "TextLine",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "HeadlineToken",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "BeginToken",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "TextToken",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "EndToken",
        category: KindCategory::Token,
    },
];
static LANGUAGE: LanguageSpec = LanguageSpec {
    language: "structure-test",
    version: "v1",
    contract: "structure-test.v1",
    grammar_digest: "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    case_insensitive: false,
    root_kind: 0,
    kinds: KINDS,
    terminals: &[],
    lexical_rules: &[],
    actions: &[],
    gotos: &[],
    productions: &[],
};
static BLOCKS: &[BlockLineRule] = &[BlockLineRule {
    opening: "#+begin_src",
    closing: "#+end_src",
    case_insensitive: true,
    indent: true,
    block_node: 3,
    begin_token: 6,
    body_token: 7,
    end_token: 8,
    unclosed: UnclosedBlockPolicy::CloseAtEof,
    heading_bound: false,
}];
static RECOVER_BLOCKS: &[BlockLineRule] = &[BlockLineRule {
    unclosed: UnclosedBlockPolicy::RecoverAsText,
    ..BLOCKS[0]
}];
static HEADING_BOUND_BLOCKS: &[BlockLineRule] = &[BlockLineRule {
    unclosed: UnclosedBlockPolicy::RecoverAsText,
    heading_bound: true,
    ..BLOCKS[0]
}];
static BROKEN_BLOCKS: &[BlockLineRule] = &[BlockLineRule {
    begin_token: 2,
    ..BLOCKS[0]
}];
static STRUCTURE: LineStructureSpec = LineStructureSpec {
    grammar_digest: LANGUAGE.grammar_digest,
    parser_digest: "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    heading: HeadingLineRule {
        marker: b'*',
        separator: b' ',
        section_node: 1,
        heading_node: 2,
        heading_token: 5,
    },
    blocks: BLOCKS,
    text_node: 4,
    text_token: 7,
};
static RECOVER_STRUCTURE: LineStructureSpec = LineStructureSpec {
    blocks: RECOVER_BLOCKS,
    ..STRUCTURE
};
static HEADING_BOUND_STRUCTURE: LineStructureSpec = LineStructureSpec {
    blocks: HEADING_BOUND_BLOCKS,
    ..STRUCTURE
};

#[test]
fn sections_nest_and_blocks_mask_headlines_losslessly() {
    let source = "前言\r\n* Parent\n#+BEGIN_SRC rust\n** not heading\n  #+end_src \r\n** Child\nbody\r* Sibling\n";
    let parsed = parse_structural_lines(&LANGUAGE, &STRUCTURE, source).unwrap();
    let root = parsed.syntax();
    assert_eq!(root.to_string(), source);
    assert_eq!(parsed.receipt().language, LANGUAGE.language);
    assert_eq!(parsed.receipt().grammar_digest, LANGUAGE.grammar_digest);
    assert_eq!(
        parsed.receipt().parser_digest,
        Some(STRUCTURE.parser_digest)
    );
    assert_eq!(parsed.receipt().source_digest.len(), 71);
    assert_eq!(
        parsed.selective_glr_receipt().winner_reason,
        "deterministic-structure"
    );
    let sections: Vec<_> = root
        .descendants()
        .filter(|node| node.kind().0 == 1)
        .collect();
    assert_eq!(sections.len(), 3);
    assert_eq!(sections[0].parent().unwrap().kind().0, 0);
    assert_eq!(sections[1].parent().unwrap().kind().0, 1);
    assert_eq!(sections[2].parent().unwrap().kind().0, 0);
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 2).count(),
        3
    );
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 3).count(),
        1
    );
}

#[test]
fn unclosed_block_recovers_at_eof_and_preserves_bytes() {
    let source = "* One\n#+begin_src\n** data\n";
    let parsed = parse_structural_lines(&LANGUAGE, &STRUCTURE, source).unwrap();
    let root = parsed.syntax();
    assert_eq!(root.to_string(), source);
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 2).count(),
        1
    );
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 3).count(),
        1
    );
}

#[test]
fn declared_text_recovery_keeps_following_headline_visible() {
    let source = "* One\n#+begin_src\n** data\n";
    let parsed = parse_structural_lines(&LANGUAGE, &RECOVER_STRUCTURE, source).unwrap();
    let root = parsed.syntax();
    assert_eq!(root.to_string(), source);
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 2).count(),
        2
    );
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 3).count(),
        0
    );
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 4).count(),
        1
    );
}

#[test]
fn declared_text_recovery_still_masks_headlines_in_closed_blocks() {
    let source = "* One\n#+begin_src\n** data\n#+end_src\n** Two\n";
    let parsed = parse_structural_lines(&LANGUAGE, &RECOVER_STRUCTURE, source).unwrap();
    let root = parsed.syntax();
    assert_eq!(root.to_string(), source);
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 2).count(),
        2
    );
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 3).count(),
        1
    );
}

#[test]
fn many_incomplete_openers_recover_without_repeated_suffix_scans() {
    let source = "#+begin_src\n".repeat(10_000);
    let parsed = parse_structural_lines(&LANGUAGE, &RECOVER_STRUCTURE, &source).unwrap();
    let root = parsed.syntax();
    assert_eq!(root.to_string(), source);
    assert_eq!(root.children().count(), 10_000);
}

#[test]
fn heading_boundary_prevents_a_later_closer_from_claiming_a_block() {
    let source = "* One\n#+begin_src rust\n** Next\n#+end_src\n";
    let root = parse_structural_lines(&LANGUAGE, &HEADING_BOUND_STRUCTURE, source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 2).count(),
        2
    );
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 3).count(),
        0
    );
}

#[test]
fn heading_boundary_cache_does_not_hide_a_later_closed_block() {
    let source = "#+begin_src\n* Next\n#+begin_src\nbody\n#+end_src\n";
    let root = parse_structural_lines(&LANGUAGE, &HEADING_BOUND_STRUCTURE, source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 2).count(),
        1
    );
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 3).count(),
        1
    );
}

#[test]
fn wrong_category_in_unreached_rule_fails_closed() {
    let mut broken = STRUCTURE;
    broken.blocks = BROKEN_BLOCKS;
    assert_eq!(
        parse_structural_lines(&LANGUAGE, &broken, "")
            .unwrap_err()
            .diagnostic
            .reason_kind,
        "invalid-structural-aot"
    );
}

#[test]
fn malformed_parser_identity_fails_closed_with_the_attempted_receipt() {
    let mut broken = STRUCTURE;
    broken.parser_digest = "unversioned";
    let error = parse_structural_lines(&LANGUAGE, &broken, "* Heading\n").unwrap_err();
    assert_eq!(error.diagnostic.reason_kind, "invalid-structural-aot");
    assert_eq!(error.receipt.parser_digest, Some("unversioned"));
}
