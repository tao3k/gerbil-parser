use super::model::{
    BlockHeaderRule, BlockLineRule, HeadingFieldsRule, HeadingLineRule, InlineLinkRule,
    KeyValueLineRule, KindCategory, KindSpec, LanguageSpec, LineStructureSpec, UnclosedBlockPolicy,
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
    KindSpec {
        name: "PropertyDrawer",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "NodeProperty",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "DrawerBegin",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "DrawerEnd",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "PropertyLine",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "PropertyKey",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "PropertyValue",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "PropertyTrivia",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "HeaderArgument",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "HeaderTrivia",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "InlineLink",
        category: KindCategory::Node,
    },
    KindSpec {
        name: "LinkTarget",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "LinkDescription",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "LinkTrivia",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "HeadingTitle",
        category: KindCategory::Token,
    },
    KindSpec {
        name: "HeadingTrivia",
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
    body_line: None,
    header: None,
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
static PROPERTY_BLOCKS: &[BlockLineRule] = &[BlockLineRule {
    opening: ":PROPERTIES:",
    closing: ":END:",
    case_insensitive: true,
    indent: true,
    block_node: 9,
    begin_token: 11,
    body_token: 7,
    end_token: 12,
    unclosed: UnclosedBlockPolicy::RecoverAsText,
    heading_bound: true,
    body_line: Some(KeyValueLineRule {
        marker: b':',
        node: 10,
        key_token: 14,
        value_token: 15,
        trivia_token: 16,
    }),
    header: None,
}];
static HEADER_BLOCKS: &[BlockLineRule] = &[BlockLineRule {
    header: Some(BlockHeaderRule {
        argument_token: 17,
        trivia_token: 18,
    }),
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
        fields: None,
    },
    blocks: BLOCKS,
    text_node: 4,
    text_token: 7,
    inline_link: None,
};
static HEADING_FIELDS_STRUCTURE: LineStructureSpec = LineStructureSpec {
    heading: HeadingLineRule {
        fields: Some(HeadingFieldsRule {
            title_token: 23,
            trivia_token: 24,
        }),
        ..STRUCTURE.heading
    },
    ..STRUCTURE
};

#[test]
fn heading_fields_preserve_utf8_title_and_trailing_trivia() {
    let source = "**  中文 title  \r\n";
    let root = parse_structural_lines(&LANGUAGE, &HEADING_FIELDS_STRUCTURE, source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    let title = root
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .find(|token| token.kind().0 == 23)
        .expect("typed title token");
    assert_eq!(title.text(), "中文 title");
}
static LINK_STRUCTURE: LineStructureSpec = LineStructureSpec {
    inline_link: Some(InlineLinkRule {
        opening: "[[",
        separator: "][",
        closing: "]]",
        node: 19,
        target_token: 20,
        description_token: 21,
        trivia_token: 22,
    }),
    ..STRUCTURE
};

#[test]
fn inline_links_preserve_text_and_expose_target_and_description() {
    let source = "prefix [[https://example.test][inside]] and [[id:123]]\r\n";
    let root = parse_structural_lines(&LANGUAGE, &LINK_STRUCTURE, source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    let links: Vec<_> = root
        .descendants()
        .filter(|node| node.kind().0 == 19)
        .collect();
    assert_eq!(links.len(), 2);
    let target: Vec<_> = links[0]
        .children_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| token.kind().0 == 20)
        .map(|token| token.text().to_owned())
        .collect();
    assert_eq!(target, ["https://example.test"]);
}
static RECOVER_STRUCTURE: LineStructureSpec = LineStructureSpec {
    blocks: RECOVER_BLOCKS,
    ..STRUCTURE
};
static HEADING_BOUND_STRUCTURE: LineStructureSpec = LineStructureSpec {
    blocks: HEADING_BOUND_BLOCKS,
    ..STRUCTURE
};
static PROPERTY_STRUCTURE: LineStructureSpec = LineStructureSpec {
    blocks: PROPERTY_BLOCKS,
    ..STRUCTURE
};
static HEADER_STRUCTURE: LineStructureSpec = LineStructureSpec {
    blocks: HEADER_BLOCKS,
    ..STRUCTURE
};

#[test]
fn block_header_argument_is_typed_without_losing_trivia() {
    let source = "* One\n  #+BEGIN_SRC  org-contract :results raw\r\nbody\n#+END_SRC\n";
    let parsed = parse_structural_lines(&LANGUAGE, &HEADER_STRUCTURE, source).unwrap();
    let root = parsed.syntax();
    assert_eq!(root.to_string(), source);
    let arguments: Vec<_> = root
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| token.kind().0 == 17)
        .map(|token| token.text().to_owned())
        .collect();
    assert_eq!(arguments, ["org-contract"]);
}

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
fn key_value_block_emits_typed_lines_with_exact_spans() {
    let source = "* Task\n:PROPERTIES:\n:CONTRACT_ORG: one\n:ID: café\n:END:\n";
    let root = parse_structural_lines(&LANGUAGE, &PROPERTY_STRUCTURE, source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    let properties: Vec<_> = root
        .descendants()
        .filter(|node| node.kind().0 == 10)
        .collect();
    assert_eq!(properties.len(), 2);
    assert_eq!(properties[0].to_string(), ":CONTRACT_ORG: one\n");
    assert_eq!(properties[1].to_string(), ":ID: café\n");
    let keys: Vec<_> = root
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| token.kind().0 == 14)
        .map(|token| token.text().to_string())
        .collect();
    let values: Vec<_> = root
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| token.kind().0 == 15)
        .map(|token| token.text().to_string())
        .collect();
    assert_eq!(keys, ["CONTRACT_ORG", "ID"]);
    assert_eq!(values, ["one", "café"]);
    assert_eq!(usize::from(properties[0].text_range().start()), 20);
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 9).count(),
        1
    );
}

#[test]
fn malformed_key_value_line_rejects_the_whole_block() {
    let source = ":PROPERTIES:\nnot a property\n:END:\n* Next\n";
    let root = parse_structural_lines(&LANGUAGE, &PROPERTY_STRUCTURE, source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 9).count(),
        0
    );
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 2).count(),
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
