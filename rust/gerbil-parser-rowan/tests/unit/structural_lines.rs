use super::model::{
    BlockLineRule, HeadingLineRule, KindCategory, KindSpec, LanguageSpec, LineStructureSpec,
    SyntaxNode,
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
}];
static BROKEN_BLOCKS: &[BlockLineRule] = &[BlockLineRule {
    begin_token: 2,
    ..BLOCKS[0]
}];
static STRUCTURE: LineStructureSpec = LineStructureSpec {
    grammar_digest: LANGUAGE.grammar_digest,
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

#[test]
fn sections_nest_and_blocks_mask_headlines_losslessly() {
    let source = "前言\r\n* Parent\n#+BEGIN_SRC rust\n** not heading\n  #+end_src \r\n** Child\nbody\r* Sibling\n";
    let green = parse_structural_lines(&LANGUAGE, &STRUCTURE, source).unwrap();
    let root = SyntaxNode::new_root(green);
    assert_eq!(root.to_string(), source);
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
    let green = parse_structural_lines(&LANGUAGE, &STRUCTURE, source).unwrap();
    let root = SyntaxNode::new_root(green);
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
            .reason_kind,
        "invalid-structural-aot"
    );
}
