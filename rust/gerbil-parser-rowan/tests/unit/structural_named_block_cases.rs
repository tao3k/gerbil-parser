use super::{
    BLOCKS, BlockContents, BlockHeaderRule, BlockLineRule, BlockOpeningMode, LANGUAGE,
    LineStructureSpec, STRUCTURE, UnclosedBlockPolicy, parse_structural_lines,
};

static NAMED_BLOCKS: &[BlockLineRule] = &[BlockLineRule {
    opening: ":",
    opening_mode: BlockOpeningMode::NamedDelimited,
    closing: ":END:",
    heading_bound: true,
    unclosed: UnclosedBlockPolicy::RecoverAsText,
    contents: BlockContents::Elements,
    header: Some(BlockHeaderRule {
        argument_token: 17,
        trivia_token: 18,
    }),
    ..BLOCKS[0]
}];
static NAMED_BLOCK_STRUCTURE: LineStructureSpec = LineStructureSpec {
    blocks: NAMED_BLOCKS,
    ..STRUCTURE
};

static REQUIRED_NAME_BLOCKS: &[BlockLineRule] = &[BlockLineRule {
    opening: "#+BEGIN:",
    opening_mode: BlockOpeningMode::RequiredNamedArgument,
    closing: "#+END:",
    header: Some(BlockHeaderRule {
        argument_token: 17,
        trivia_token: 18,
    }),
    ..BLOCKS[0]
}];
static REQUIRED_NAME_STRUCTURE: LineStructureSpec = LineStructureSpec {
    blocks: REQUIRED_NAME_BLOCKS,
    ..STRUCTURE
};

#[test]
fn named_delimited_block_projects_name_and_recovers_at_heading() {
    let source = "* One\n :NOTE: \r\nbody\n:END:\n** Two\n";
    let root = parse_structural_lines(&LANGUAGE, &NAMED_BLOCK_STRUCTURE, source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    let names: Vec<_> = root
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| token.kind().0 == 17)
        .map(|token| token.text().to_owned())
        .collect();
    assert_eq!(names, ["NOTE"]);
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 3).count(),
        1
    );

    let unclosed = "* One\n:NOTE:\nbody\n** Two\n";
    let recovered = parse_structural_lines(&LANGUAGE, &NAMED_BLOCK_STRUCTURE, unclosed)
        .unwrap()
        .syntax();
    assert_eq!(recovered.to_string(), unclosed);
    assert_eq!(
        recovered
            .descendants()
            .filter(|node| node.kind().0 == 3)
            .count(),
        0
    );
    assert_eq!(
        recovered
            .descendants()
            .filter(|node| node.kind().0 == 2)
            .count(),
        2
    );
}

#[test]
fn many_unclosed_named_blocks_recover_without_suffix_rescans() {
    let source = ":NOTE:\n".repeat(10_000);
    let root = parse_structural_lines(&LANGUAGE, &NAMED_BLOCK_STRUCTURE, &source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    assert_eq!(
        root.descendants().filter(|node| node.kind().0 == 3).count(),
        0
    );
}

#[test]
fn required_name_block_rejects_empty_or_malformed_headers() {
    for source in ["#+BEGIN:\n#+END:\n", "#+BEGIN: 9clock\n#+END:\n"] {
        let root = parse_structural_lines(&LANGUAGE, &REQUIRED_NAME_STRUCTURE, source)
            .unwrap()
            .syntax();
        assert_eq!(root.to_string(), source);
        assert_eq!(
            root.descendants().filter(|node| node.kind().0 == 3).count(),
            0
        );
    }
    let source = "#+BEGIN: clocktable :scope file\nbody\n#+END:\n";
    let root = parse_structural_lines(&LANGUAGE, &REQUIRED_NAME_STRUCTURE, source)
        .unwrap()
        .syntax();
    assert_eq!(root.to_string(), source);
    let names: Vec<_> = root
        .descendants_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .filter(|token| token.kind().0 == 17)
        .map(|token| token.text().to_owned())
        .collect();
    assert_eq!(names, ["clocktable"]);
}
