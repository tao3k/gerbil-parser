use super::{GraphIndex, GraphIndexError, GraphRecord, GraphRelation};
use gerbil_parser_artifact::syntax::{TextRange, TextSize};

fn record(
    id: usize,
    parent_id: Option<usize>,
    children: &[usize],
    kind: &'static str,
) -> GraphRecord {
    GraphRecord {
        id,
        parent_id,
        child_ids: children.to_vec(),
        syntax_kind: 0,
        category: "element",
        kind,
        range: TextRange::empty(TextSize::from(0)),
        fields: Vec::new(),
    }
}

fn fixture() -> Vec<GraphRecord> {
    vec![
        record(0, None, &[1, 5], "root"),
        record(1, Some(0), &[2, 4], "section"),
        record(2, Some(1), &[3], "section"),
        record(3, Some(2), &[], "item"),
        record(4, Some(1), &[], "item"),
        record(5, Some(0), &[6], "section"),
        record(6, Some(5), &[], "item"),
    ]
}

#[test]
fn preorder_index_reuses_subtrees_and_relationships() {
    let records = fixture();
    let index = GraphIndex::new(&records).unwrap();
    assert_eq!(index.subtree_end(1), Some(5));
    assert_eq!(index.subtree_end(5), Some(7));
    assert!(index.contains(1, 3));
    assert!(!index.contains(1, 6));
    assert_eq!(
        index.select(
            &records,
            0,
            "item",
            GraphRelation::DescendantOf,
            &[1, 2],
            |_| true
        ),
        Ok(vec![3, 4])
    );
    assert_eq!(
        index.select(&records, 1, "item", GraphRelation::ChildOf, &[1], |_| true),
        Ok(vec![4])
    );
    assert_eq!(
        index.select(&records, 1, "section", GraphRelation::At, &[1], |_| true),
        Ok(vec![1])
    );
    assert_eq!(
        index.select(
            &records,
            1,
            "item",
            GraphRelation::Any,
            &[],
            |record| record.id == 3
        ),
        Ok(vec![3])
    );
}

#[test]
fn malformed_graphs_and_scopes_fail_closed() {
    let mut records = fixture();
    records[3].id = 7;
    assert!(matches!(
        GraphIndex::new(&records),
        Err(GraphIndexError::InvalidRecord)
    ));

    let mut records = fixture();
    records[1].child_ids.clear();
    assert!(matches!(
        GraphIndex::new(&records),
        Err(GraphIndexError::InvalidRecord)
    ));

    let records = fixture();
    let index = GraphIndex::new(&records).unwrap();
    assert_eq!(
        index.select(&records, 10, "item", GraphRelation::Any, &[], |_| true),
        Err(GraphIndexError::InvalidScope)
    );
    assert_eq!(
        index.select(&records, 0, "item", GraphRelation::At, &[10], |_| true),
        Err(GraphIndexError::InvalidTarget)
    );
}

#[test]
fn many_targets_preserve_preorder_and_merge_nested_descendants() {
    let records = fixture();
    let index = GraphIndex::new(&records).unwrap();
    let many_targets = [0, 1, 2, 3, 4, 5, 6, 0, 1];
    assert_eq!(
        index.select(
            &records,
            0,
            "item",
            GraphRelation::At,
            &many_targets,
            |_| true
        ),
        Ok(vec![3, 4, 6])
    );
    assert_eq!(
        index.select(
            &records,
            0,
            "item",
            GraphRelation::ChildOf,
            &many_targets,
            |_| true
        ),
        Ok(vec![3, 4, 6])
    );
    assert_eq!(
        index.select(
            &records,
            0,
            "item",
            GraphRelation::DescendantOf,
            &[1, 2, 5],
            |_| true
        ),
        Ok(vec![3, 4, 6])
    );
}

#[test]
fn nearest_matching_ancestor_is_strict_and_linear_in_depth() {
    let records = fixture();
    let index = GraphIndex::new(&records).unwrap();
    let mut calls = 0;
    let nearest = index
        .nearest_ancestors_matching(&records, |record| {
            calls += 1;
            record.kind == "section"
        })
        .unwrap();
    assert_eq!(
        nearest,
        vec![None, None, Some(1), Some(2), Some(1), None, Some(5)]
    );
    assert_eq!(calls, records.len());
    assert_eq!(
        index.nearest_ancestors_matching(&records[..1], |_| true),
        Err(GraphIndexError::InvalidRecord)
    );
}
