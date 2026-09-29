//! Language-neutral, validated preorder index for AOT Rowan graph queries.

use std::collections::HashSet;

use super::graph_projection::GraphRecord;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GraphIndexError {
    InvalidRecord,
    InvalidScope,
    InvalidTarget,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GraphRelation {
    Any,
    At,
    ChildOf,
    DescendantOf,
}

#[derive(Clone, Debug)]
pub struct GraphIndex {
    subtree_end: Vec<usize>,
}

impl GraphIndex {
    /// Validate preorder ancestry once, then reuse its subtree intervals.
    ///
    /// # Errors
    /// Rejects out-of-order IDs, non-preorder parents, and inconsistent child IDs.
    pub fn new(records: &[GraphRecord]) -> Result<Self, GraphIndexError> {
        let mut subtree_end = vec![records.len(); records.len()];
        let mut stack = Vec::<usize>::new();
        let mut child_counts = vec![0usize; records.len()];
        for (id, record) in records.iter().enumerate() {
            if record.id != id {
                return Err(GraphIndexError::InvalidRecord);
            }
            while stack.last().copied() != record.parent_id {
                let Some(closed) = stack.pop() else {
                    return Err(GraphIndexError::InvalidRecord);
                };
                subtree_end[closed] = id;
            }
            if let Some(parent) = record.parent_id {
                let children = &records[parent].child_ids;
                if children.get(child_counts[parent]) != Some(&id) {
                    return Err(GraphIndexError::InvalidRecord);
                }
                child_counts[parent] += 1;
            }
            stack.push(id);
        }
        for id in stack {
            subtree_end[id] = records.len();
        }
        if records
            .iter()
            .enumerate()
            .any(|(id, record)| record.child_ids.len() != child_counts[id])
        {
            return Err(GraphIndexError::InvalidRecord);
        }
        Ok(Self { subtree_end })
    }

    #[must_use]
    pub fn subtree_end(&self, id: usize) -> Option<usize> {
        self.subtree_end.get(id).copied()
    }

    #[must_use]
    pub fn contains(&self, ancestor: usize, node: usize) -> bool {
        self.subtree_end(ancestor)
            .is_some_and(|end| ancestor <= node && node < end)
    }

    /// Find the nearest matching *strict* ancestor of every preorder record.
    /// Each predicate result is computed once; the projection is linear in the
    /// graph size, regardless of nesting depth.
    ///
    /// # Errors
    /// Rejects records that do not belong to an index of this size.
    pub fn nearest_ancestors_matching<F>(
        &self,
        records: &[GraphRecord],
        mut predicate: F,
    ) -> Result<Vec<Option<usize>>, GraphIndexError>
    where
        F: FnMut(&GraphRecord) -> bool,
    {
        if records.len() != self.subtree_end.len() {
            return Err(GraphIndexError::InvalidRecord);
        }
        let mut nearest = Vec::with_capacity(records.len());
        let mut matches = Vec::with_capacity(records.len());
        for record in records {
            let ancestor = match record.parent_id {
                Some(parent) if parent < nearest.len() => {
                    if matches[parent] {
                        Some(parent)
                    } else {
                        nearest[parent]
                    }
                }
                Some(_) => return Err(GraphIndexError::InvalidRecord),
                None => None,
            };
            nearest.push(ancestor);
            matches.push(predicate(record));
        }
        Ok(nearest)
    }

    /// Select graph records inside one scope using a language-owned predicate.
    ///
    /// # Errors
    /// Rejects a missing scope or target; records must match this index.
    pub fn select<F>(
        &self,
        records: &[GraphRecord],
        scope: usize,
        kind: &str,
        relation: GraphRelation,
        targets: &[usize],
        mut predicate: F,
    ) -> Result<Vec<usize>, GraphIndexError>
    where
        F: FnMut(&GraphRecord) -> bool,
    {
        let Some(end) = self.subtree_end(scope) else {
            return Err(GraphIndexError::InvalidScope);
        };
        if records.len() != self.subtree_end.len() {
            return Err(GraphIndexError::InvalidRecord);
        }
        if targets.iter().any(|&id| id >= records.len()) {
            return Err(GraphIndexError::InvalidTarget);
        }
        let intervals =
            (relation == GraphRelation::DescendantOf).then(|| self.descendant_intervals(targets));
        let target_set = (targets.len() > 8
            && matches!(relation, GraphRelation::At | GraphRelation::ChildOf))
        .then(|| targets.iter().copied().collect::<HashSet<_>>());
        let contains_target = |id| {
            target_set
                .as_ref()
                .map_or_else(|| targets.contains(&id), |set| set.contains(&id))
        };
        Ok(records[scope..end]
            .iter()
            .filter(|record| {
                record.kind == kind
                    && match relation {
                        GraphRelation::Any => true,
                        GraphRelation::At => contains_target(record.id),
                        GraphRelation::ChildOf => record.parent_id.is_some_and(contains_target),
                        GraphRelation::DescendantOf => {
                            let ranges = intervals.as_deref().unwrap_or_default();
                            let position = ranges.partition_point(|&(start, _)| start <= record.id);
                            position > 0 && record.id < ranges[position - 1].1
                        }
                    }
                    && predicate(record)
            })
            .map(|record| record.id)
            .collect())
    }

    fn descendant_intervals(&self, targets: &[usize]) -> Vec<(usize, usize)> {
        let mut ranges: Vec<_> = targets
            .iter()
            .filter_map(|&target| {
                let start = target + 1;
                let end = self.subtree_end[target];
                (start < end).then_some((start, end))
            })
            .collect();
        ranges.sort_unstable_by_key(|&(start, _)| start);
        let mut merged = Vec::<(usize, usize)>::with_capacity(ranges.len());
        for (start, end) in ranges {
            if let Some(last) = merged.last_mut()
                && start <= last.1
            {
                last.1 = last.1.max(end);
            } else {
                merged.push((start, end));
            }
        }
        merged
    }
}
