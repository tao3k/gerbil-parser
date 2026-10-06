//! Admission and first-character dispatch for Scheme-owned part/binding tables.
use super::{
    CaptureKind, Diagnostic, PreparedResultProfile, PreparedResultProjection, PreparedTextProfile,
    ResultProfileSpec, TextProfile,
};
use crate::scanner::{PreparedRegionPlan, RegionScope, RegionSpec};
use std::collections::{HashMap, HashSet};

/// Declared subscript opener with the Scheme-derived restricted region plan.
#[derive(Clone, Copy, Debug)]
pub struct SubscriptSpec {
    pub prefix: &'static str,
    pub closing: char,
    pub regions: &'static RegionSpec,
    pub scopes: &'static [RegionScope],
}
/// A binding recognizer and its bounded prefix/operator vocabulary.
#[derive(Clone, Copy, Debug)]
pub struct BindingSpec {
    pub name: &'static TextProfile,
    pub prefixes: &'static [&'static str],
    pub operators: &'static [&'static str],
    pub subscript: Option<SubscriptSpec>,
}
/// Guard on the character following a declared prefix.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PartGuard {
    Any,
    Next,
    Characters(&'static str),
}
/// Closed part recognition operations; node vocabulary belongs to projections.
#[derive(Clone, Copy, Debug)]
pub enum PartOpcode {
    Quote { delimiter: char },
    QuotedBody { delimiter: char },
    Pair,
    Parameter { binding: usize },
    Name { binding: usize },
    Escape,
}
/// Ordered rule shared by one or more declared contexts.
#[derive(Clone, Copy, Debug)]
pub struct PartRule {
    pub contexts: &'static [&'static str],
    pub prefix: &'static str,
    pub guard: PartGuard,
    pub opcode: PartOpcode,
    pub projection: &'static str,
    pub opening: usize,
    pub closing: usize,
}
/// Immutable part/binding tables lowered from an admitted POO profile.
#[derive(Clone, Copy, Debug)]
pub struct PartProfileSpec {
    pub contexts: &'static [&'static str],
    pub rules: &'static [PartRule],
    pub bindings: &'static [BindingSpec],
    pub assignment: Option<usize>,
    pub literal: &'static str,
}
struct PartContext {
    ascii: Box<[Vec<usize>; 128]>,
    wide: HashMap<char, Vec<usize>>,
}
impl PartContext {
    fn entries(&mut self, first: char) -> &mut Vec<usize> {
        let code = first as usize;
        if code < 128 {
            &mut self.ascii[code]
        } else {
            self.wide.entry(first).or_default()
        }
    }
    fn rules(&self, first: char) -> &[usize] {
        let code = first as usize;
        if code < 128 {
            &self.ascii[code]
        } else {
            self.wide.get(&first).map_or(&[], Vec::as_slice)
        }
    }
}
fn prepare_dispatch(spec: &PartProfileSpec) -> Result<Vec<PartContext>, Diagnostic> {
    let mut dispatch: Vec<_> = spec
        .contexts
        .iter()
        .map(|_| PartContext {
            ascii: Box::new(std::array::from_fn(|_| Vec::new())),
            wide: HashMap::new(),
        })
        .collect();
    for (index, rule) in spec.rules.iter().enumerate() {
        let first = rule
            .prefix
            .chars()
            .next()
            .ok_or_else(|| error(0, "empty part prefix"))?;
        for context in rule.contexts {
            let slot = context_index(spec, context)?;
            let entries = dispatch[slot].entries(first);
            if entries.iter().any(|i| {
                let old = &spec.rules[*i];
                old.prefix == rule.prefix && old.guard == rule.guard
            }) {
                return Err(error(0, "duplicate part recognition rule"));
            }
            entries.push(index);
        }
    }
    Ok(dispatch)
}
fn context_index(spec: &PartProfileSpec, name: &str) -> Result<usize, Diagnostic> {
    spec.contexts
        .iter()
        .position(|c| *c == name)
        .ok_or_else(|| error(0, "unknown part context"))
}
/// Once-admitted recognition tables and bound result instructions.
pub struct PreparedPartProfile {
    pub(super) spec: &'static PartProfileSpec,
    pub(super) projections: Vec<PreparedResultProjection>,
    pub(super) rule_projections: Vec<usize>,
    pub(super) literal: usize,
    pub(super) word: usize,
    pub(super) here: usize,
    pub(super) assignment: Option<usize>,
    pub(super) subscript: Option<usize>,
    pub(super) names: Vec<PreparedTextProfile>,
    pub(super) regions: PreparedRegionPlan,
    pub(super) subscripts: Vec<Option<PreparedRegionPlan>>,
    pub(super) word_context: usize,
    pub(super) here_context: usize,
    pub(super) quote_contexts: Vec<Option<usize>>,
    dispatch: Vec<PartContext>,
}
pub(super) fn error(at: usize, message: &str) -> Diagnostic {
    Diagnostic {
        reason_kind: "invalid-part-recognition",
        byte_offset: at,
        message: message.into(),
    }
}
fn unique(values: &[&str]) -> bool {
    let mut seen = HashSet::new();
    values.iter().all(|v| !v.is_empty() && seen.insert(*v))
}
const LEAF: &[(&str, CaptureKind, bool)] = &[("text", CaptureKind::Span, true)];
const PARTS: &[(&str, CaptureKind, bool)] = &[("parts", CaptureKind::Parts, false)];
const QUOTE: &[(&str, CaptureKind, bool)] = &[
    ("open", CaptureKind::Span, true),
    ("parts", CaptureKind::Parts, false),
    ("close", CaptureKind::Span, true),
];
const OPAQUE: &[(&str, CaptureKind, bool)] = &[
    ("open", CaptureKind::Span, true),
    ("body", CaptureKind::Span, false),
    ("close", CaptureKind::Span, true),
];
const PARAMETER: &[(&str, CaptureKind, bool)] = &[
    ("open", CaptureKind::Span, true),
    ("prefix", CaptureKind::Span, false),
    ("name", CaptureKind::Span, false),
    ("subscript", CaptureKind::Node, false),
    ("operator", CaptureKind::Span, false),
    ("parts", CaptureKind::Parts, false),
    ("close", CaptureKind::Span, true),
];
const ASSIGNMENT: &[(&str, CaptureKind, bool)] = &[
    ("name", CaptureKind::Span, true),
    ("operator", CaptureKind::Span, true),
    ("parts", CaptureKind::Parts, false),
];
fn bind(
    results: &PreparedResultProfile,
    projections: &mut Vec<PreparedResultProjection>,
    id: &str,
    signature: &[(&str, CaptureKind, bool)],
) -> Result<usize, Diagnostic> {
    let projection = results.bind(id)?;
    if !projection.has_signature(signature) {
        return Err(error(0, "part projection signature mismatch"));
    }
    let index = projections.len();
    projections.push(projection);
    Ok(index)
}
fn valid_subscript(sub: SubscriptSpec, base: &RegionSpec) -> bool {
    if sub.prefix.is_empty()
        || sub.regions.stops != base.stops
        || sub.regions.quotes != base.quotes
        || sub.regions.consume_initial_stop != base.consume_initial_stop
    {
        return false;
    }
    let Some(opener) = sub.prefix.chars().last() else {
        return false;
    };
    let expected = crate::scanner::RegionPair {
        prefix: sub.prefix,
        opening: opener,
        closing: sub.closing,
        depth: 1,
    };
    let existing = base.pairs.iter().find(|p| p.prefix == sub.prefix);
    if existing.is_some_and(|p| *p != expected) {
        return false;
    }
    if sub.regions.pairs.len() != base.pairs.len() + usize::from(existing.is_none())
        || !base.pairs.iter().all(|p| sub.regions.pairs.contains(p))
        || !sub.regions.pairs.contains(&expected)
    {
        return false;
    }
    let base_names: Vec<_> = base
        .pairs
        .iter()
        .filter(|p| p.prefix != sub.prefix)
        .map(|p| p.prefix)
        .collect();
    sub.scopes.len() == base_names.len() + 1
        && sub.scopes.iter().all(|s| {
            if s.prefix == sub.prefix {
                s.pairs.len() == base_names.len() + 1
                    && s.pairs.contains(&sub.prefix)
                    && base_names.iter().all(|n| s.pairs.contains(n))
            } else {
                base_names.contains(&s.prefix) && s.pairs == base_names
            }
        })
}
fn rule_signature(
    rule: &PartRule,
    spec: &PartProfileSpec,
    regions: &RegionSpec,
) -> Result<&'static [(&'static str, CaptureKind, bool)], Diagnostic> {
    Ok(match rule.opcode {
        PartOpcode::Name { binding } => {
            if rule.opening != 1 || rule.closing != 0 || binding >= spec.bindings.len() {
                return Err(error(0, "invalid name part"));
            }
            LEAF
        }
        PartOpcode::Escape => {
            if rule.opening != 1 || rule.closing != 0 {
                return Err(error(0, "invalid escape part"));
            }
            LEAF
        }
        PartOpcode::Quote { delimiter } | PartOpcode::QuotedBody { delimiter } => {
            if rule.closing != 1
                || !rule.prefix.ends_with(delimiter)
                || !regions.quotes.iter().any(|q| q.delimiter == delimiter)
            {
                return Err(error(0, "invalid quote part"));
            }
            if matches!(rule.opcode, PartOpcode::Quote { .. }) {
                if !spec.contexts.contains(&rule.projection) {
                    return Err(error(0, "missing quote context"));
                }
                QUOTE
            } else {
                OPAQUE
            }
        }
        PartOpcode::Pair => {
            if !regions
                .pairs
                .iter()
                .any(|p| p.prefix == rule.prefix && p.depth == rule.closing)
            {
                return Err(error(0, "invalid pair part"));
            }
            OPAQUE
        }
        PartOpcode::Parameter { binding } => {
            if binding >= spec.bindings.len()
                || rule.opening != 2
                || rule.closing != 1
                || !regions
                    .pairs
                    .iter()
                    .any(|p| p.prefix == rule.prefix && p.depth == rule.closing)
            {
                return Err(error(0, "invalid parameter part"));
            }
            PARAMETER
        }
    })
}
impl PreparedPartProfile {
    /// Admit recognition, region, binding and projection contracts once.
    /// # Errors
    /// Rejects malformed rules, nullable names, region conflicts and projection signatures.
    pub fn new(
        spec: &'static PartProfileSpec,
        results: &'static ResultProfileSpec,
        regions: &'static RegionSpec,
    ) -> Result<Self, Diagnostic> {
        if !unique(spec.contexts)
            || !spec.contexts.contains(&"word")
            || !spec.contexts.contains(&"HereDocument")
        {
            return Err(error(0, "part profile lacks required unique contexts"));
        }
        let results = PreparedResultProfile::new(results)?;
        let mut projections = Vec::new();
        let word = bind(&results, &mut projections, "Word", PARTS)?;
        let here = bind(&results, &mut projections, "HereDocumentLine", PARTS)?;
        let literal = bind(&results, &mut projections, spec.literal, LEAF)?;
        let regions_plan = PreparedRegionPlan::new(regions, &[])?;
        let mut names = Vec::new();
        let mut subscripts = Vec::new();
        for binding in spec.bindings {
            if !unique(binding.prefixes) || !unique(binding.operators) {
                return Err(error(0, "invalid binding vocabulary"));
            }
            names.push(
                PreparedTextProfile::new(binding.name)
                    .ok_or_else(|| error(0, "invalid binding name profile"))?,
            );
            subscripts.push(if let Some(sub) = binding.subscript {
                if !valid_subscript(sub, regions) {
                    return Err(error(0, "invalid derived binding region"));
                }
                Some(PreparedRegionPlan::new(sub.regions, sub.scopes)?)
            } else {
                None
            });
        }
        let assignment = if let Some(index) = spec.assignment {
            let b = spec
                .bindings
                .get(index)
                .ok_or_else(|| error(0, "unknown assignment binding"))?;
            if b.operators.is_empty() || !b.prefixes.is_empty() || b.subscript.is_some() {
                return Err(error(0, "invalid assignment binding"));
            }
            Some(bind(&results, &mut projections, "Assignment", ASSIGNMENT)?)
        } else {
            None
        };

        let mut rule_projections = Vec::new();
        let mut subscript = None;
        for rule in spec.rules {
            if rule.opening != rule.prefix.chars().count()
                || !unique(rule.contexts)
                || rule.contexts.is_empty()
                || !rule.contexts.iter().all(|c| spec.contexts.contains(c))
            {
                return Err(error(0, "invalid part opening or contexts"));
            }
            let signature = rule_signature(rule, spec, regions)?;
            if let PartOpcode::Parameter { binding } = rule.opcode
                && subscripts[binding].is_some()
                && subscript.is_none()
            {
                subscript = Some(bind(&results, &mut projections, "ArraySubscript", QUOTE)?);
            }
            rule_projections.push(bind(
                &results,
                &mut projections,
                rule.projection,
                signature,
            )?);
        }
        Ok(Self {
            spec,
            projections,
            rule_projections,
            literal,
            word,
            here,
            assignment,
            subscript,
            names,
            regions: regions_plan,
            subscripts,
            dispatch: prepare_dispatch(spec)?,
            word_context: context_index(spec, "word")?,
            here_context: context_index(spec, "HereDocument")?,
            quote_contexts: spec
                .rules
                .iter()
                .map(|r| spec.contexts.iter().position(|c| *c == r.projection))
                .collect(),
        })
    }
    pub(super) fn matching(
        &self,
        source: &str,
        at: usize,
        end: usize,
        context: usize,
    ) -> Option<usize> {
        let first = source.get(at..end)?.chars().next()?;
        self.dispatch[context]
            .rules(first)
            .iter()
            .copied()
            .find(|i| {
                let rule = &self.spec.rules[*i];
                if !source[at..end].starts_with(rule.prefix) {
                    return false;
                }
                let following = source[at + rule.prefix.len()..end].chars().next();
                match rule.guard {
                    PartGuard::Any => true,
                    PartGuard::Next => following.is_some(),
                    PartGuard::Characters(chars) => following.is_some_and(|ch| chars.contains(ch)),
                }
            })
    }
}
