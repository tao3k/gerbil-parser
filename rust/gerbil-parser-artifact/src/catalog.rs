//! An owned vocabulary published by the Scheme parser, independent of LR tables.
use crate::NativeArtifactError;
use serde_json::Value;

/// Category of a published syntax kind.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum KindCategory {
    Node,
    Token,
}
/// A named published kind with its allowed fields.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NativeKind {
    pub name: String,
    pub category: KindCategory,
    pub fields: Vec<String>,
}
/// A named token class emitted by the scanner.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NativeTerminal {
    pub name: String,
    /// Syntax token kind, when published by a grammar catalog.
    pub syntax_kind: Option<usize>,
}
/// The immutable catalog and digest obtained from the same native language handle.
#[derive(Clone, Debug)]
pub struct NativeCatalog {
    pub(crate) language: String,
    pub(crate) grammar_digest: String,
    pub(crate) root_kind: usize,
    pub(crate) kinds: Vec<NativeKind>,
    pub(crate) terminals: Vec<NativeTerminal>,
    pub(crate) fields: Vec<String>,
}
fn invalid() -> NativeArtifactError {
    crate::wire::error("descriptor", None)
}
fn text(value: &Value) -> Result<String, NativeArtifactError> {
    value
        .as_str()
        .filter(|s| !s.is_empty())
        .map(str::to_owned)
        .ok_or_else(invalid)
}
fn texts(value: &Value) -> Result<Vec<String>, NativeArtifactError> {
    value
        .as_array()
        .ok_or_else(invalid)?
        .iter()
        .map(text)
        .collect()
}
fn distinct<'a>(values: impl IntoIterator<Item = &'a str>) -> bool {
    let mut seen = std::collections::HashSet::new();
    values.into_iter().all(|v| seen.insert(v))
}
impl NativeCatalog {
    /// Decode the native descriptor; never import a generated parser specification.
    /// # Errors
    /// Rejects malformed schemas, catalogs, identities or undeclared fields/roots.
    pub fn from_descriptor(bytes: &[u8]) -> Result<Self, NativeArtifactError> {
        let value = crate::datum::read(bytes)?;
        if value["schema"] != "gerbil-parser.native-descriptor.v1" {
            return Err(invalid());
        }
        let language = text(&value["language"])?;
        let grammar_digest = text(&value["grammarDigest"])?;
        let hex = grammar_digest.strip_prefix("sha256:").ok_or_else(invalid)?;
        if hex.len() != 64
            || !hex
                .bytes()
                .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
        {
            return Err(invalid());
        }
        let fields = texts(&value["fields"])?;
        let kinds = read_kinds(&value["syntaxKinds"], &fields)?;
        let terminals = read_terminals(&value["terminals"], &kinds)?;
        let root_name = text(&value["rootKind"])?;
        let root_kind = kinds
            .iter()
            .position(|k| k.name == root_name && k.category == KindCategory::Node)
            .ok_or_else(invalid)?;
        if !distinct(kinds.iter().map(|k| k.name.as_str()))
            || !distinct(terminals.iter().map(|t| t.name.as_str()))
            || !distinct(fields.iter().map(String::as_str))
        {
            return Err(invalid());
        }
        Ok(Self {
            language,
            grammar_digest,
            root_kind,
            kinds,
            terminals,
            fields,
        })
    }
    /// Language identity published by the handle.
    #[must_use]
    pub fn language(&self) -> &str {
        &self.language
    }
    /// Exact parser product digest, including contextual identity when present.
    #[must_use]
    pub fn grammar_digest(&self) -> &str {
        &self.grammar_digest
    }
    /// Declared root kind name.
    #[must_use]
    pub fn root_kind(&self) -> &str {
        &self.kinds[self.root_kind].name
    }
    /// Published node kinds and their field declarations.
    #[must_use]
    pub fn kinds(&self) -> &[NativeKind] {
        &self.kinds
    }
    /// Published field names in wire-index order.
    #[must_use]
    pub fn fields(&self) -> &[String] {
        &self.fields
    }
    /// Published terminal classes in wire-index order.
    #[must_use]
    pub fn terminals(&self) -> &[NativeTerminal] {
        &self.terminals
    }
}

fn read_kinds(value: &Value, fields: &[String]) -> Result<Vec<NativeKind>, NativeArtifactError> {
    let declared: std::collections::HashSet<_> = fields.iter().collect();
    let mut kinds = Vec::new();
    for row in value.as_array().ok_or_else(invalid)? {
        let row = row
            .as_array()
            .filter(|r| r.len() == 3)
            .ok_or_else(invalid)?;
        let category = match row[1].as_str() {
            Some("result" | "node") => KindCategory::Node,
            Some("token") => KindCategory::Token,
            _ => return Err(invalid()),
        };
        let names = texts(&row[2])?;
        if !distinct(names.iter().map(String::as_str))
            || names.iter().any(|n| !declared.contains(n))
        {
            return Err(invalid());
        }
        kinds.push(NativeKind {
            name: text(&row[0])?,
            category,
            fields: names,
        });
    }
    Ok(kinds)
}
fn read_terminals(
    value: &Value,
    kinds: &[NativeKind],
) -> Result<Vec<NativeTerminal>, NativeArtifactError> {
    let token_kinds: std::collections::HashMap<_, _> = kinds
        .iter()
        .enumerate()
        .filter(|(_, kind)| kind.category == KindCategory::Token)
        .map(|(index, kind)| (kind.name.as_str(), index))
        .collect();
    let mut terminals = Vec::new();
    for row in value.as_array().ok_or_else(invalid)? {
        let row = row
            .as_array()
            .filter(|r| r.len() == 2)
            .ok_or_else(invalid)?;
        let token_kind = text(&row[1])?;
        let syntax_kind = token_kinds.get(token_kind.as_str()).copied();
        if syntax_kind.is_none() && token_kind != "token" {
            return Err(invalid());
        }
        terminals.push(NativeTerminal {
            name: text(&row[0])?,
            syntax_kind,
        });
    }
    Ok(terminals)
}
