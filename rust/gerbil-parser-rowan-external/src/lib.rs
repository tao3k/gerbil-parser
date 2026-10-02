//! Opt-in event handoff for externally maintained parsers.
//!
//! This crate validates a lossless CST and reports the external provider as
//! syntax authority. It does not recognize source text or claim Gerbil Parser
//! IR, generated-LR, or Scheme-AOT parity.

#![forbid(unsafe_code)]

use gerbil_parser_rowan::{
    Diagnostic, EventCatalog, SyntaxKind, SyntaxNode, TreeEvent, build_rowan_events_catalog,
};
use rowan::GreenNode;
use sha2::{Digest, Sha256};

/// Immutable identity and syntax-kind catalog supplied by an external pack.
#[derive(Clone, Copy, Debug)]
pub struct ExternalLanguageSpec {
    pub language: &'static str,
    pub version: &'static str,
    pub contract: &'static str,
    pub provider: &'static str,
    pub provider_version: &'static str,
    /// Build-bound digest of the external parser artifact.
    pub provider_digest: &'static str,
    pub catalog: EventCatalog,
}

/// Source-bound receipt whose authority cannot be mistaken for Grammar IR.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExternalParseReceipt {
    pub authority: &'static str,
    pub language: &'static str,
    pub version: &'static str,
    pub contract: &'static str,
    pub provider: &'static str,
    pub provider_version: &'static str,
    pub provider_digest: &'static str,
    pub source_digest: String,
}

/// A validated external CST with its own provenance receipt.
#[derive(Clone, Debug)]
pub struct ExternalParse {
    green: GreenNode,
    spec: &'static ExternalLanguageSpec,
    receipt: ExternalParseReceipt,
}

impl ExternalParse {
    #[must_use]
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    #[must_use]
    pub fn receipt(&self) -> &ExternalParseReceipt {
        &self.receipt
    }

    #[must_use]
    pub fn kind_name(&self, kind: SyntaxKind) -> Option<&str> {
        self.spec
            .catalog
            .kinds
            .get(usize::from(kind.0))
            .map(|entry| entry.name)
    }
}

/// Typed failure carrying the same external provenance and source identity.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExternalParseError {
    pub receipt: Box<ExternalParseReceipt>,
    pub diagnostic: Diagnostic,
}

impl ExternalParseError {
    /// Wrap a rejection produced before an external parser emitted events.
    #[must_use]
    pub fn from_provider(
        spec: &ExternalLanguageSpec,
        source: &str,
        diagnostic: Diagnostic,
    ) -> Self {
        Self {
            receipt: Box::new(external_receipt(spec, source)),
            diagnostic,
        }
    }
}

#[must_use]
pub fn external_receipt(spec: &ExternalLanguageSpec, source: &str) -> ExternalParseReceipt {
    let digest = Sha256::digest(source.as_bytes());
    ExternalParseReceipt {
        authority: "external-parser",
        language: spec.language,
        version: spec.version,
        contract: spec.contract,
        provider: spec.provider,
        provider_version: spec.provider_version,
        provider_digest: spec.provider_digest,
        source_digest: format!("sha256:{digest:x}"),
    }
}

/// Validate external events and return one source-exact Rowan tree.
///
/// # Errors
///
/// Rejects incomplete provider identity, an invalid artifact digest, or an
/// event stream that fails the generic catalog and source-coverage checks.
pub fn parse_external_events(
    spec: &'static ExternalLanguageSpec,
    source: &str,
    events: &[TreeEvent],
) -> Result<ExternalParse, ExternalParseError> {
    let receipt = external_receipt(spec, source);
    let failure = |diagnostic| ExternalParseError {
        receipt: Box::new(receipt.clone()),
        diagnostic,
    };
    if [
        spec.language,
        spec.version,
        spec.contract,
        spec.provider,
        spec.provider_version,
    ]
    .iter()
    .any(|value| value.is_empty())
        || !canonical_sha256(spec.provider_digest)
    {
        return Err(failure(Diagnostic {
            reason_kind: "invalid-external-provider",
            byte_offset: 0,
            message: "external provider identity or artifact digest is invalid".into(),
        }));
    }
    let green = build_rowan_events_catalog(&spec.catalog, source, events).map_err(failure)?;
    Ok(ExternalParse {
        green,
        spec,
        receipt,
    })
}

fn canonical_sha256(digest: &str) -> bool {
    digest.len() == 71
        && digest.starts_with("sha256:")
        && digest[7..].bytes().all(|byte| byte.is_ascii_hexdigit())
}
