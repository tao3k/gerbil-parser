//! Opt-in event handoff for externally maintained parsers.

#![forbid(unsafe_code)]

mod adapter;

pub use adapter::{
    ExternalLanguageSpec, ExternalParse, ExternalParseError, ExternalParseReceipt,
    external_receipt, parse_external_events,
};
