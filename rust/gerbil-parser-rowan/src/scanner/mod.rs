//! Shared contextual scanner API; execution belongs to the contextual leaf.
mod contextual;

pub(crate) use contextual::error;
pub use contextual::{
    BalancedPair, ContextualScanner, MarkerPolicy, RegionPair, RegionQuote,
    SCANNER_OPCODE_CONTRACT, ScannerAction, ScannerCell, ScannerCheckpoint, ScannerMatcher,
    ScannerRule, ScannerSpec,
};

#[cfg(test)]
#[path = "../../tests/unit/shared_scanner.rs"]
mod shared_scanner_tests;

#[cfg(test)]
#[path = "../../tests/unit/contextual_scanner.rs"]
mod contextual_scanner_tests;
