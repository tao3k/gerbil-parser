//! Linked into the Scheme native probe, with all checks owned by its test module.
#[path = "../tests/unit/probe.rs"]
mod probe;
#[path = "../tests/unit/source_probe.rs"]
mod source_probe;

#[cfg(feature = "tokio")]
#[path = "../tests/unit/tokio_cpu.rs"]
mod tokio_cpu;
