//! Matched complete requests: shared Tokio/FFI, independent old engine kernels.
use gerbil_parser_ffi::{RawResult, RuntimeApi, TokioParser};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

unsafe extern "C" {
    fn records_language_create() -> u64;
    fn records_engine_control_create() -> u64;
    fn gerbil_parser_language_parse(
        handle: u64,
        source: *const u8,
        length: usize,
        result: *mut RawResult,
    ) -> i32;
    fn gerbil_parser_language_release(handle: u64) -> i32;
    fn gerbil_parser_benchmark_cpu_seconds() -> f64;
    fn gerbil_parser_benchmark_prepare_thread() -> i32;
}
static CONTROL: AtomicU64 = AtomicU64::new(0);
static REFERENCE: AtomicBool = AtomicBool::new(false);

unsafe extern "C" fn create_pair() -> u64 {
    // SAFETY: test-only scheduling setup acts on this current OS thread.
    if unsafe { gerbil_parser_benchmark_prepare_thread() } != 0 {
        return 0;
    }
    // SAFETY: TokioParser initializes and calls the factory on the unique owner.
    let candidate = unsafe { records_language_create() };
    if candidate == 0 {
        return 0;
    }
    // SAFETY: the independent control belongs to this same initialized bundle.
    let control = unsafe { records_engine_control_create() };
    if control == 0 {
        // SAFETY: release the owned candidate after a failed paired admission.
        unsafe { gerbil_parser_language_release(candidate) };
        return 0;
    }
    CONTROL.store(control, Ordering::Relaxed);
    candidate
}
unsafe extern "C" fn parse_pair(
    handle: u64,
    source: *const u8,
    length: usize,
    result: *mut RawResult,
) -> i32 {
    let selected = if REFERENCE.load(Ordering::Acquire) {
        CONTROL.load(Ordering::Relaxed)
    } else {
        handle
    };
    // SAFETY: the admitted owner passes exclusive result storage and live bytes.
    unsafe { gerbil_parser_language_parse(selected, source, length, result) }
}
unsafe extern "C" fn release_pair(handle: u64) -> i32 {
    // SAFETY: both unique handles have no surviving C result borrowers.
    let control = unsafe { gerbil_parser_language_release(CONTROL.swap(0, Ordering::Relaxed)) };
    // SAFETY: this is the original handle created by this paired factory.
    let candidate = unsafe { gerbil_parser_language_release(handle) };
    if control == 0 { candidate } else { control }
}
fn cpu() -> f64 {
    // SAFETY: the C helper reads process statistics without entering Scheme.
    let value = unsafe { gerbil_parser_benchmark_cpu_seconds() };
    assert!(value >= 0.0, "process CPU measurement failed");
    value
}
async fn parity(parser: &TokioParser, source: &str) {
    REFERENCE.store(true, Ordering::Release);
    let reference = parser.parse(source.into()).await.unwrap();
    REFERENCE.store(false, Ordering::Release);
    let candidate = parser.parse(source.into()).await.unwrap();
    assert_eq!(reference.bytes(), candidate.bytes(), "complete GPA1 parity");
    assert_eq!(
        reference.view().unwrap().accepted(),
        candidate.view().unwrap().accepted()
    );
}
async fn batch(
    parser: &TokioParser,
    source: &str,
    expected: &[u8],
    reference: bool,
    calls: usize,
) -> f64 {
    REFERENCE.store(reference, Ordering::Release);
    let before = cpu();
    assert_eq!(calls % 8, 0);
    let mut clients = tokio::task::JoinSet::new();
    for _ in 0..8 {
        let parser = parser.clone();
        let source = source.to_owned();
        let expected = expected.to_owned();
        clients.spawn(async move {
            for _ in 0..calls / 8 {
                let parsed = parser.parse(source.clone()).await.unwrap();
                assert_eq!(parsed.bytes(), expected);
                let view = if reference {
                    parsed.reference_view()
                } else {
                    parsed.view()
                }
                .unwrap();
                assert!(view.accepted());
                assert_eq!(view.events().len(), 642);
                assert_eq!(view.root().unwrap().children().count(), 64);
                std::hint::black_box(view);
            }
        });
    }
    while let Some(result) = clients.join_next().await {
        result.unwrap();
    }
    let elapsed = cpu() - before;
    assert!(elapsed >= 0.001, "unresolved CPU batch: {elapsed}");
    elapsed
}
fn percentiles(values: &[f64]) -> [f64; 3] {
    let mut ordered = values.to_vec();
    ordered.sort_by(f64::total_cmp);
    // Twenty groups: exact nearest-rank P10/P50/P90, no discarded groups.
    [ordered[1], ordered[9], ordered[17]]
}
async fn phase(
    parser: &TokioParser,
    source: &str,
    expected: &[u8],
    name: &str,
    right_reference: bool,
) -> [f64; 3] {
    let mut ratios = Vec::new();
    for group in 0..20 {
        // Same eight-leg, four-batch-per-side protocol as Scheme Benchmark.
        let order = if group % 2 == 0 {
            [false, true, true, false, true, false, false, true]
        } else {
            [true, false, false, true, false, true, true, false]
        };
        let mut left = 0.0;
        let mut right = 0.0;
        let mut costs = [0.0; 8];
        for (leg, is_right) in order.into_iter().enumerate() {
            let reference = !is_right || right_reference;
            let cost = batch(parser, source, expected, reference, 256).await;
            if is_right {
                right += cost;
            } else {
                left += cost;
            }
            costs[leg] = cost;
        }
        println!(
            "RUST-TOKIO-CPU-GROUP phase={name} group={group} cpu={costs:?} ratio={:.9}",
            right / left
        );
        ratios.push(right / left);
    }
    percentiles(&ratios)
}
async fn qualify(unicode: bool) -> bool {
    let mut api = RuntimeApi::linked(create_pair);
    api.language.parse = parse_pair;
    api.language.release = release_pair;
    // SAFETY: this independent host process owns one permanently loaded bundle.
    let parser = unsafe { TokioParser::start(api, 4).await }.unwrap();
    for source in ["", "a=1\n", "α=1\r\n", "name=\"猫\"\n", "a=\n", "?"] {
        parity(&parser, source).await;
    }
    let source = if unicode { "α=1\n" } else { "a=1\n" }.repeat(64);
    parity(&parser, &source).await;
    let expected = parser.parse(source.clone()).await.unwrap().bytes().to_vec();
    assert_eq!(expected.len(), 15488);
    // Identical complete requests, actor, catalog, decoder and row writer.
    println!(
        "RUST-TOKIO-CPU-METHOD records=64 events=642 payload=15488 profile=release groups-per-phase=20 calls=256 legs=8 phases=3 queue=4 owners=1 clients=8 workload=closed-loop candidate=fused-admission control=former-engine+two-pass-admission cpu=process-user+system gc=included source-bytes={} unicode={unicode}",
        source.len()
    );
    // Fixed symmetric warmup for the one-shot VM, including normal GC cycles.
    for (leg, reference) in [true, false, false, true, false, true, true, false]
        .into_iter()
        .enumerate()
    {
        batch(&parser, &source, &expected, reference, 256).await;
        println!("RUST-TOKIO-CPU-WARMUP leg={leg} calls=256 reference={reference}");
    }
    let before = phase(&parser, &source, &expected, "before", true).await;
    let production = phase(&parser, &source, &expected, "production", false).await;
    let after = phase(&parser, &source, &expected, "after", true).await;
    println!(
        "RUST-TOKIO-CPU-SUMMARY unicode={unicode} before={before:?} production={production:?} after={after:?}"
    );
    parser.shutdown().await.unwrap();
    let admitted = before[0] >= 0.90
        && before[2] <= 1.10
        && after[0] >= 0.90
        && after[2] <= 1.10
        && production[2] < 0.95;
    println!(
        "RUST-TOKIO-CPU-{} unicode={unicode}",
        if admitted { "PROOF-OK" } else { "REJECTED" }
    );
    admitted
}

/// Independent process entry; reports rejection through both status and marker.
#[unsafe(no_mangle)]
pub extern "C" fn gerbil_parser_rust_tokio_cpu_probe(unicode: i32) -> i32 {
    let result = std::panic::catch_unwind(|| {
        // SAFETY: test-only scheduling setup acts on this current OS thread.
        assert_eq!(unsafe { gerbil_parser_benchmark_prepare_thread() }, 0);
        tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
            .block_on(qualify(unicode != 0))
    });
    if matches!(result, Ok(true)) { 0 } else { -1 }
}
