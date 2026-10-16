//! Serial arithmetic engine baseline, not Org/FFI/Tokio/SMP qualification.
use std::hint::black_box;
use std::time::Instant;

use sha2::{Digest, Sha256};

#[path = "../../../build-support/gerbil-parser-build-support/tests/fixtures/generated/arithmetic.rs"]
mod arithmetic;

fn percentile(sorted: &[f64], percent: usize) -> f64 {
    sorted[(sorted.len() * percent).div_ceil(100) - 1]
}

fn main() {
    let args: Vec<_> = std::env::args()
        .skip(1)
        .filter(|arg| arg != "--bench")
        .collect();
    assert_eq!(args.len(), 2, "expected requests and batches");
    let count: usize = args[0].parse().unwrap();
    let batches: usize = args[1].parse().unwrap();
    assert!([1_000, 10_000, 100_000].contains(&count));
    assert_eq!(batches, 20, "baseline requires twenty full batches");
    for batch in 1..=batches {
        let inputs: Vec<_> = (count * batch..count * (batch + 1))
            .map(|index| format!("alpha + {index} * (beta - 3)\n"))
            .collect();
        let corpus = inputs.concat();
        let digest = format!("{:x}", Sha256::digest(corpus.as_bytes()));
        let reserved: usize = inputs.iter().map(String::capacity).sum::<usize>()
            + inputs.capacity() * std::mem::size_of::<String>();
        let warm = gerbil_parser_runtime::parse(&arithmetic::LANGUAGE, &inputs[0]).unwrap();
        assert_eq!(warm.syntax().to_string(), inputs[0]);
        drop(warm);
        let mut latencies = Vec::with_capacity(count);
        let started = Instant::now();
        for source in &inputs {
            let request = Instant::now();
            let parsed = gerbil_parser_runtime::parse(&arithmetic::LANGUAGE, black_box(source))
                .expect("changing source must parse");
            let actual = parsed.syntax().to_string();
            latencies.push(request.elapsed().as_secs_f64() * 1_000_000.0);
            assert_eq!(&actual, source);
        }
        let wall = started.elapsed().as_secs_f64();
        latencies.sort_by(f64::total_cmp);
        println!(
            "(backend-baseline.v1 (backend . rust-aot) (requests . {count}) (batch . {batch}) \
             (grammar . {:?}) (corpus-sha256 . {:?}) (input-bytes . {}) \
             (input-reserved-bytes . {reserved}) (wall-s . {wall}) \
             (throughput-requests/s . {}) (p95-us . {}) (p99-us . {}) \
             (unresolved . {}) (clock . rust-instant-monotonic) \
             (boundary . parse-and-lossless-roundtrip) (concurrency . 1) (smp . not-exercised))",
            arithmetic::LANGUAGE.grammar_digest,
            digest,
            corpus.len(),
            f64::from(u32::try_from(count).unwrap()) / wall,
            percentile(&latencies, 95),
            percentile(&latencies, 99),
            latencies.iter().filter(|value| **value == 0.0).count()
        );
    }
    println!("BACKEND-BASELINE-COMPLETE");
}
