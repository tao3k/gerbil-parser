//! Complete Org Fold -> canonical OEV1, not AST projection or Tokio queueing.
use gerbil_parser_runtime::TreeEvent;
use sha2::{Digest, Sha256};
use std::{fs, time::Instant};
include!(env!("ORG_FOLD_RUST_SOURCE"));

fn main() {
    let args: Vec<_> = std::env::args().skip(1).collect();
    let count: usize = args[0].parse().unwrap();
    assert!([1_000, 10_000, 100_000].contains(&count));
    assert_eq!(args[1], "20");
    let fixtures = &args[2];
    let grammar = fs::read_to_string(&args[3]).unwrap();
    let header = format!("OEV1{grammar}").into_bytes();
    let bases: Vec<_> = ["doc.org", "plain-links.org", "quote-heavy.org"]
        .iter()
        .map(|name| fs::read_to_string(format!("{fixtures}/{name}")).unwrap())
        .collect();
    for batch in 1..=20 {
        let inputs: Vec<_> = (count * batch..count * (batch + 1))
            .map(|index| format!("{}\nchanging request {index} α中🦀\n", bases[index % 3]))
            .collect();
        let mut corpus = Sha256::new();
        for input in &inputs {
            corpus.update(input.as_bytes());
        }
        let corpus = format!("{:x}", corpus.finalize());
        let reserved: usize = inputs.capacity() * std::mem::size_of::<String>()
            + inputs.iter().map(String::capacity).sum::<usize>();
        let mut latencies = Vec::with_capacity(count);
        let mut output = Sha256::new();
        let mut output_bytes = 0usize;
        let mut max_output_capacity = 0usize;
        drop(parse_org_fold(&inputs[0]));
        let begin = Instant::now();
        for input in &inputs {
            let started = Instant::now();
            let events = parse_org_fold(input);
            let mut tape = header.clone();
            for event in &events {
                match event {
                    TreeEvent::StartNode(kind) => {
                        tape.push(1);
                        tape.extend_from_slice(&kind.to_le_bytes());
                    }
                    TreeEvent::FinishNode => tape.push(0),
                    TreeEvent::Token { kind, start, end } => {
                        tape.push(2);
                        tape.extend_from_slice(&kind.to_le_bytes());
                        tape.extend_from_slice(&u64::try_from(*start).unwrap().to_le_bytes());
                        tape.extend_from_slice(&u64::try_from(*end).unwrap().to_le_bytes());
                    }
                }
            }
            latencies.push(started.elapsed().as_secs_f64() * 1e6);
            max_output_capacity = max_output_capacity
                .max(tape.capacity() + events.capacity() * std::mem::size_of::<TreeEvent>());
            output_bytes += tape.len();
            output.update(&tape);
        }
        let wall = begin.elapsed().as_secs_f64();
        latencies.sort_by(f64::total_cmp);
        println!(
            "(backend-baseline.v1 (backend . rust-aot) (workload . real-org-fold) (requests . {count}) (batch . {batch}) (grammar . {grammar:?}) (corpus-sha256 . {corpus:?}) (input-bytes . {}) (input-reserved-bytes . {reserved}) (wall-s . {wall}) (throughput-requests/s . {}) (p95-us . {}) (p99-us . {}) (output-sha256 . {:?}) (output-bytes . {output_bytes}) (max-output-reserved-bytes . {max_output_capacity}) (boundary . fold-to-oev1-with-stream-validation) (concurrency . 1) (smp . not-exercised))",
            inputs.iter().map(String::len).sum::<usize>(),
            count as f64 / wall,
            latencies[(count * 95).div_ceil(100) - 1],
            latencies[(count * 99).div_ceil(100) - 1],
            format!("{:x}", output.finalize())
        );
    }
    println!("BACKEND-BASELINE-COMPLETE");
}
