//! Scheme-owned recognition controls for the shared region execution engine.
#[path = "../fixtures/generated/region_boundaries.rs"]
mod generated;
use super::{PreparedRegionPlan, PreparedRegionSource, RegionScope, RegionSpec};

#[test]
fn regions_match_scheme_production_and_scoped_unicode_controls() {
    for (spec, scopes, traces) in [
        (
            &generated::bash::REGION,
            generated::bash::SCOPES,
            generated::bash::TRACES,
        ),
        (
            &generated::generic::REGION,
            generated::generic::SCOPES,
            generated::generic::TRACES,
        ),
    ] {
        for &(source, requests) in traces {
            let mut prepared =
                PreparedRegionSource::new(spec, source, scopes).expect("declared regions");
            for &(operation, at, expected) in requests {
                let result = match operation {
                    0 => PreparedRegionSource::new(spec, source, &[])
                        .unwrap()
                        .word_end(at),
                    1 => prepared.pair_end(at).map(Some),
                    code => prepared
                        .quote_end(at, char::from_u32(code).unwrap())
                        .map(Some),
                };
                let actual = match result {
                    Err(_) => -1,
                    Ok(None) => -2,
                    Ok(Some(end)) => i64::try_from(end).unwrap(),
                };
                assert_eq!(actual, expected, "{source:?} operation={operation} at={at}");
            }
        }
    }
}
#[test]
fn admission_offsets_and_lazy_source_are_bounded() {
    static UNKNOWN: &[RegionScope] = &[RegionScope {
        prefix: "unknown",
        pairs: &[],
    }];
    static DUPLICATE: &[RegionScope] = &[
        RegionScope {
            prefix: "${",
            pairs: &[],
        },
        RegionScope {
            prefix: "${",
            pairs: &[],
        },
    ];
    static INVALID: RegionSpec = RegionSpec {
        stops: &[""],
        ..generated::bash::REGION
    };
    assert!(PreparedRegionSource::new(&generated::bash::REGION, "", UNKNOWN).is_err());
    assert!(PreparedRegionSource::new(&generated::bash::REGION, "", DUPLICATE).is_err());
    assert!(PreparedRegionSource::new(&INVALID, "", &[]).is_err());
    let mut source = PreparedRegionSource::new(&generated::bash::REGION, "α'raw", &[]).unwrap();
    // Unrequested unterminated regions remain inert at preparation.
    assert!(source.word_end(1).is_err());
    assert!(source.pair_end(usize::MAX).is_err());
    assert!(source.quote_end(2, '"').is_err());
    assert!(source.quote_end(2, '\'').is_err());
}
#[test]
fn nested_frames_populate_cache_without_suffix_rescans() {
    let text = "${x:-${y:-\"中\"}}";
    let mut source = PreparedRegionSource::new(&generated::bash::REGION, text, &[]).unwrap();
    assert_eq!(source.pair_end(0).unwrap(), text.len());
    let pairs = source.ends.pairs.len();
    let quotes = source.ends.quotes.len();
    assert_eq!((pairs, quotes), (2, 1));
    let visits = source.ends.visits;
    for _ in 0..100 {
        assert_eq!(source.pair_end(5).unwrap(), text.len() - 1);
        assert_eq!(source.quote_end(10, '"').unwrap(), text.len() - 2);
    }
    assert_eq!(
        source.ends.visits, visits,
        "cached queries do not revisit source characters"
    );
    assert_eq!(
        (source.ends.pairs.len(), source.ends.quotes.len()),
        (pairs, quotes)
    );
}

#[test]
fn admitted_plan_reuses_tables_with_independent_source_caches() {
    let plan = PreparedRegionPlan::new(&generated::bash::REGION, &[]).unwrap();
    let text = format!("{}中{}", "${".repeat(512), "}".repeat(512));
    let mut deep = plan.source(&text);
    let mut unrelated = plan.source("${短}");
    assert_eq!(deep.pair_end(0).unwrap(), text.len());
    assert_eq!(deep.ends.pairs.len(), 512);
    assert!(deep.ends.visits <= text.chars().count());
    assert!(unrelated.ends.pairs.is_empty());
    assert_eq!(unrelated.pair_end(0).unwrap(), "${短}".len());
    let visits = deep.ends.visits;
    for level in 0..512 {
        assert_eq!(deep.pair_end(level * 2).unwrap(), text.len() - level);
    }
    assert_eq!(deep.ends.visits, visits);
}
