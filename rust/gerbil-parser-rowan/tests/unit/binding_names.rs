use crate::{PreparedTextProfile, TextClass, TextProfile};
#[path = "../fixtures/generated/binding_names.rs"]
mod generated;
#[test]
fn binding_names_match_production_scheme_at_every_body_boundary() {
    let prepared: Vec<_> = generated::PROFILES
        .iter()
        .map(|(_, p)| PreparedTextProfile::new(p).unwrap())
        .collect();
    for &(index, source, start, limit, expected) in generated::TRACES {
        assert_eq!(
            prepared[index].match_prefix(source, start, limit),
            expected,
            "profile={} source={source:?} span={start}..{limit}",
            generated::PROFILES[index].0
        );
    }
}
#[test]
fn binding_names_reject_nullable_profiles_and_invalid_byte_spans() {
    static NULLABLE: TextProfile = TextProfile::Run {
        class: TextClass::Alphabetic,
        minimum: 0,
        maximum: None,
    };
    static NAME: TextProfile = TextProfile::Run {
        class: TextClass::Alphabetic,
        minimum: 1,
        maximum: None,
    };
    assert!(PreparedTextProfile::new(&NULLABLE).is_none());
    let profile = PreparedTextProfile::new(&NAME).unwrap();
    for (start, limit) in [(1, 2), (0, 1), (3, 2), (0, 4)] {
        assert_eq!(profile.match_prefix("αx", start, limit), None);
    }
    assert_eq!(profile.match_prefix("αx", 0, 2), Some(2));
}
