//! Reviewable Scheme-owned golden data; no parser or fixture generation in Rust.
pub(crate) fn bytes() -> &'static [u8] {
    static BYTES: std::sync::OnceLock<Vec<u8>> = std::sync::OnceLock::new();
    BYTES.get_or_init(|| {
        let cases: serde_json::Value =
            serde_json::from_str(include_str!("../fixtures/languages.json")).unwrap();
        let mut bytes = Vec::new();
        for case in cases.as_array().unwrap() {
            let hex = case["payload_hex"].as_str().unwrap();
            assert_eq!(hex.len() % 2, 0);
            let payload: Vec<u8> = (0..hex.len())
                .step_by(2)
                .map(|at| u8::from_str_radix(&hex[at..at + 2], 16).unwrap())
                .collect();
            for field in [
                case["name"].as_str().unwrap().as_bytes(),
                case["descriptor"].as_str().unwrap().as_bytes(),
                case["source"].as_str().unwrap().as_bytes(),
                payload.as_slice(),
            ] {
                bytes.extend_from_slice(&u32::try_from(field.len()).unwrap().to_le_bytes());
                bytes.extend_from_slice(field);
            }
            bytes.push(u8::from(case["accepted"].as_bool().unwrap()));
        }
        bytes
    })
}
