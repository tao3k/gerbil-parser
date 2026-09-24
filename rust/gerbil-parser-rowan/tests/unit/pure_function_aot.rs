//! Compile and execute a Scheme-authored Rust function, not a text snapshot.

include!("pure_function_generated.rs");

macro_rules! check_rust_aot_function {
    ($function:path; $($input:expr => $expected:expr),+ $(,)?) => {
        $(assert_eq!($function($input), $expected, "input: {:?}", $input);)+
    };
}

#[test]
fn generated_pure_function_compiles_and_runs() {
    check_rust_aot_function!(normalized_title;
        "  Alpha  " => "Alpha",
        "\tBeta\n" => "Beta",
        "" => "",
    );
}
