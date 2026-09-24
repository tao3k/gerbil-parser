//! Compile and execute a Scheme-authored Rust function, not a text snapshot.

include!("pure_function_generated.rs");
include!("classify_first_word_generated.rs");
include!("classify_or_unknown_generated.rs");
include!("owned_first_word_generated.rs");
include!("owned_rest_after_first_word_generated.rs");

macro_rules! check_rust_aot_function {
    ($function:path; $(($($input:expr),+ $(,)?) => $expected:expr),+ $(,)?) => {
        $(assert_eq!($function($($input),+), $expected);)+
    };
}

#[test]
fn generated_pure_function_compiles_and_runs() {
    check_rust_aot_function!(normalized_title;
        ("  Alpha  ") => "Alpha",
        ("\tBeta\n") => "Beta",
        ("") => "",
    );
    check_rust_aot_function!(classify_first_word;
        (" WAIT\tTask", &["WAIT"], &["DONE"]) => "active",
        ("DONE Task", &["WAIT"], &["DONE"]) => "complete",
        ("TODO Task", &["WAIT"], &["DONE"]) => "",
        ("WAIT Task", &[], &["WAIT"]) => "complete",
    );
    check_rust_aot_function!(classify_or_unknown;
        (" WAIT Task", &["WAIT"], &["DONE"]) => "active",
        ("DONE Task", &["WAIT"], &["DONE"]) => "complete",
        ("LATER Task", &["WAIT"], &["DONE"]) => "unknown",
    );
    check_rust_aot_function!(owned_first_word;
        ("  WAIT Task", true) => "WAIT",
        ("  WAIT Task", false) => "",
        ("é heading", true) => "é",
    );
    check_rust_aot_function!(owned_rest_after_first_word;
        ("  WAIT   [#A] Head :tag:  ") => "[#A] Head :tag:",
        ("WAIT") => "",
        ("é  heading") => "heading",
    );
}
