use super::{
    GlrContext, LanguageSpec, ParserAction, ParserConfiguration, Token, Value, explore_fork,
};
use crate::engine::selective_glr_tests::{BASE_LANGUAGE, DISTINCT_PRODUCTIONS};

#[test]
fn overflow_is_fatal_even_after_a_branch_completes() {
    for (score, first, second) in [(i64::MAX - 1, 1, 2), (i64::MIN + 1, -1, -2)] {
        let mut productions = DISTINCT_PRODUCTIONS.to_vec();
        productions[3].dynamic_precedence = first;
        productions[4].dynamic_precedence = second;
        let language = LanguageSpec {
            productions: Box::leak(productions.into_boxed_slice()),
            ..BASE_LANGUAGE
        };
        let configuration = ParserConfiguration {
            states: vec![0, 1],
            values: vec![Value::Token(0)],
            cursor: 1,
            score,
        };
        let tokens = [Token {
            terminal: "identifier",
            syntax_kind: 2,
            text: "x",
            start: 0,
            end: 1,
        }];
        for branches in [
            [ParserAction::Reduce(3), ParserAction::Reduce(4)],
            [ParserAction::Reduce(4), ParserAction::Reduce(3)],
        ] {
            let error = explore_fork(
                &language,
                &tokens,
                &[0],
                &configuration,
                &branches,
                0,
                &mut GlrContext::default(),
            )
            .expect_err("a capacity failure cannot discard a competing branch");
            assert_eq!(error.reason_kind, "dynamic-precedence-overflow");
        }
    }
}
