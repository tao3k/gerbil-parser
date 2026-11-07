use super::{Operand, OperandAction, Production, Reduction, Terminal, Token, Value, reduce};
use crate::engine::model::{Child, Symbol};

#[test]
fn nullable_aliases_use_the_following_operand_boundary() {
    let production = Production {
        lhs: "source-file",
        rhs: &[
            Operand {
                symbol: Symbol::Nonterminal("empty-before"),
                actions: &[OperandAction::Alias(1)],
            },
            Operand {
                symbol: Symbol::Terminal(Terminal::Token("word")),
                actions: &[],
            },
            Operand {
                symbol: Symbol::Nonterminal("empty-after"),
                actions: &[OperandAction::Alias(2)],
            },
        ],
        reduction: Reduction::Concat,
        dynamic_precedence: 0,
    };
    for offset in [0, 17, 3] {
        let end = offset + 1;
        let tokens = [Token {
            terminal: "word",
            syntax_kind: 0,
            text: "a",
            start: offset,
            end,
        }];
        let values = vec![
            Value::Fragment(vec![]),
            Value::Token(0),
            Value::Fragment(vec![]),
        ];
        let expected = Value::Fragment(vec![
            Child {
                field: None,
                value: Value::Node {
                    kind: 1,
                    start: offset,
                    end: offset,
                    children: vec![],
                },
            },
            Child {
                field: None,
                value: Value::Token(0),
            },
            Child {
                field: None,
                value: Value::Node {
                    kind: 2,
                    start: end,
                    end,
                    children: vec![],
                },
            },
        ]);
        assert_eq!(
            reduce(&production, values.into_iter(), &tokens, end),
            expected
        );
    }
}
