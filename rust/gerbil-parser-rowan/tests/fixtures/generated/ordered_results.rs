// @generated from the admitted Scheme ResultProfile IR.
use gerbil_parser_rowan::{
    CaptureKind, CaptureSpec, KindCategory, KindSpec, ProjectionInstruction, ProjectionOpcode,
    ResultNodeSpec, ResultProfileSpec, ResultProjection,
};
pub static RESULT_PROFILE: ResultProfileSpec = ResultProfileSpec {
    kinds: &[
        KindSpec {
            name: "Redirection",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "ArrayAssignment",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "SimpleCommand",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "CommandList",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "IfCommand",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "WhileCommand",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "UntilCommand",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "ForCommand",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "SelectCommand",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "ArithmeticForCommand",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "CaseCommand",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "CaseClause",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "FunctionDefinition",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "ConditionalCommand",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "ArithmeticCommand",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "RedirectedCommand",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "BraceGroup",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "Subshell",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "Pipeline",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "AndOrList",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "HereDocument",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "BashFile",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "LiteralPart",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "EscapeSequence",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "SimpleParameter",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "Word",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "HereDocumentLine",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "SingleQuoted",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "DoubleQuoted",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "AnsiCString",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "ArithmeticExpansion",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "CommandSubstitution",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "ProcessSubstitution",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "ArraySubscript",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "ParameterExpansion",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "Assignment",
            category: KindCategory::Node,
        },
        KindSpec {
            name: "unparsed-source",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "LiteralPart",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "EscapeSequence",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "SimpleParameter",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "quote-open",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "quote-close",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "substitution-open",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "substitution-body",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "substitution-close",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "subscript-open",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "subscript-close",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "parameter-open",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "parameter-prefix",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "parameter-name",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "parameter-operator",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "parameter-close",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "assignment-name",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "assignment-operator",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "horizontal-whitespace",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "newline",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "line-continuation",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "comment",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "word",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "operator",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "heredoc-marker",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "heredoc-content",
            category: KindCategory::Token,
        },
        KindSpec {
            name: "heredoc-end",
            category: KindCategory::Token,
        },
    ],
    nodes: &[
        ResultNodeSpec {
            kind: 0,
            fields: &["descriptor", "operator", "target"],
        },
        ResultNodeSpec {
            kind: 1,
            fields: &["assignment", "open", "close", "separator", "element"],
        },
        ResultNodeSpec {
            kind: 2,
            fields: &["assignment", "name", "argument", "redirect"],
        },
        ResultNodeSpec {
            kind: 3,
            fields: &["command", "separator", "here-document"],
        },
        ResultNodeSpec {
            kind: 4,
            fields: &["keyword", "condition", "body", "else-body"],
        },
        ResultNodeSpec {
            kind: 5,
            fields: &["keyword", "condition", "body"],
        },
        ResultNodeSpec {
            kind: 6,
            fields: &["keyword", "condition", "body"],
        },
        ResultNodeSpec {
            kind: 7,
            fields: &["keyword", "header", "variable", "item", "separator", "body"],
        },
        ResultNodeSpec {
            kind: 8,
            fields: &["keyword", "header", "variable", "item", "separator", "body"],
        },
        ResultNodeSpec {
            kind: 9,
            fields: &["keyword", "header", "variable", "item", "separator", "body"],
        },
        ResultNodeSpec {
            kind: 10,
            fields: &["keyword", "subject", "separator", "clause"],
        },
        ResultNodeSpec {
            kind: 11,
            fields: &[
                "open",
                "pattern",
                "alternate",
                "close",
                "body",
                "terminator",
            ],
        },
        ResultNodeSpec {
            kind: 12,
            fields: &["keyword", "name", "open", "close", "body"],
        },
        ResultNodeSpec {
            kind: 13,
            fields: &["open", "close", "operator", "operand"],
        },
        ResultNodeSpec {
            kind: 14,
            fields: &["open", "close", "expression", "operator"],
        },
        ResultNodeSpec {
            kind: 15,
            fields: &["command", "redirect"],
        },
        ResultNodeSpec {
            kind: 16,
            fields: &["open", "body", "close"],
        },
        ResultNodeSpec {
            kind: 17,
            fields: &["open", "body", "close"],
        },
        ResultNodeSpec {
            kind: 18,
            fields: &["keyword", "option", "negate", "command", "operator"],
        },
        ResultNodeSpec {
            kind: 19,
            fields: &["command", "operator"],
        },
        ResultNodeSpec {
            kind: 20,
            fields: &["delimiter", "content"],
        },
        ResultNodeSpec {
            kind: 21,
            fields: &["command", "separator", "here-document"],
        },
        ResultNodeSpec {
            kind: 22,
            fields: &["text"],
        },
        ResultNodeSpec {
            kind: 23,
            fields: &["text"],
        },
        ResultNodeSpec {
            kind: 24,
            fields: &["text"],
        },
        ResultNodeSpec {
            kind: 25,
            fields: &["part"],
        },
        ResultNodeSpec {
            kind: 26,
            fields: &["part"],
        },
        ResultNodeSpec {
            kind: 27,
            fields: &["open", "part", "close"],
        },
        ResultNodeSpec {
            kind: 28,
            fields: &["open", "part", "close"],
        },
        ResultNodeSpec {
            kind: 29,
            fields: &["open", "part", "close"],
        },
        ResultNodeSpec {
            kind: 30,
            fields: &["open", "body", "close"],
        },
        ResultNodeSpec {
            kind: 31,
            fields: &["open", "body", "close"],
        },
        ResultNodeSpec {
            kind: 32,
            fields: &["open", "body", "close"],
        },
        ResultNodeSpec {
            kind: 33,
            fields: &["open", "index", "close"],
        },
        ResultNodeSpec {
            kind: 34,
            fields: &[
                "open",
                "prefix",
                "name",
                "subscript",
                "operator",
                "operand",
                "close",
            ],
        },
        ResultNodeSpec {
            kind: 35,
            fields: &["name", "operator", "value"],
        },
    ],
    projections: &[
        ResultProjection {
            id: "LiteralPart",
            kind: 22,
            captures: &[CaptureSpec {
                name: "text",
                kind: CaptureKind::Span,
            }],
            instructions: &[ProjectionInstruction {
                field: "text",
                opcode: ProjectionOpcode::Token {
                    kind: 37,
                    required: true,
                },
            }],
        },
        ResultProjection {
            id: "EscapeSequence",
            kind: 23,
            captures: &[CaptureSpec {
                name: "text",
                kind: CaptureKind::Span,
            }],
            instructions: &[ProjectionInstruction {
                field: "text",
                opcode: ProjectionOpcode::Token {
                    kind: 38,
                    required: true,
                },
            }],
        },
        ResultProjection {
            id: "SimpleParameter",
            kind: 24,
            captures: &[CaptureSpec {
                name: "text",
                kind: CaptureKind::Span,
            }],
            instructions: &[ProjectionInstruction {
                field: "text",
                opcode: ProjectionOpcode::Token {
                    kind: 39,
                    required: true,
                },
            }],
        },
        ResultProjection {
            id: "Word",
            kind: 25,
            captures: &[CaptureSpec {
                name: "parts",
                kind: CaptureKind::Parts,
            }],
            instructions: &[ProjectionInstruction {
                field: "part",
                opcode: ProjectionOpcode::Many,
            }],
        },
        ResultProjection {
            id: "HereDocumentLine",
            kind: 26,
            captures: &[CaptureSpec {
                name: "parts",
                kind: CaptureKind::Parts,
            }],
            instructions: &[ProjectionInstruction {
                field: "part",
                opcode: ProjectionOpcode::Many,
            }],
        },
        ResultProjection {
            id: "SingleQuoted",
            kind: 27,
            captures: &[
                CaptureSpec {
                    name: "open",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "parts",
                    kind: CaptureKind::Parts,
                },
                CaptureSpec {
                    name: "close",
                    kind: CaptureKind::Span,
                },
            ],
            instructions: &[
                ProjectionInstruction {
                    field: "open",
                    opcode: ProjectionOpcode::Token {
                        kind: 40,
                        required: true,
                    },
                },
                ProjectionInstruction {
                    field: "part",
                    opcode: ProjectionOpcode::Many,
                },
                ProjectionInstruction {
                    field: "close",
                    opcode: ProjectionOpcode::Token {
                        kind: 41,
                        required: true,
                    },
                },
            ],
        },
        ResultProjection {
            id: "DoubleQuoted",
            kind: 28,
            captures: &[
                CaptureSpec {
                    name: "open",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "parts",
                    kind: CaptureKind::Parts,
                },
                CaptureSpec {
                    name: "close",
                    kind: CaptureKind::Span,
                },
            ],
            instructions: &[
                ProjectionInstruction {
                    field: "open",
                    opcode: ProjectionOpcode::Token {
                        kind: 40,
                        required: true,
                    },
                },
                ProjectionInstruction {
                    field: "part",
                    opcode: ProjectionOpcode::Many,
                },
                ProjectionInstruction {
                    field: "close",
                    opcode: ProjectionOpcode::Token {
                        kind: 41,
                        required: true,
                    },
                },
            ],
        },
        ResultProjection {
            id: "AnsiCString",
            kind: 29,
            captures: &[
                CaptureSpec {
                    name: "open",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "parts",
                    kind: CaptureKind::Parts,
                },
                CaptureSpec {
                    name: "close",
                    kind: CaptureKind::Span,
                },
            ],
            instructions: &[
                ProjectionInstruction {
                    field: "open",
                    opcode: ProjectionOpcode::Token {
                        kind: 40,
                        required: true,
                    },
                },
                ProjectionInstruction {
                    field: "part",
                    opcode: ProjectionOpcode::Many,
                },
                ProjectionInstruction {
                    field: "close",
                    opcode: ProjectionOpcode::Token {
                        kind: 41,
                        required: true,
                    },
                },
            ],
        },
        ResultProjection {
            id: "ArithmeticExpansion",
            kind: 30,
            captures: &[
                CaptureSpec {
                    name: "open",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "body",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "close",
                    kind: CaptureKind::Span,
                },
            ],
            instructions: &[
                ProjectionInstruction {
                    field: "open",
                    opcode: ProjectionOpcode::Token {
                        kind: 42,
                        required: true,
                    },
                },
                ProjectionInstruction {
                    field: "body",
                    opcode: ProjectionOpcode::Token {
                        kind: 43,
                        required: false,
                    },
                },
                ProjectionInstruction {
                    field: "close",
                    opcode: ProjectionOpcode::Token {
                        kind: 44,
                        required: true,
                    },
                },
            ],
        },
        ResultProjection {
            id: "CommandSubstitution",
            kind: 31,
            captures: &[
                CaptureSpec {
                    name: "open",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "body",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "close",
                    kind: CaptureKind::Span,
                },
            ],
            instructions: &[
                ProjectionInstruction {
                    field: "open",
                    opcode: ProjectionOpcode::Token {
                        kind: 42,
                        required: true,
                    },
                },
                ProjectionInstruction {
                    field: "body",
                    opcode: ProjectionOpcode::Token {
                        kind: 43,
                        required: false,
                    },
                },
                ProjectionInstruction {
                    field: "close",
                    opcode: ProjectionOpcode::Token {
                        kind: 44,
                        required: true,
                    },
                },
            ],
        },
        ResultProjection {
            id: "ProcessSubstitution",
            kind: 32,
            captures: &[
                CaptureSpec {
                    name: "open",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "body",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "close",
                    kind: CaptureKind::Span,
                },
            ],
            instructions: &[
                ProjectionInstruction {
                    field: "open",
                    opcode: ProjectionOpcode::Token {
                        kind: 42,
                        required: true,
                    },
                },
                ProjectionInstruction {
                    field: "body",
                    opcode: ProjectionOpcode::Token {
                        kind: 43,
                        required: false,
                    },
                },
                ProjectionInstruction {
                    field: "close",
                    opcode: ProjectionOpcode::Token {
                        kind: 44,
                        required: true,
                    },
                },
            ],
        },
        ResultProjection {
            id: "ArraySubscript",
            kind: 33,
            captures: &[
                CaptureSpec {
                    name: "open",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "parts",
                    kind: CaptureKind::Parts,
                },
                CaptureSpec {
                    name: "close",
                    kind: CaptureKind::Span,
                },
            ],
            instructions: &[
                ProjectionInstruction {
                    field: "open",
                    opcode: ProjectionOpcode::Token {
                        kind: 45,
                        required: true,
                    },
                },
                ProjectionInstruction {
                    field: "index",
                    opcode: ProjectionOpcode::Many,
                },
                ProjectionInstruction {
                    field: "close",
                    opcode: ProjectionOpcode::Token {
                        kind: 46,
                        required: true,
                    },
                },
            ],
        },
        ResultProjection {
            id: "ParameterExpansion",
            kind: 34,
            captures: &[
                CaptureSpec {
                    name: "open",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "prefix",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "name",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "subscript",
                    kind: CaptureKind::Node,
                },
                CaptureSpec {
                    name: "operator",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "parts",
                    kind: CaptureKind::Parts,
                },
                CaptureSpec {
                    name: "close",
                    kind: CaptureKind::Span,
                },
            ],
            instructions: &[
                ProjectionInstruction {
                    field: "open",
                    opcode: ProjectionOpcode::Token {
                        kind: 47,
                        required: true,
                    },
                },
                ProjectionInstruction {
                    field: "prefix",
                    opcode: ProjectionOpcode::Token {
                        kind: 48,
                        required: false,
                    },
                },
                ProjectionInstruction {
                    field: "name",
                    opcode: ProjectionOpcode::Token {
                        kind: 49,
                        required: false,
                    },
                },
                ProjectionInstruction {
                    field: "subscript",
                    opcode: ProjectionOpcode::One { required: false },
                },
                ProjectionInstruction {
                    field: "operator",
                    opcode: ProjectionOpcode::Token {
                        kind: 50,
                        required: false,
                    },
                },
                ProjectionInstruction {
                    field: "operand",
                    opcode: ProjectionOpcode::Many,
                },
                ProjectionInstruction {
                    field: "close",
                    opcode: ProjectionOpcode::Token {
                        kind: 51,
                        required: true,
                    },
                },
            ],
        },
        ResultProjection {
            id: "Assignment",
            kind: 35,
            captures: &[
                CaptureSpec {
                    name: "name",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "operator",
                    kind: CaptureKind::Span,
                },
                CaptureSpec {
                    name: "parts",
                    kind: CaptureKind::Parts,
                },
            ],
            instructions: &[
                ProjectionInstruction {
                    field: "name",
                    opcode: ProjectionOpcode::Token {
                        kind: 52,
                        required: true,
                    },
                },
                ProjectionInstruction {
                    field: "operator",
                    opcode: ProjectionOpcode::Token {
                        kind: 53,
                        required: true,
                    },
                },
                ProjectionInstruction {
                    field: "value",
                    opcode: ProjectionOpcode::Many,
                },
            ],
        },
    ],
};
use gerbil_parser_rowan::{
    Diagnostic, PreparedResultProfile, ProjectedNode, ProjectedValue, ResultChildCapture, TreeEvent,
};
pub fn replay_0<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.bind_node("SimpleCommand")?.build(
        source,
        0..3,
        vec![
            ResultChildCapture {
                field: "name",
                value: ProjectedValue::Node(profile.bind_node("Word")?.build(
                    source,
                    0..1,
                    vec![ResultChildCapture {
                        field: "part",
                        value: ProjectedValue::Token {
                            kind: 37,
                            span: 0..1,
                        },
                    }],
                )?),
            },
            ResultChildCapture {
                field: "redirect",
                value: ProjectedValue::Node(profile.bind_node("Redirection")?.build(
                    source,
                    1..2,
                    vec![ResultChildCapture {
                        field: "operator",
                        value: ProjectedValue::Token {
                            kind: 59,
                            span: 1..2,
                        },
                    }],
                )?),
            },
            ResultChildCapture {
                field: "argument",
                value: ProjectedValue::Node(profile.bind_node("Word")?.build(
                    source,
                    2..3,
                    vec![ResultChildCapture {
                        field: "part",
                        value: ProjectedValue::Token {
                            kind: 37,
                            span: 2..3,
                        },
                    }],
                )?),
            },
        ],
    )
}
pub fn replay_1<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.bind_node("Pipeline")?.build(
        source,
        0..3,
        vec![
            ResultChildCapture {
                field: "command",
                value: ProjectedValue::Node(profile.bind_node("SimpleCommand")?.build(
                    source,
                    0..1,
                    vec![ResultChildCapture {
                        field: "name",
                        value: ProjectedValue::Node(profile.bind_node("Word")?.build(
                            source,
                            0..1,
                            vec![ResultChildCapture {
                                field: "part",
                                value: ProjectedValue::Token {
                                    kind: 37,
                                    span: 0..1,
                                },
                            }],
                        )?),
                    }],
                )?),
            },
            ResultChildCapture {
                field: "operator",
                value: ProjectedValue::Token {
                    kind: 59,
                    span: 1..2,
                },
            },
            ResultChildCapture {
                field: "command",
                value: ProjectedValue::Node(profile.bind_node("SimpleCommand")?.build(
                    source,
                    2..3,
                    vec![ResultChildCapture {
                        field: "name",
                        value: ProjectedValue::Node(profile.bind_node("Word")?.build(
                            source,
                            2..3,
                            vec![ResultChildCapture {
                                field: "part",
                                value: ProjectedValue::Token {
                                    kind: 37,
                                    span: 2..3,
                                },
                            }],
                        )?),
                    }],
                )?),
            },
        ],
    )
}
pub fn replay_2<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.bind_node("BashFile")?.build(
        source,
        0..8,
        vec![ResultChildCapture {
            field: "command",
            value: ProjectedValue::Node(profile.bind_node("SimpleCommand")?.build(
                source,
                0..8,
                vec![
                    ResultChildCapture {
                        field: "argument",
                        value: ProjectedValue::Node(profile.bind_node("Word")?.build(
                            source,
                            0..2,
                            vec![ResultChildCapture {
                                field: "part",
                                value: ProjectedValue::Token {
                                    kind: 37,
                                    span: 0..2,
                                },
                            }],
                        )?),
                    },
                    ResultChildCapture {
                        field: "name",
                        value: ProjectedValue::Node(profile.bind_node("Word")?.build(
                            source,
                            2..4,
                            vec![ResultChildCapture {
                                field: "part",
                                value: ProjectedValue::Token {
                                    kind: 37,
                                    span: 2..4,
                                },
                            }],
                        )?),
                    },
                    ResultChildCapture {
                        field: "argument",
                        value: ProjectedValue::Node(profile.bind_node("Word")?.build(
                            source,
                            4..8,
                            vec![ResultChildCapture {
                                field: "part",
                                value: ProjectedValue::Token {
                                    kind: 37,
                                    span: 4..8,
                                },
                            }],
                        )?),
                    },
                ],
            )?),
        }],
    )
}
pub fn replay_3<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.bind_node("BashFile")?.build(source, 0..0, vec![])
}
pub static SOURCES: &[&str] = &["abc", "a|b", "αβ😀", ""];
pub static EVENTS: &[&[TreeEvent]] = &[
    &[
        TreeEvent::StartNode(2),
        TreeEvent::StartNode(25),
        TreeEvent::Token {
            kind: 37,
            start: 0,
            end: 1,
        },
        TreeEvent::FinishNode,
        TreeEvent::StartNode(0),
        TreeEvent::Token {
            kind: 59,
            start: 1,
            end: 2,
        },
        TreeEvent::FinishNode,
        TreeEvent::StartNode(25),
        TreeEvent::Token {
            kind: 37,
            start: 2,
            end: 3,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(18),
        TreeEvent::StartNode(2),
        TreeEvent::StartNode(25),
        TreeEvent::Token {
            kind: 37,
            start: 0,
            end: 1,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
        TreeEvent::Token {
            kind: 59,
            start: 1,
            end: 2,
        },
        TreeEvent::StartNode(2),
        TreeEvent::StartNode(25),
        TreeEvent::Token {
            kind: 37,
            start: 2,
            end: 3,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(21),
        TreeEvent::StartNode(2),
        TreeEvent::StartNode(25),
        TreeEvent::Token {
            kind: 37,
            start: 0,
            end: 2,
        },
        TreeEvent::FinishNode,
        TreeEvent::StartNode(25),
        TreeEvent::Token {
            kind: 37,
            start: 2,
            end: 4,
        },
        TreeEvent::FinishNode,
        TreeEvent::StartNode(25),
        TreeEvent::Token {
            kind: 37,
            start: 4,
            end: 8,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[TreeEvent::StartNode(21), TreeEvent::FinishNode],
];
