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
    Diagnostic, PreparedResultProfile, ProjectedNode, ResultCapture, TreeEvent,
};
pub static SOURCES: &[&str] = &[
    "",
    "α😀",
    "'α😀'",
    "\"α${x:-中}\"",
    "$'a\\n'",
    "${arr[${i:-1}]:-λ}",
    "$(echo α)",
    "$((1+2))",
    "<(echo 中)",
    "`echo α`",
    "$x\\α",
    "name+=\"α${x:-中}\"",
    "α${x:-中}\\\n",
];
fn replay_0<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project("Word", source, 0..0, vec![ResultCapture::Parts(vec![])])
}
fn replay_1<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project(
        "Word",
        source,
        0..6,
        vec![ResultCapture::Parts(vec![profile.project(
            "LiteralPart",
            source,
            0..6,
            vec![ResultCapture::Span(0..6)],
        )?])],
    )
}
fn replay_2<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project(
        "Word",
        source,
        0..8,
        vec![ResultCapture::Parts(vec![profile.project(
            "SingleQuoted",
            source,
            0..8,
            vec![
                ResultCapture::Span(0..1),
                ResultCapture::Parts(vec![profile.project(
                    "LiteralPart",
                    source,
                    1..7,
                    vec![ResultCapture::Span(1..7)],
                )?]),
                ResultCapture::Span(7..8),
            ],
        )?])],
    )
}
fn replay_3<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project(
        "Word",
        source,
        0..13,
        vec![ResultCapture::Parts(vec![profile.project(
            "DoubleQuoted",
            source,
            0..13,
            vec![
                ResultCapture::Span(0..1),
                ResultCapture::Parts(vec![
                    profile.project(
                        "LiteralPart",
                        source,
                        1..3,
                        vec![ResultCapture::Span(1..3)],
                    )?,
                    profile.project(
                        "ParameterExpansion",
                        source,
                        3..12,
                        vec![
                            ResultCapture::Span(3..5),
                            ResultCapture::Absent,
                            ResultCapture::Span(5..6),
                            ResultCapture::Absent,
                            ResultCapture::Span(6..8),
                            ResultCapture::Parts(vec![profile.project(
                                "LiteralPart",
                                source,
                                8..11,
                                vec![ResultCapture::Span(8..11)],
                            )?]),
                            ResultCapture::Span(11..12),
                        ],
                    )?,
                ]),
                ResultCapture::Span(12..13),
            ],
        )?])],
    )
}
fn replay_4<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project(
        "Word",
        source,
        0..6,
        vec![ResultCapture::Parts(vec![profile.project(
            "AnsiCString",
            source,
            0..6,
            vec![
                ResultCapture::Span(0..2),
                ResultCapture::Parts(vec![
                    profile.project(
                        "LiteralPart",
                        source,
                        2..3,
                        vec![ResultCapture::Span(2..3)],
                    )?,
                    profile.project(
                        "EscapeSequence",
                        source,
                        3..5,
                        vec![ResultCapture::Span(3..5)],
                    )?,
                ]),
                ResultCapture::Span(5..6),
            ],
        )?])],
    )
}
fn replay_5<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project(
        "Word",
        source,
        0..19,
        vec![ResultCapture::Parts(vec![profile.project(
            "ParameterExpansion",
            source,
            0..19,
            vec![
                ResultCapture::Span(0..2),
                ResultCapture::Absent,
                ResultCapture::Span(2..5),
                ResultCapture::Node(profile.project(
                    "ArraySubscript",
                    source,
                    5..14,
                    vec![
                        ResultCapture::Span(5..6),
                        ResultCapture::Parts(vec![profile.project(
                            "ParameterExpansion",
                            source,
                            6..13,
                            vec![
                                ResultCapture::Span(6..8),
                                ResultCapture::Absent,
                                ResultCapture::Span(8..9),
                                ResultCapture::Absent,
                                ResultCapture::Span(9..11),
                                ResultCapture::Parts(vec![profile.project(
                                    "LiteralPart",
                                    source,
                                    11..12,
                                    vec![ResultCapture::Span(11..12)],
                                )?]),
                                ResultCapture::Span(12..13),
                            ],
                        )?]),
                        ResultCapture::Span(13..14),
                    ],
                )?),
                ResultCapture::Span(14..16),
                ResultCapture::Parts(vec![profile.project(
                    "LiteralPart",
                    source,
                    16..18,
                    vec![ResultCapture::Span(16..18)],
                )?]),
                ResultCapture::Span(18..19),
            ],
        )?])],
    )
}
fn replay_6<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project(
        "Word",
        source,
        0..10,
        vec![ResultCapture::Parts(vec![profile.project(
            "CommandSubstitution",
            source,
            0..10,
            vec![
                ResultCapture::Span(0..2),
                ResultCapture::Span(2..9),
                ResultCapture::Span(9..10),
            ],
        )?])],
    )
}
fn replay_7<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project(
        "Word",
        source,
        0..8,
        vec![ResultCapture::Parts(vec![profile.project(
            "ArithmeticExpansion",
            source,
            0..8,
            vec![
                ResultCapture::Span(0..3),
                ResultCapture::Span(3..6),
                ResultCapture::Span(6..8),
            ],
        )?])],
    )
}
fn replay_8<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project(
        "Word",
        source,
        0..11,
        vec![ResultCapture::Parts(vec![profile.project(
            "ProcessSubstitution",
            source,
            0..11,
            vec![
                ResultCapture::Span(0..2),
                ResultCapture::Span(2..10),
                ResultCapture::Span(10..11),
            ],
        )?])],
    )
}
fn replay_9<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project(
        "Word",
        source,
        0..9,
        vec![ResultCapture::Parts(vec![profile.project(
            "CommandSubstitution",
            source,
            0..9,
            vec![
                ResultCapture::Span(0..1),
                ResultCapture::Span(1..8),
                ResultCapture::Span(8..9),
            ],
        )?])],
    )
}
fn replay_10<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project(
        "Word",
        source,
        0..5,
        vec![ResultCapture::Parts(vec![
            profile.project(
                "SimpleParameter",
                source,
                0..2,
                vec![ResultCapture::Span(0..2)],
            )?,
            profile.project(
                "EscapeSequence",
                source,
                2..5,
                vec![ResultCapture::Span(2..5)],
            )?,
        ])],
    )
}
fn replay_11<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project(
        "Assignment",
        source,
        0..19,
        vec![
            ResultCapture::Span(0..4),
            ResultCapture::Span(4..6),
            ResultCapture::Parts(vec![profile.project(
                "DoubleQuoted",
                source,
                6..19,
                vec![
                    ResultCapture::Span(6..7),
                    ResultCapture::Parts(vec![
                        profile.project(
                            "LiteralPart",
                            source,
                            7..9,
                            vec![ResultCapture::Span(7..9)],
                        )?,
                        profile.project(
                            "ParameterExpansion",
                            source,
                            9..18,
                            vec![
                                ResultCapture::Span(9..11),
                                ResultCapture::Absent,
                                ResultCapture::Span(11..12),
                                ResultCapture::Absent,
                                ResultCapture::Span(12..14),
                                ResultCapture::Parts(vec![profile.project(
                                    "LiteralPart",
                                    source,
                                    14..17,
                                    vec![ResultCapture::Span(14..17)],
                                )?]),
                                ResultCapture::Span(17..18),
                            ],
                        )?,
                    ]),
                    ResultCapture::Span(18..19),
                ],
            )?]),
        ],
    )
}
fn replay_12<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    profile.project(
        "HereDocumentLine",
        source,
        0..13,
        vec![ResultCapture::Parts(vec![
            profile.project("LiteralPart", source, 0..2, vec![ResultCapture::Span(0..2)])?,
            profile.project(
                "ParameterExpansion",
                source,
                2..11,
                vec![
                    ResultCapture::Span(2..4),
                    ResultCapture::Absent,
                    ResultCapture::Span(4..5),
                    ResultCapture::Absent,
                    ResultCapture::Span(5..7),
                    ResultCapture::Parts(vec![profile.project(
                        "LiteralPart",
                        source,
                        7..10,
                        vec![ResultCapture::Span(7..10)],
                    )?]),
                    ResultCapture::Span(10..11),
                ],
            )?,
            profile.project(
                "EscapeSequence",
                source,
                11..13,
                vec![ResultCapture::Span(11..13)],
            )?,
        ])],
    )
}
pub fn replay<'s>(
    profile: &PreparedResultProfile,
    source: &'s str,
    index: usize,
) -> Result<ProjectedNode<'s>, Diagnostic> {
    match index {
        0 => replay_0(profile, source),
        1 => replay_1(profile, source),
        2 => replay_2(profile, source),
        3 => replay_3(profile, source),
        4 => replay_4(profile, source),
        5 => replay_5(profile, source),
        6 => replay_6(profile, source),
        7 => replay_7(profile, source),
        8 => replay_8(profile, source),
        9 => replay_9(profile, source),
        10 => replay_10(profile, source),
        11 => replay_11(profile, source),
        12 => replay_12(profile, source),
        _ => panic!("unknown generated control"),
    }
}
pub static EXPECTED: &[&[TreeEvent]] = &[
    &[TreeEvent::StartNode(25), TreeEvent::FinishNode],
    &[
        TreeEvent::StartNode(25),
        TreeEvent::StartNode(22),
        TreeEvent::Token {
            kind: 37,
            start: 0,
            end: 6,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(25),
        TreeEvent::StartNode(27),
        TreeEvent::Token {
            kind: 40,
            start: 0,
            end: 1,
        },
        TreeEvent::StartNode(22),
        TreeEvent::Token {
            kind: 37,
            start: 1,
            end: 7,
        },
        TreeEvent::FinishNode,
        TreeEvent::Token {
            kind: 41,
            start: 7,
            end: 8,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(25),
        TreeEvent::StartNode(28),
        TreeEvent::Token {
            kind: 40,
            start: 0,
            end: 1,
        },
        TreeEvent::StartNode(22),
        TreeEvent::Token {
            kind: 37,
            start: 1,
            end: 3,
        },
        TreeEvent::FinishNode,
        TreeEvent::StartNode(34),
        TreeEvent::Token {
            kind: 47,
            start: 3,
            end: 5,
        },
        TreeEvent::Token {
            kind: 49,
            start: 5,
            end: 6,
        },
        TreeEvent::Token {
            kind: 50,
            start: 6,
            end: 8,
        },
        TreeEvent::StartNode(22),
        TreeEvent::Token {
            kind: 37,
            start: 8,
            end: 11,
        },
        TreeEvent::FinishNode,
        TreeEvent::Token {
            kind: 51,
            start: 11,
            end: 12,
        },
        TreeEvent::FinishNode,
        TreeEvent::Token {
            kind: 41,
            start: 12,
            end: 13,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(25),
        TreeEvent::StartNode(29),
        TreeEvent::Token {
            kind: 40,
            start: 0,
            end: 2,
        },
        TreeEvent::StartNode(22),
        TreeEvent::Token {
            kind: 37,
            start: 2,
            end: 3,
        },
        TreeEvent::FinishNode,
        TreeEvent::StartNode(23),
        TreeEvent::Token {
            kind: 38,
            start: 3,
            end: 5,
        },
        TreeEvent::FinishNode,
        TreeEvent::Token {
            kind: 41,
            start: 5,
            end: 6,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(25),
        TreeEvent::StartNode(34),
        TreeEvent::Token {
            kind: 47,
            start: 0,
            end: 2,
        },
        TreeEvent::Token {
            kind: 49,
            start: 2,
            end: 5,
        },
        TreeEvent::StartNode(33),
        TreeEvent::Token {
            kind: 45,
            start: 5,
            end: 6,
        },
        TreeEvent::StartNode(34),
        TreeEvent::Token {
            kind: 47,
            start: 6,
            end: 8,
        },
        TreeEvent::Token {
            kind: 49,
            start: 8,
            end: 9,
        },
        TreeEvent::Token {
            kind: 50,
            start: 9,
            end: 11,
        },
        TreeEvent::StartNode(22),
        TreeEvent::Token {
            kind: 37,
            start: 11,
            end: 12,
        },
        TreeEvent::FinishNode,
        TreeEvent::Token {
            kind: 51,
            start: 12,
            end: 13,
        },
        TreeEvent::FinishNode,
        TreeEvent::Token {
            kind: 46,
            start: 13,
            end: 14,
        },
        TreeEvent::FinishNode,
        TreeEvent::Token {
            kind: 50,
            start: 14,
            end: 16,
        },
        TreeEvent::StartNode(22),
        TreeEvent::Token {
            kind: 37,
            start: 16,
            end: 18,
        },
        TreeEvent::FinishNode,
        TreeEvent::Token {
            kind: 51,
            start: 18,
            end: 19,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(25),
        TreeEvent::StartNode(31),
        TreeEvent::Token {
            kind: 42,
            start: 0,
            end: 2,
        },
        TreeEvent::Token {
            kind: 43,
            start: 2,
            end: 9,
        },
        TreeEvent::Token {
            kind: 44,
            start: 9,
            end: 10,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(25),
        TreeEvent::StartNode(30),
        TreeEvent::Token {
            kind: 42,
            start: 0,
            end: 3,
        },
        TreeEvent::Token {
            kind: 43,
            start: 3,
            end: 6,
        },
        TreeEvent::Token {
            kind: 44,
            start: 6,
            end: 8,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(25),
        TreeEvent::StartNode(32),
        TreeEvent::Token {
            kind: 42,
            start: 0,
            end: 2,
        },
        TreeEvent::Token {
            kind: 43,
            start: 2,
            end: 10,
        },
        TreeEvent::Token {
            kind: 44,
            start: 10,
            end: 11,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(25),
        TreeEvent::StartNode(31),
        TreeEvent::Token {
            kind: 42,
            start: 0,
            end: 1,
        },
        TreeEvent::Token {
            kind: 43,
            start: 1,
            end: 8,
        },
        TreeEvent::Token {
            kind: 44,
            start: 8,
            end: 9,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(25),
        TreeEvent::StartNode(24),
        TreeEvent::Token {
            kind: 39,
            start: 0,
            end: 2,
        },
        TreeEvent::FinishNode,
        TreeEvent::StartNode(23),
        TreeEvent::Token {
            kind: 38,
            start: 2,
            end: 5,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(35),
        TreeEvent::Token {
            kind: 52,
            start: 0,
            end: 4,
        },
        TreeEvent::Token {
            kind: 53,
            start: 4,
            end: 6,
        },
        TreeEvent::StartNode(28),
        TreeEvent::Token {
            kind: 40,
            start: 6,
            end: 7,
        },
        TreeEvent::StartNode(22),
        TreeEvent::Token {
            kind: 37,
            start: 7,
            end: 9,
        },
        TreeEvent::FinishNode,
        TreeEvent::StartNode(34),
        TreeEvent::Token {
            kind: 47,
            start: 9,
            end: 11,
        },
        TreeEvent::Token {
            kind: 49,
            start: 11,
            end: 12,
        },
        TreeEvent::Token {
            kind: 50,
            start: 12,
            end: 14,
        },
        TreeEvent::StartNode(22),
        TreeEvent::Token {
            kind: 37,
            start: 14,
            end: 17,
        },
        TreeEvent::FinishNode,
        TreeEvent::Token {
            kind: 51,
            start: 17,
            end: 18,
        },
        TreeEvent::FinishNode,
        TreeEvent::Token {
            kind: 41,
            start: 18,
            end: 19,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
    &[
        TreeEvent::StartNode(26),
        TreeEvent::StartNode(22),
        TreeEvent::Token {
            kind: 37,
            start: 0,
            end: 2,
        },
        TreeEvent::FinishNode,
        TreeEvent::StartNode(34),
        TreeEvent::Token {
            kind: 47,
            start: 2,
            end: 4,
        },
        TreeEvent::Token {
            kind: 49,
            start: 4,
            end: 5,
        },
        TreeEvent::Token {
            kind: 50,
            start: 5,
            end: 7,
        },
        TreeEvent::StartNode(22),
        TreeEvent::Token {
            kind: 37,
            start: 7,
            end: 10,
        },
        TreeEvent::FinishNode,
        TreeEvent::Token {
            kind: 51,
            start: 10,
            end: 11,
        },
        TreeEvent::FinishNode,
        TreeEvent::StartNode(23),
        TreeEvent::Token {
            kind: 38,
            start: 11,
            end: 13,
        },
        TreeEvent::FinishNode,
        TreeEvent::FinishNode,
    ],
];
pub static FIELDS: &[&[(&str, &str)]] = &[
    &[],
    &[("Word", "part"), ("LiteralPart", "text")],
    &[
        ("Word", "part"),
        ("SingleQuoted", "open"),
        ("SingleQuoted", "part"),
        ("LiteralPart", "text"),
        ("SingleQuoted", "close"),
    ],
    &[
        ("Word", "part"),
        ("DoubleQuoted", "open"),
        ("DoubleQuoted", "part"),
        ("LiteralPart", "text"),
        ("DoubleQuoted", "part"),
        ("ParameterExpansion", "open"),
        ("ParameterExpansion", "name"),
        ("ParameterExpansion", "operator"),
        ("ParameterExpansion", "operand"),
        ("LiteralPart", "text"),
        ("ParameterExpansion", "close"),
        ("DoubleQuoted", "close"),
    ],
    &[
        ("Word", "part"),
        ("AnsiCString", "open"),
        ("AnsiCString", "part"),
        ("LiteralPart", "text"),
        ("AnsiCString", "part"),
        ("EscapeSequence", "text"),
        ("AnsiCString", "close"),
    ],
    &[
        ("Word", "part"),
        ("ParameterExpansion", "open"),
        ("ParameterExpansion", "name"),
        ("ParameterExpansion", "subscript"),
        ("ArraySubscript", "open"),
        ("ArraySubscript", "index"),
        ("ParameterExpansion", "open"),
        ("ParameterExpansion", "name"),
        ("ParameterExpansion", "operator"),
        ("ParameterExpansion", "operand"),
        ("LiteralPart", "text"),
        ("ParameterExpansion", "close"),
        ("ArraySubscript", "close"),
        ("ParameterExpansion", "operator"),
        ("ParameterExpansion", "operand"),
        ("LiteralPart", "text"),
        ("ParameterExpansion", "close"),
    ],
    &[
        ("Word", "part"),
        ("CommandSubstitution", "open"),
        ("CommandSubstitution", "body"),
        ("CommandSubstitution", "close"),
    ],
    &[
        ("Word", "part"),
        ("ArithmeticExpansion", "open"),
        ("ArithmeticExpansion", "body"),
        ("ArithmeticExpansion", "close"),
    ],
    &[
        ("Word", "part"),
        ("ProcessSubstitution", "open"),
        ("ProcessSubstitution", "body"),
        ("ProcessSubstitution", "close"),
    ],
    &[
        ("Word", "part"),
        ("CommandSubstitution", "open"),
        ("CommandSubstitution", "body"),
        ("CommandSubstitution", "close"),
    ],
    &[
        ("Word", "part"),
        ("SimpleParameter", "text"),
        ("Word", "part"),
        ("EscapeSequence", "text"),
    ],
    &[
        ("Assignment", "name"),
        ("Assignment", "operator"),
        ("Assignment", "value"),
        ("DoubleQuoted", "open"),
        ("DoubleQuoted", "part"),
        ("LiteralPart", "text"),
        ("DoubleQuoted", "part"),
        ("ParameterExpansion", "open"),
        ("ParameterExpansion", "name"),
        ("ParameterExpansion", "operator"),
        ("ParameterExpansion", "operand"),
        ("LiteralPart", "text"),
        ("ParameterExpansion", "close"),
        ("DoubleQuoted", "close"),
    ],
    &[
        ("HereDocumentLine", "part"),
        ("LiteralPart", "text"),
        ("HereDocumentLine", "part"),
        ("ParameterExpansion", "open"),
        ("ParameterExpansion", "name"),
        ("ParameterExpansion", "operator"),
        ("ParameterExpansion", "operand"),
        ("LiteralPart", "text"),
        ("ParameterExpansion", "close"),
        ("HereDocumentLine", "part"),
        ("EscapeSequence", "text"),
    ],
];
