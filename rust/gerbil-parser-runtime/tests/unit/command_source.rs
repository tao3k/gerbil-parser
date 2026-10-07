//! Full Source recognition controls are produced by the independent Scheme engine.
#[path = "../fixtures/generated/command_source.rs"]
mod generated;
use crate::scanner::ContextualScanner;
use crate::{
    CommandForm, CommandInstruction, CommandProgramSpec, CommandTrigger, EventCatalog,
    PreparedCommandProgram, PreparedPartProfile, ProjectedNode, ProjectedValue,
    build_syntax_events_catalog,
};
fn fields(root: &ProjectedNode<'_>) -> Vec<&'static str> {
    let mut out = Vec::new();
    let mut stack: Vec<_> = root.children().iter().rev().collect();
    while let Some(child) = stack.pop() {
        out.push(child.field());
        if let ProjectedValue::Node(node) = child.value() {
            stack.extend(node.children().iter().rev());
        }
    }
    out
}
#[test]
fn source_matches_scheme_events_fields_fifo_links_and_rejections() {
    assert_eq!(generated::bash::CONTROLS.len(), 49);
    assert!(generated::bash::regions::SCOPES.is_empty());
    assert!(generated::extended::regions::SCOPES.is_empty());
    for (commands, parts, results, regions, scanner, controls) in [
        (
            &generated::bash::COMMAND_PROFILE,
            &generated::bash::PART_PROFILE,
            &generated::bash::RESULT_PROFILE,
            &generated::bash::regions::REGION,
            &generated::bash::scanner::SCANNER,
            generated::bash::CONTROLS,
        ),
        (
            &generated::extended::COMMAND_PROFILE,
            &generated::extended::PART_PROFILE,
            &generated::extended::RESULT_PROFILE,
            &generated::extended::regions::REGION,
            &generated::extended::scanner::SCANNER,
            generated::extended::CONTROLS,
        ),
    ] {
        let program = PreparedCommandProgram::new(commands, results).unwrap();
        let parts = PreparedPartProfile::new(parts, results, regions).unwrap();
        for control in controls {
            let outcome = ContextualScanner::new(scanner, control.source)
                .unwrap()
                .scan("source")
                .and_then(|tokens| program.parse_scanned(control.source, &tokens, &parts));
            assert_eq!(
                outcome.is_ok(),
                control.accepted,
                "{:?}: {:?}",
                control.source,
                outcome.as_ref().err()
            );
            if let Ok(parse) = outcome {
                let events = parse
                    .events()
                    .unwrap_or_else(|e| panic!("{:?}: {e:?}", control.source));
                assert_eq!(events, control.events, "events {:?}", control.source);
                assert_eq!(
                    fields(&parse.root),
                    control.fields,
                    "fields {:?}",
                    control.source
                );
                assert_eq!(
                    parse.here_documents, control.links,
                    "links {:?}",
                    control.source
                );
                let tree = build_syntax_events_catalog(
                    &EventCatalog {
                        kinds: results.kinds,
                        root_kind: parse.root.kind(),
                    },
                    control.source,
                    &events,
                )
                .unwrap();
                assert_eq!(tree.root().text().to_string(), control.source);
            }
        }
    }
}
fn changed_form(mut form: CommandForm) -> &'static CommandProgramSpec {
    let mut forms = generated::bash::COMMAND_PROFILE.forms.to_vec();
    let index = forms.iter().position(|f| f.id == form.id).unwrap_or(0);
    if index == 0 {
        form.id = forms[0].id;
    }
    forms[index] = form;
    Box::leak(Box::new(CommandProgramSpec {
        forms: Box::leak(forms.into_boxed_slice()),
        ..generated::bash::COMMAND_PROFILE
    }))
}
#[test]
fn command_admission_rejects_cycles_nullable_loops_foreign_fields_and_selectors() {
    let base = generated::bash::COMMAND_PROFILE.forms[0];
    let mut nested: &'static [CommandInstruction] = &[CommandInstruction::Raw("keyword")];
    for _ in 0..129 {
        nested = Box::leak(
            vec![CommandInstruction::Node {
                field: "command",
                kind: "Pipeline",
                program: nested,
            }]
            .into_boxed_slice(),
        );
    }
    for spec in [
        changed_form(CommandForm {
            program: &[CommandInstruction::Call {
                field: "body",
                form: "unknown-target",
            }],
            ..base
        }),
        changed_form(CommandForm {
            program: nested,
            kind: "Pipeline",
            ..base
        }),
        changed_form(CommandForm {
            program: &[CommandInstruction::Many {
                trigger: CommandTrigger::Token {
                    kind: "word",
                    literals: &[],
                },
                program: &[],
            }],
            ..base
        }),
        changed_form(CommandForm {
            program: &[CommandInstruction::Call {
                field: "body",
                form: "conditional-branch",
            }],
            ..base
        }),
        changed_form(CommandForm {
            program: &[CommandInstruction::Raw("not-a-field")],
            ..base
        }),
        changed_form(CommandForm {
            trigger: CommandTrigger::Role("separator"),
            ..base
        }),
        changed_form(CommandForm {
            trigger: CommandTrigger::Token {
                kind: "word",
                literals: &["while"],
            },
            ..base
        }),
        changed_form(CommandForm {
            program: &[CommandInstruction::Call {
                field: "body",
                form: "pipeline-prefix",
            }],
            ..base
        }),
        changed_form(CommandForm {
            id: "pipeline-prefix",
            kind: "Pipeline",
            trigger: CommandTrigger::Manual(None),
            priority: 0,
            program: &[CommandInstruction::Raw("keyword")],
        }),
    ] {
        assert!(PreparedCommandProgram::new(spec, &generated::bash::RESULT_PROFILE).is_err());
    }
}
#[test]
fn source_rejects_invalid_tapes_foreign_word_owners_and_bounded_nesting() {
    let program = PreparedCommandProgram::new(
        &generated::bash::COMMAND_PROFILE,
        &generated::bash::RESULT_PROFILE,
    )
    .unwrap();
    let parts = PreparedPartProfile::new(
        &generated::bash::PART_PROFILE,
        &generated::bash::RESULT_PROFILE,
        &generated::bash::regions::REGION,
    )
    .unwrap();
    let foreign = PreparedPartProfile::new(
        &generated::extended::PART_PROFILE,
        &generated::extended::RESULT_PROFILE,
        &generated::extended::regions::REGION,
    )
    .unwrap();
    let source = "echo α";
    let tokens = ContextualScanner::new(&generated::bash::scanner::SCANNER, source)
        .unwrap()
        .scan("source")
        .unwrap();
    assert!(program.parse_scanned(source, &tokens, &foreign).is_err());
    assert!(program.parse_scanned("", &[], &foreign).is_err());
    assert!(program.parse_scanned(source, &tokens[1..], &parts).is_err());
    assert!(
        program
            .parse_scanned(source, &tokens[..tokens.len() - 1], &parts)
            .is_err()
    );
    let source = format!("{}echo α; {}", "{ ".repeat(300), "; }".repeat(300));
    let tokens = ContextualScanner::new(&generated::bash::scanner::SCANNER, &source)
        .unwrap()
        .scan("source")
        .unwrap();
    let error = program
        .parse_scanned(&source, &tokens, &parts)
        .err()
        .unwrap();
    assert!(error.message.contains("nesting"), "{error:?}");
}

#[test]
fn admission_visits_shared_call_dag_once_without_expanding_execution_paths() {
    let names: Vec<&'static str> = (0..32)
        .map(|i| Box::leak(format!("dag-{i}").into_boxed_str()) as &'static str)
        .collect();
    let mut forms = generated::bash::COMMAND_PROFILE.forms.to_vec();
    for (index, &id) in names.iter().enumerate() {
        let program: &'static [CommandInstruction] = if index + 1 == names.len() {
            &[CommandInstruction::Raw("keyword")]
        } else {
            Box::leak(
                vec![
                    CommandInstruction::Call {
                        field: "command",
                        form: names[index + 1],
                    },
                    CommandInstruction::Call {
                        field: "command",
                        form: names[index + 1],
                    },
                ]
                .into_boxed_slice(),
            )
        };
        forms.push(CommandForm {
            id,
            kind: "Pipeline",
            trigger: CommandTrigger::Manual(None),
            priority: 0,
            program,
        });
    }
    let spec = Box::leak(Box::new(CommandProgramSpec {
        forms: Box::leak(forms.into_boxed_slice()),
        ..generated::bash::COMMAND_PROFILE
    }));
    PreparedCommandProgram::new(spec, &generated::bash::RESULT_PROFILE).unwrap();
}

#[test]
fn one_generated_source_product_executes_both_poo_declarations() {
    for (spec, controls) in [
        (&generated::bash::SOURCE, generated::bash::CONTROLS),
        (&generated::extended::SOURCE, generated::extended::CONTROLS),
    ] {
        let engine = crate::PreparedCommandSource::new(spec).unwrap();
        for control in controls {
            let parsed = engine.parse(control.source);
            assert_eq!(parsed.is_ok(), control.accepted, "{:?}", control.source);
            if let Ok(parsed) = parsed {
                assert_eq!(parsed.events().unwrap(), control.events);
            }
        }
    }
}

#[test]
fn source_product_rejects_missing_scanner_positions_and_terminal_publication() {
    static NO_SOURCE: crate::scanner::ScannerSpec = crate::scanner::ScannerSpec {
        positions: &["other"],
        ..generated::bash::scanner::SCANNER
    };
    static SOURCE: crate::CommandSourceSpec = crate::CommandSourceSpec {
        scanner: &NO_SOURCE,
        ..generated::bash::SOURCE
    };
    assert!(crate::PreparedCommandSource::new(&SOURCE).is_err());
}

fn renamed_id(id: &str) -> &'static str {
    Box::leak(format!("private/{id}").into_boxed_str())
}
fn renamed_program(program: &[CommandInstruction]) -> &'static [CommandInstruction] {
    use CommandInstruction::{Balance, Branch, Call, Choose, Many, Node, Optional, Until};
    let rows: Vec<_> = program
        .iter()
        .map(|step| match *step {
            Call { field, form } => Call {
                field,
                form: renamed_id(form),
            },
            Node {
                field,
                kind,
                program,
            } => Node {
                field,
                kind,
                program: renamed_program(program),
            },
            Optional { trigger, program } => Optional {
                trigger,
                program: renamed_program(program),
            },
            Many { trigger, program } => Many {
                trigger,
                program: renamed_program(program),
            },
            Until { trigger, program } => Until {
                trigger,
                program: renamed_program(program),
            },
            Branch { trigger, yes, no } => Branch {
                trigger,
                yes: renamed_program(yes),
                no: renamed_program(no),
            },
            Choose(choices) => Choose(Box::leak(
                choices
                    .iter()
                    .map(|choice| crate::CommandChoice {
                        trigger: choice.trigger,
                        program: renamed_program(choice.program),
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            )),
            Balance {
                open,
                close,
                depth,
                open_field,
                close_field,
                program,
            } => Balance {
                open,
                close,
                depth,
                open_field,
                close_field,
                program: renamed_program(program),
            },
            leaf => leaf,
        })
        .collect();
    Box::leak(rows.into_boxed_slice())
}
#[test]
fn private_rule_renaming_retains_scheme_events_and_rejection_controls() {
    let original = &generated::bash::COMMAND_PROFILE;
    let renamed = Box::leak(Box::new(CommandProgramSpec {
        assignment_tail: renamed_id(original.assignment_tail),
        pipeline_head: renamed_id(original.pipeline_head),
        forms: Box::leak(
            original
                .forms
                .iter()
                .map(|form| CommandForm {
                    id: renamed_id(form.id),
                    program: renamed_program(form.program),
                    ..*form
                })
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        ),
        ..*original
    }));
    let program = PreparedCommandProgram::new(renamed, &generated::bash::RESULT_PROFILE).unwrap();
    let parts = PreparedPartProfile::new(
        &generated::bash::PART_PROFILE,
        &generated::bash::RESULT_PROFILE,
        &generated::bash::regions::REGION,
    )
    .unwrap();
    for control in generated::bash::CONTROLS {
        let outcome = ContextualScanner::new(&generated::bash::scanner::SCANNER, control.source)
            .unwrap()
            .scan("source")
            .and_then(|tokens| program.parse_scanned(control.source, &tokens, &parts));
        assert_eq!(outcome.is_ok(), control.accepted, "{}", control.source);
        if let Ok(parsed) = outcome {
            let events = parsed.events().unwrap();
            assert_eq!(events, control.events, "{}", control.source);
            assert_eq!(parsed.here_documents, control.links, "{}", control.source);
            let tree = build_syntax_events_catalog(
                &EventCatalog {
                    kinds: generated::bash::RESULT_PROFILE.kinds,
                    root_kind: parsed.root.kind(),
                },
                control.source,
                &events,
            )
            .unwrap();
            assert_eq!(tree.root().text().to_string(), control.source);
        }
    }
}
