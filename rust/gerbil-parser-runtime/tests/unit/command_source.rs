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
fn source_rejects_invalid_tapes_and_foreign_word_owners() {
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

#[test]
fn source_suspends_mixed_compounds_and_releases_deep_results_on_small_stack() {
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(|| {
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
            // Alternation crosses Source List -> Command VM -> Compound repeatedly.
            let source = format!("{}echo α; {}", "{ ( ".repeat(512), "); } ; ".repeat(512));
            let tokens = ContextualScanner::new(&generated::bash::scanner::SCANNER, &source)
                .unwrap()
                .scan("source")
                .unwrap();
            let parse = program.parse_scanned(&source, &tokens, &parts).unwrap();
            assert_eq!(parse.root.span(), 0..source.len());
            let mut body_fields = 0;
            parse
                .walk_events(|event| {
                    if let crate::CommandEvent::StartField { name: "body", .. } = event {
                        body_fields += 1;
                    }
                })
                .unwrap();
            assert_eq!(body_fields, 1024);
            let events = parse.events().unwrap();
            let tree = build_syntax_events_catalog(
                &EventCatalog {
                    kinds: generated::bash::RESULT_PROFILE.kinds,
                    root_kind: parse.root.kind(),
                },
                &source,
                &events,
            )
            .unwrap();
            assert_eq!(tree.root().text().to_string(), source);
            assert_eq!(
                fields(&parse.root).iter().filter(|f| **f == "body").count(),
                1024
            );
            drop(tree);
            drop(parse);
            // Exercise cleanup with a completed deep child retained by an unfinished parent.
            let rejected = format!("{source} |");
            let tokens = ContextualScanner::new(&generated::bash::scanner::SCANNER, &rejected)
                .unwrap()
                .scan("source")
                .unwrap();
            assert!(program.parse_scanned(&rejected, &tokens, &parts).is_err());
            // Failure state must not contaminate the immutable plan's next request.
            let tokens = ContextualScanner::new(&generated::bash::scanner::SCANNER, "echo ok")
                .unwrap()
                .scan("source")
                .unwrap();
            assert!(program.parse_scanned("echo ok", &tokens, &parts).is_ok());
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn source_frame_budget_unwinds_and_keeps_next_parse_independent() {
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
    let source = format!("{}echo ok; {}", "{ ".repeat(4096), "}; ".repeat(4096));
    let tokens = ContextualScanner::new(&generated::bash::scanner::SCANNER, &source)
        .unwrap()
        .scan("source")
        .unwrap();
    let error = program
        .parse_scanned(&source, &tokens, &parts)
        .err()
        .unwrap();
    assert_eq!(error.reason_kind, "command-source-resource-limit");
    assert!(source.is_char_boundary(error.byte_offset));
    let tokens = ContextualScanner::new(&generated::bash::scanner::SCANNER, "echo ok")
        .unwrap()
        .scan("source")
        .unwrap();
    assert!(program.parse_scanned("echo ok", &tokens, &parts).is_ok());
}

#[test]
fn source_budget_also_bounds_a_vm_call_chain_without_source_suspensions() {
    let names: Vec<&'static str> = (0..6000)
        .map(|i| Box::leak(format!("frame-{i}").into_boxed_str()) as &'static str)
        .collect();
    let mut forms = generated::bash::COMMAND_PROFILE.forms.to_vec();
    let brace = forms.iter_mut().find(|f| f.id == "brace-group").unwrap();
    brace.program = Box::leak(
        vec![CommandInstruction::Call {
            field: "body",
            form: names[0],
        }]
        .into_boxed_slice(),
    );
    for (index, &id) in names.iter().enumerate() {
        let program: &'static [CommandInstruction] = if index + 1 == names.len() {
            &[CommandInstruction::Raw("keyword")]
        } else {
            Box::leak(
                vec![CommandInstruction::Call {
                    field: "command",
                    form: names[index + 1],
                }]
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
    let program = PreparedCommandProgram::new(spec, &generated::bash::RESULT_PROFILE).unwrap();
    let parts = PreparedPartProfile::new(
        &generated::bash::PART_PROFILE,
        &generated::bash::RESULT_PROFILE,
        &generated::bash::regions::REGION,
    )
    .unwrap();
    let tokens = ContextualScanner::new(&generated::bash::scanner::SCANNER, "{")
        .unwrap()
        .scan("source")
        .unwrap();
    let error = program.parse_scanned("{", &tokens, &parts).err().unwrap();
    assert_eq!(error.reason_kind, "command-source-resource-limit");
    let tokens = ContextualScanner::new(&generated::bash::scanner::SCANNER, "echo ok")
        .unwrap()
        .scan("source")
        .unwrap();
    assert!(program.parse_scanned("echo ok", &tokens, &parts).is_ok());
}
// Scheme emits this through the same codec used by the language-independent C ABI.
const NATIVE_CASES: &[u8] = include_bytes!("../fixtures/generated/command_source_native.bin");
fn native_bytes<'a>(input: &mut &'a [u8]) -> &'a [u8] {
    let (length, rest) = input.split_at(4);
    let length = u32::from_le_bytes(length.try_into().unwrap()) as usize;
    let (value, rest) = rest.split_at(length);
    *input = rest;
    value
}
fn canonical_event(
    event: crate::CommandEvent,
    results: &crate::ResultProfileSpec,
) -> (
    gerbil_parser_artifact::NativeEventKind,
    &'static str,
    u64,
    usize,
    usize,
) {
    use crate::CommandEvent;
    use gerbil_parser_artifact::NativeEventKind as Kind;
    match event {
        CommandEvent::StartNode { id, kind, offset } => (
            Kind::StartNode,
            results.kinds[kind as usize].name,
            id,
            offset,
            0,
        ),
        CommandEvent::FinishNode { id, kind, offset } => (
            Kind::FinishNode,
            results.kinds[kind as usize].name,
            id,
            0,
            offset,
        ),
        CommandEvent::StartField { name, offset } => (Kind::StartField, name, 0, offset, 0),
        CommandEvent::FinishField { name, offset } => (Kind::FinishField, name, 0, 0, offset),
        CommandEvent::Token {
            id,
            kind,
            start,
            end,
        } => (
            Kind::Token,
            results.kinds[kind as usize].name,
            id,
            start,
            end,
        ),
    }
}
#[test]
fn source_canonical_publication_matches_scheme_native_ids_fields_ranges_and_trivia() {
    use gerbil_parser_artifact::{NativeArtifactView, NativeCatalog, NativeEventKind as Kind};
    let mut input = NATIVE_CASES;
    let mut count = 0;
    for (name, spec, controls) in [
        ("bash", &generated::bash::SOURCE, generated::bash::CONTROLS),
        (
            "extended",
            &generated::extended::SOURCE,
            generated::extended::CONTROLS,
        ),
    ] {
        let engine = crate::PreparedCommandSource::new(spec).unwrap();
        for control in controls {
            assert_eq!(native_bytes(&mut input), name.as_bytes());
            let catalog = NativeCatalog::from_descriptor(native_bytes(&mut input)).unwrap();
            assert_eq!(native_bytes(&mut input), control.source.as_bytes());
            let payload = native_bytes(&mut input);
            let accepted = input[0] == 1;
            input = &input[1..];
            assert_eq!(accepted, control.accepted);
            let view = NativeArtifactView::decode(payload, control.source, &catalog).unwrap();
            assert_eq!(view.accepted(), accepted);
            let parsed = engine.parse(control.source);
            assert_eq!(parsed.is_ok(), accepted);
            if let Ok(parsed) = parsed {
                // Compare every canonical record, independently of CST projection.
                let expected: Vec<_> = view
                    .events()
                    .map(|event| {
                        let name = match event.kind {
                            Kind::StartNode | Kind::FinishNode => {
                                &catalog.kinds()[event.symbol as usize].name
                            }
                            Kind::StartField | Kind::FinishField => {
                                &catalog.fields()[event.symbol as usize]
                            }
                            Kind::Token => &catalog.terminals()[event.symbol as usize].name,
                        };
                        (event.kind, name.as_str(), event.id, event.start, event.end)
                    })
                    .collect();
                let mut actual = Vec::new();
                parsed
                    .walk_events(|event| actual.push(canonical_event(event, spec.results)))
                    .unwrap();
                assert_eq!(actual, expected, "{name} {:?}", control.source);
                assert_eq!(view.root().unwrap().text(), control.source);
                // A second publication starts its identifiers again, retaining no state.
                let mut replay = Vec::new();
                parsed.walk_events(|event| replay.push(event)).unwrap();
                let mut again = Vec::new();
                parsed.walk_events(|event| again.push(event)).unwrap();
                assert_eq!(again, replay);
            } else {
                assert!(view.root().is_none());
                let tokens: Vec<_> = view.tokens().collect();
                assert_eq!(tokens.len(), usize::from(!control.source.is_empty()));
                if let Some(token) = tokens.first() {
                    assert_eq!(token.kind(), "unparsed-source");
                    assert_eq!(token.text(), control.source);
                }
            }
            for token in view.tokens() {
                assert_eq!(
                    token.text().as_ptr(),
                    control.source[token.range().start..].as_ptr()
                );
            }
            // Payload/source identities must be admitted together, including rejects.
            let changed = format!("{}x", control.source);
            assert_eq!(
                NativeArtifactView::decode(payload, &changed, &catalog)
                    .unwrap_err()
                    .reason,
                "source-digest"
            );
            drop(view);
            count += 1;
        }
    }
    assert!(input.is_empty());
    assert_eq!(count, 54);
}

#[test]
fn canonical_source_publication_rejects_a_replaced_root_before_emitting() {
    let engine = crate::PreparedCommandSource::new(&generated::bash::SOURCE).unwrap();
    let source = String::from("echo α");
    let equal_source = source.clone();
    assert_ne!(source.as_ptr(), equal_source.as_ptr());
    let mut parsed = engine.parse(&source).unwrap();
    assert_eq!(parsed.source().as_ptr(), source.as_ptr());
    let foreign = engine.parse(&equal_source).unwrap();
    parsed.root = foreign.root;
    let mut count = 0;
    let error = parsed.walk_events(|_| count += 1).unwrap_err();
    assert_eq!(count, 0);
    assert_eq!(error.message, "foreign command Source root");
    assert!(parsed.events().is_err());
    // Equal bytes do not authenticate the original source allocation.
    assert!(engine.parse(&source).unwrap().events().is_ok());
}
