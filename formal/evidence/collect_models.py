"""Execute the model layer and ask native ASCENT for the remaining evidence gaps."""
import json
import os
from pathlib import Path

from parser_evidence import Check, NativeReview, observe, review


def model_checks(root: Path):
    lean_inputs = tuple(str(path.relative_to(root)) for path in sorted((root / "formal").rglob("*.lean"))
                        if ".lake" not in path.parts)
    lean_inputs += ("formal/lean-toolchain", "formal/lakefile.toml", "formal/lake-manifest.json",
                    "src/runtime/recognition.ss", "src/runtime/token.ss", "src/runtime/reduce.ss", "src/runtime/reduction-plan.ss", "src/runtime/funcs.ss")
    lean_inputs += ("src/compiler/event-fold-analysis.ss",
                    "src/compiler/event-fold-call-plan.ss", "t/fold-call-plan-test.ss",
                    "src/compiler/event-fold-zero-state.ss",
                    "src/compiler/event-fold-byte-dispatch.ss",
                    "src/compiler/event-fold-prefix-family.ss",
                    "src/compiler/event-fold-shared-probes.ss",
                    "src/compiler/event-fold-scheme-context.ss",
                    "src/compiler/event-fold-scheme-modules.ss",
                    "src/compiler/event-fold-scheme-expressions.ss",
                    "src/compiler/event-fold-scheme-statements.ss")
    lean_inputs += ("src/compiler/event-fold-ordinary-run.ss",
                    "src/compiler/event-fold-consumer-state.ss", "t/fold-consumer-state-test.ss",
                    "src/compiler/event-fold-scheme-runs.ss",
                    "src/compiler/event-fold-scheme.ss",
                    "src/runtime/byte-spans.ss",
                    "src/runtime/source-lines.ss", "src/runtime/fold-tape.ss",
                    "t/fold-buffer-consumer-test.ss",
                    "t/fold-call-environment-test.ss",
                    "t/fold-run-test.ss",
                    "t/fold-byte-dispatch-test.ss",
                    "t/fold-condition-test.ss", "t/fold-frame-facts-test.ss", "t/fold-statement-scans-test.ss")
    # Materialized dependency contents are bound separately from the lockfile pin.
    dependency = root / "formal/.lake/packages/LeanPoo/LeanPoo"
    lean_inputs += tuple(str(path.relative_to(root)) for path in sorted(dependency.rglob("*.lean")))
    if not dependency.is_dir():
        raise RuntimeError("prepare the locked Lean dependency with just formal first")
    # Bind actual owners and controls for traceability, not checked refinement.
    bridge_inputs = ("t/core-observability-test.ss", "t/language-abi-benchmark-test.ss",
                     "t/selective-glr-benchmark-test.ss", "t/lalr-fixed-point-benchmark-test.ss",
                     "t/recovery-frontier-benchmark-test.ss", "t/versioned-language-benchmark-test.ss",
                     "t/incremental-suffix-benchmark-test.ss", "t/gql/runtime-benchmark-test.ss",
                     "t/opencypher-large-query-benchmark-test.ss", "t/benchmark-test.ss", "t/fixtures/tla-sany-differential/worker-control.ss",
                     "t/fixtures/tla-sany-differential/worker-left.ss", "t/fixtures/tla-sany-differential/worker-right.ss",
                     "t/fixtures/tla-sany-differential/worker-exclusive.ss",
                     "t/fixtures/tla-sany-differential/affinity-exclusive.ss",
                     "t/fixtures/tla-sany-differential/worker-exclusive-invalid.ss", "test-processes.ss", "src/compiler/bound-ir.ss", "t/bound-ir-index-test.ss",
                     "src/runtime/scan/lexical.ss", "src/runtime/scan/literal.ss", "src/runtime/scan/regular.ss", "t/ranked-regular-scanner-test.ss",
                     "t/canonical-lr1-reference-test.ss", "src/ffi/language-handles.ss", "scripts/tests/history-commit-test.ss",
                     "src/runtime/lr-parser/recognition.ss", "src/runtime/lr-parser/checkpoint.ss",
                     "src/runtime/lr-parser/transfer.ss", "src/runtime/lr-parser/prepare.ss", "src/runtime/parser.ss",
                     "src/runtime/lr-parser/batch.ss", "src/runtime/layout.ss", "src/runtime/lr-stack-storage.ss",
                     "t/fixtures/tla-sany-differential/test-suite.ss", "t/lr-input-protocol-test.ss", "t/lr-goto-plan-test.ss",
                     "t/lr-action-preparation-test.ss", "t/fixtures/lr-action-selection.ss",
                     "t/sparse-capture-test.ss", "t/lr-fragment-transfer-test.ss", "t/runtime-module-organization-test.ss",
                     "src/runtime/lr-parser/state.ss", "src/runtime/token.ss",
                     "src/language/module-input.ss", "src/ffi/rust-runtime-aot.ss",
                     "t/fixtures/tla-sany-differential/preload.ss",
                     "t/benchmarks/module-loading/controls.ss", "t/benchmarks/module-loading/benchmark.ss",
                     "t/benchmarks/module-loading/reference-preload.ss",
                     "t/benchmarks/capture-prefix/controls.ss", "t/benchmarks/capture-prefix/worker.ss",
                     "t/benchmarks/capture-prefix/benchmark.ss", "t/support/qualification/benchmark-qos.ss",
                     "t/benchmarks/capture-prefix/build-reference.ss",
                     "t/benchmarks/capture-prefix/phase-cost.ss", "t/benchmarks/parser-stage-cost/benchmark.ss",
                     "t/benchmarks/lr-request-stack/capture-reference.ss")
    policy_inputs = (".github/workflows/ci.yml", ".github/workflows/scheme.yml",
                     "docs/research/dsl-test-preparation.org", "docs/research/lr-request-model-foundation.org", "docs/research/capture-fold-cost-model.org",
                     "formal/evidence/parser_evidence.py", "formal/evidence/collect_models.py",
                     "formal/evidence/pyproject.toml", "formal/evidence/uv.lock")
    return (
        Check("lean-model", ("lake", "build"), lean_inputs + bridge_inputs + policy_inputs,
              ("Build completed successfully",), cwd="formal"),
        Check("quint-protocol", ("npm", "test"),
              ("formal/quint/CaptureContext.qnt", "formal/quint/YieldMemo.qnt", "formal/quint/OwnedTerminal.qnt", "formal/quint/SparseCapture.qnt", "formal/quint/ModuleAdmission.qnt", "formal/quint/LookaheadScope.qnt", "formal/quint/CapturePlan.qnt", "formal/quint/HistoryCommit.qnt", "formal/quint/ReuseAdmission.qnt", "formal/quint/Publication.qnt", "formal/quint/TapeOwner.qnt", "formal/quint/FoldPlan.qnt", "formal/quint/IdlePlan.qnt", "formal/quint/RunPlan.qnt", "formal/quint/ConsumerScope.qnt", "formal/quint/package.json",
               "formal/quint/package-lock.json", "formal/quint/node_modules/@informalsystems/quint/package.json") + bridge_inputs + policy_inputs,
              ("11 passing", "16 passing", "23 passing", "8 passing", "7 passing", "12 passing", "4 passing", "5 passing", "6 passing", "No violation found"), cwd="formal/quint"),
    )


def collect(root: Path):
    checks = model_checks(root)
    observations = tuple(observe(root, check) for check in checks)
    for item in observations:
        item.save(root)
    engine = NativeReview(Path(os.environ["ASCENT_ENGINE_LIBRARY"]))
    result = review(engine, root, observations)
    output = root / ".data/parser-evidence/models.json"
    output.write_text(json.dumps(result, indent=2) + "\n")
    rows = result["review"]["result"]["relations"]
    print(json.dumps({"outcomes": [item.outcome for item in observations],
                      "established": rows["Established"], "missing": rows["Missing"],
                      "executionAuthorized": result["review"]["executionAuthorized"]}, indent=2))
    if any(item.outcome != "passed" for item in observations):
        raise SystemExit(1)
    # Passing models qualify only this layer; required runtime gaps remain visible.
    if {row[1] for row in rows["Missing"]} != {"scheme-conformance", "matched-performance"}:
        raise RuntimeError("unexpected evidence closure: inspect the native review")


if __name__ == "__main__":
    collect(Path(__file__).resolve().parents[2])
