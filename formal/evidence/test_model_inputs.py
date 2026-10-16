"""Model observations bind each source and preserve model-only claim scope."""
from pathlib import Path
import tempfile
import unittest

from collect_models import model_checks
from parser_evidence import snapshot


class ModelInputControls(unittest.TestCase):
    def test_reuse_models_are_bound_and_layers_stay_separate(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            dependency = root / "formal/.lake/packages/LeanPoo/LeanPoo"
            dependency.mkdir(parents=True)
            source = root / "formal/ParserEngine/Reuse.lean"
            source.parent.mkdir(parents=True)
            source.write_text("namespace Example\nend Example\n")
            capture = root / "formal/ParserEngine/Capture.lean"
            capture.write_text("namespace Capture\nend Capture\n")
            lookahead = root / "formal/ParserEngine/Lookahead.lean"
            lookahead.write_text("namespace Lookahead\nend Lookahead\n")
            request = root / "formal/ParserEngine/Request.lean"
            request.write_text("namespace Request\nend Request\n")
            (root / "formal/ParserEngine/Grammar.lean").write_text("namespace Grammar\nend Grammar\n")
            for module in ("Callback", "IteratorEncoding", "EventIterator", "RequestAST", "Publication", "TableBridge", "SemanticValues", "Viable", "RawRequest", "Completion", "NativeFacts", "Lowering", "Worklist", "Solver", "Encoding", "Names", "Construction", "Operational", "Host", "HostLowering"):
                (root / f"formal/ParserEngine/{module}.lean").write_text(f"namespace {module}\nend {module}\n")
            lean, quint = model_checks(root)
            self.assertEqual((lean.claim, quint.claim), ("lean-model", "quint-protocol"))
            self.assertEqual(len(lean.inputs), len(set(lean.inputs)))
            self.assertIn("formal/ParserEngine/Completion.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/NativeFacts.lean", lean.inputs)
            self.assertIn("formal/quint/Completion.qnt", quint.inputs)
            self.assertIn("formal/ParserEngine/Viable.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Construction.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Operational.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Host.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/HostLowering.lean", lean.inputs)
            self.assertIn("src/runtime/lr-parser/checkpoint-base.ss", lean.inputs)
            self.assertIn("src/runtime/lr-parser/checkpoint-base.ss", quint.inputs)
            self.assertIn("t/layout-boundary-test.ss", lean.inputs)
            self.assertIn("t/layout-boundary-test.ss", quint.inputs)
            self.assertIn("t/fixtures/layout-columns.ss", lean.inputs)
            self.assertIn("t/fixtures/layout-columns.ss", quint.inputs)
            self.assertIn("t/fixtures/compiler-reservation.ss", lean.inputs)
            self.assertIn("t/fixtures/compiler-reservation.ss", quint.inputs)
            self.assertIn("formal/ParserEngine/RawRequest.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/TableBridge.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/SemanticValues.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/RequestAST.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Publication.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/EventIterator.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/IteratorEncoding.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Callback.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Grammar.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Request.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Reuse.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Capture.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Lookahead.lean", lean.inputs)
            self.assertIn("formal/quint/RawRequest.qnt", quint.inputs)
            self.assertIn("formal/quint/GrammarPipeline.qnt", quint.inputs)
            self.assertIn("formal/quint/RequestFlow.qnt", quint.inputs)
            self.assertIn("formal/quint/LookaheadScope.qnt", quint.inputs)
            self.assertIn("formal/quint/ModuleAdmission.qnt", quint.inputs)
            self.assertIn("formal/quint/SparseCapture.qnt", quint.inputs)
            self.assertIn("formal/quint/ReuseAdmission.qnt", quint.inputs)
            self.assertIn("formal/quint/HistoryCommit.qnt", quint.inputs)
            self.assertIn("formal/quint/CapturePlan.qnt", quint.inputs)
            self.assertIn("formal/quint/OwnedTerminal.qnt", quint.inputs)
            self.assertIn("formal/quint/YieldMemo.qnt", quint.inputs)
            self.assertIn("formal/quint/CaptureContext.qnt", quint.inputs)
            for check in (lean, quint):
                self.assertIn("build-language-conformance-link.ss", check.inputs)
                self.assertIn("scripts/tests/conformance-runner-test.ss", check.inputs)
                self.assertIn("docs/research/parser-engine-formal-model.org", check.inputs)
                self.assertIn("src/ffi/language-handles.ss", check.inputs)
                self.assertIn("src/runtime/lr-parser/checkpoint.ss", check.inputs)
                self.assertIn("src/runtime/lr-parser/prepare.ss", check.inputs)
                self.assertIn("src/runtime/lr-parser/entry.ss", check.inputs)
                self.assertIn("t/canonical-lr1-reference-test.ss", check.inputs)
                self.assertIn("t/resolved-grammar-test.ss", check.inputs)
                self.assertIn("src/compiler/lr-table.ss", check.inputs)
                self.assertIn("src/compiler/lr-partition.ss", check.inputs)
                self.assertIn("src/compiler/lr-compiler.ss", check.inputs)
                self.assertIn("src/runtime/lr-action-index.ss", check.inputs)
                self.assertIn("src/runtime/lr-stack-storage.ss", check.inputs)
                self.assertIn("src/runtime/scan/regular.ss", check.inputs)
                self.assertIn("t/ranked-regular-scanner-test.ss", check.inputs)
                self.assertIn("t/fixtures/tla-sany-differential/test-suite.ss", check.inputs)
                self.assertIn("t/lr-input-protocol-test.ss", check.inputs)
                self.assertIn("t/lr-semantic-preparation-test.ss", check.inputs)
                self.assertIn("t/lr-model-correspondence-test.ss", check.inputs)
                self.assertIn("t/event-iterator-test.ss", check.inputs)
                self.assertIn("t/fixtures/event-iterator-extraction.ss", check.inputs)
                self.assertIn("t/language-abi-test.ss", check.inputs)
                self.assertIn("t/fixtures/ffi/language-probe.ss", check.inputs)
                self.assertIn("t/lr-request-product-test.ss", check.inputs)
                self.assertIn("t/fixtures/lr-request-product.ss", check.inputs)
                self.assertIn("src/runtime/funcs.ss", check.inputs)
                self.assertIn("src/runtime/artifact.ss", check.inputs)
                self.assertIn("src/runtime/significant.ss", check.inputs)
                self.assertIn("src/runtime/lexer.ss", check.inputs)
                self.assertIn("src/runtime/event-program.ss", check.inputs)
                self.assertIn("t/lr-goto-plan-test.ss", check.inputs)
                self.assertIn("t/lr-fragment-transfer-test.ss", check.inputs)
                self.assertIn("t/benchmarks/capture-prefix/phase-cost.ss", check.inputs)
                self.assertIn("t/benchmarks/parser-stage-cost/benchmark.ss", check.inputs)
                self.assertIn("src/compiler/bound-ir.ss", check.inputs)
                self.assertIn("t/bound-ir-index-test.ss", check.inputs)
                self.assertIn("t/rust-runtime-generation-test.ss", check.inputs)
                self.assertIn("test-processes.ss", check.inputs)
                self.assertIn("src/runtime/token.ss", check.inputs)
                self.assertIn("src/language/module-input.ss", check.inputs)
                self.assertIn("t/fixtures/tla-sany-differential/preload.ss", check.inputs)
                self.assertIn("scripts/tests/history-commit-test.ss", check.inputs)
            self.assertNotIn("formal/quint/ReuseAdmission.qnt", lean.inputs)
            for name in lean.inputs:
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                if not path.exists():
                    path.write_text("input")
            for module in ("Callback", "IteratorEncoding", "EventIterator", "RequestAST", "Publication", "TableBridge", "SemanticValues", "Viable", "RawRequest", "Completion", "NativeFacts", "Lowering", "Worklist", "Solver", "Encoding", "Names", "Operational"):
                before = snapshot(root, lean.inputs)
                (root / f"formal/ParserEngine/{module}.lean").write_text("changed graph/request premise")
                self.assertNotEqual(before, snapshot(root, lean.inputs))
            for name in quint.inputs:
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("input")
            first = snapshot(root, quint.inputs)
            (root / "formal/quint/ReuseAdmission.qnt").write_text("changed protocol")
            self.assertNotEqual(first, snapshot(root, quint.inputs))
            for name in ("formal/quint/Completion.qnt", "formal/ParserEngine/NativeFacts.lean", "t/generate-grammar-model-facts.ss", "formal/quint/RawRequest.qnt", "src/compiler/lr-follow-automaton.ss", "formal/quint/GrammarPipeline.qnt", "src/compiler/lr-table.ss", "src/compiler/funcs.ss", "src/schema.ss", "src/ffi/schema.ss", "src/modules/parser/types.ss", "t/language-surface-test.ss", "src/compiler/language-artifact.ss", "t/language-artifact-test.ss", "src/compiler/lr-compiler.ss",
                         "t/resolved-grammar-test.ss", "t/layout-boundary-test.ss", "formal/quint/RequestFlow.qnt", "docs/research/parser-engine-formal-model.org",
                         "formal/quint/LookaheadScope.qnt", "formal/quint/ModuleAdmission.qnt",
                         "src/runtime/lr-parser/checkpoint.ss", "src/runtime/token.ss",
                         "src/language/module-input.ss", "t/fixtures/tla-sany-differential/preload.ss"):
                with self.subTest(input=name):
                    before = snapshot(root, quint.inputs)
                    (root / name).write_text("changed extent premise")
                    self.assertNotEqual(before, snapshot(root, quint.inputs))
            self.assertEqual({lean.claim, quint.claim}, {"lean-model", "quint-protocol"})


if __name__ == "__main__":
    unittest.main()
