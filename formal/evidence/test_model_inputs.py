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
            lean, quint = model_checks(root)
            self.assertEqual((lean.claim, quint.claim), ("lean-model", "quint-protocol"))
            self.assertIn("formal/ParserEngine/Reuse.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Capture.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Lookahead.lean", lean.inputs)
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
                self.assertIn("src/ffi/language-handles.ss", check.inputs)
                self.assertIn("src/runtime/lr-parser/checkpoint.ss", check.inputs)
                self.assertIn("src/runtime/lr-parser/prepare.ss", check.inputs)
                self.assertIn("src/runtime/lr-parser/entry.ss", check.inputs)
                self.assertIn("t/canonical-lr1-reference-test.ss", check.inputs)
                self.assertIn("src/runtime/lr-stack-storage.ss", check.inputs)
                self.assertIn("src/runtime/scan/regular.ss", check.inputs)
                self.assertIn("t/ranked-regular-scanner-test.ss", check.inputs)
                self.assertIn("t/fixtures/tla-sany-differential/test-suite.ss", check.inputs)
                self.assertIn("t/lr-input-protocol-test.ss", check.inputs)
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
            for name in quint.inputs:
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("input")
            first = snapshot(root, quint.inputs)
            (root / "formal/quint/ReuseAdmission.qnt").write_text("changed protocol")
            self.assertNotEqual(first, snapshot(root, quint.inputs))
            for name in ("formal/quint/LookaheadScope.qnt", "formal/quint/ModuleAdmission.qnt",
                         "src/runtime/lr-parser/checkpoint.ss", "src/runtime/token.ss",
                         "src/language/module-input.ss", "t/fixtures/tla-sany-differential/preload.ss"):
                with self.subTest(input=name):
                    before = snapshot(root, quint.inputs)
                    (root / name).write_text("changed extent premise")
                    self.assertNotEqual(before, snapshot(root, quint.inputs))
            self.assertEqual({lean.claim, quint.claim}, {"lean-model", "quint-protocol"})


if __name__ == "__main__":
    unittest.main()
