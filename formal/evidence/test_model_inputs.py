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
            lean, quint = model_checks(root)
            self.assertEqual((lean.claim, quint.claim), ("lean-model", "quint-protocol"))
            self.assertIn("formal/ParserEngine/Reuse.lean", lean.inputs)
            self.assertIn("formal/ParserEngine/Capture.lean", lean.inputs)
            self.assertIn("formal/quint/ReuseAdmission.qnt", quint.inputs)
            self.assertIn("formal/quint/HistoryCommit.qnt", quint.inputs)
            self.assertIn("formal/quint/CapturePlan.qnt", quint.inputs)
            for check in (lean, quint):
                self.assertIn("src/ffi/language-handles.ss", check.inputs)
                self.assertIn("scripts/tests/history-commit-test.ss", check.inputs)
            self.assertNotIn("formal/quint/ReuseAdmission.qnt", lean.inputs)
            for name in quint.inputs:
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("input")
            first = snapshot(root, quint.inputs)
            (root / "formal/quint/ReuseAdmission.qnt").write_text("changed protocol")
            self.assertNotEqual(first, snapshot(root, quint.inputs))
            self.assertEqual({lean.claim, quint.claim}, {"lean-model", "quint-protocol"})


if __name__ == "__main__":
    unittest.main()
