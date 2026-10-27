"""Input discovery and stale-observation controls, not parser semantics."""
from pathlib import Path
import tempfile
import unittest

from collect_models import model_checks
from parser_evidence import snapshot


class ModelInputControls(unittest.TestCase):
    def setUp(self):
        self.folder = tempfile.TemporaryDirectory()
        self.addCleanup(self.folder.cleanup)
        self.root = Path(self.folder.name)
        self.write("formal/.lake/packages/LeanPoo/LeanPoo/Contract.lean")
        self.write("formal/ParserEngine/Grammar.lean")
        self.write("formal/quint/GrammarPipeline.qnt")
        self.write("src/compiler/lr-automaton.ss")
        self.write("docs/research/parser-engine-formal-model/grammar-semantics.org")
        self.write("t/fixtures/grammar-controls.ssi")
        self.checks = model_checks(self.root)
        for check in self.checks:
            for name in check.inputs:
                self.write(name)

    def write(self, name, text="fixture"):
        path = self.root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)

    def test_claims_commands_and_model_layers_are_distinct(self):
        lean, quint = self.checks
        self.assertEqual((lean.claim, quint.claim), ("lean-model", "quint-protocol"))
        self.assertEqual((lean.argv, quint.argv), (("lake", "build"), ("npm", "test")))
        self.assertIn("formal/ParserEngine/Grammar.lean", lean.inputs)
        self.assertNotIn("formal/ParserEngine/Grammar.lean", quint.inputs)
        self.assertIn("formal/quint/GrammarPipeline.qnt", quint.inputs)
        self.assertNotIn("formal/quint/GrammarPipeline.qnt", lean.inputs)
        for check in self.checks:
            self.assertEqual(len(check.inputs), len(set(check.inputs)))

    def test_new_modules_and_document_topics_are_discovered(self):
        modules = (
            ("formal/ParserEngine/Grammar/Suffix.lean", {"lean-model"}),
            ("formal/quint/SuffixFacts.qnt", {"quint-protocol"}),
            ("docs/research/parser-engine-formal-model/suffix-facts.org",
             {"lean-model", "quint-protocol"}),
            ("t/fixtures/suffix-controls.ssi", {"lean-model", "quint-protocol"}),
        )
        modules += tuple((name, {"lean-model", "quint-protocol"}) for name in (
            "src/grammar/declaration-types.ss", "src/graph/graph-query-types.ss",
            "src/language/source-boundary-types.ss", "src/testing/language-product.ss",
            "src/utilities/types.ss", "src/ffi/config.ss", "build-selected-language.ss",
        ))
        for name, expected in modules:
            with self.subTest(input=name):
                self.write(name)
                bound = {check.claim for check in model_checks(self.root) if name in check.inputs}
                self.assertEqual(bound, expected)
        self.write("formal/.lake/build/generated.lean")
        self.assertNotIn("formal/.lake/build/generated.lean", model_checks(self.root)[0].inputs)

    def test_changed_and_missing_sources_invalidate_both_layers(self):
        for check in self.checks:
            for name in ("src/compiler/lr-automaton.ss", "t/fixtures/grammar-controls.ssi",
                         "docs/research/parser-engine-formal-model/grammar-semantics.org"):
                with self.subTest(claim=check.claim, input=name):
                    before = snapshot(self.root, check.inputs)
                    self.write(name, "changed premise")
                    self.assertNotEqual(before, snapshot(self.root, check.inputs))
                    (self.root / name).unlink()
                    with self.assertRaises(FileNotFoundError):
                        snapshot(self.root, check.inputs)
                    self.write(name)

    def test_missing_locked_dependency_is_an_error(self):
        dependency = self.root / "formal/.lake/packages/LeanPoo/LeanPoo"
        dependency.rename(dependency.with_name("unavailable"))
        with self.assertRaisesRegex(RuntimeError, "locked Lean dependency"):
            model_checks(self.root)

    def test_repository_catalogue_resolves(self):
        root = Path(__file__).resolve().parents[2]
        for check in model_checks(root):
            with self.subTest(claim=check.claim):
                self.assertEqual(len(snapshot(root, check.inputs)), len(check.inputs))


class RepositoryLayoutControls(unittest.TestCase):
    def test_authored_sources_stay_within_the_module_budget(self):
        root = Path(__file__).resolve().parents[2]
        oversized = []
        for folder, extensions in (
            ("docs", {".org"}), ("formal/ParserEngine", {".lean"}),
            ("formal/quint", {".qnt"}), ("formal/evidence", {".py"}),
            ("src", {".ss"}), ("t", {".ss", ".ssi"}),
        ):
            for path in (root / folder).rglob("*"):
                if {"node_modules", ".venv", ".lake"}.intersection(path.parts) or path.suffix not in extensions:
                    continue
                count = len(path.read_text().splitlines())
                if count > 1000:
                    oversized.append((str(path.relative_to(root)), count))
        self.assertEqual(oversized, [], "split by semantic responsibility, without numbered continuations")


if __name__ == "__main__":
    unittest.main()
