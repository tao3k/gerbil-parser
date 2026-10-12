"""Admission controls use the real Scheme ASCENT C ABI, not Python inference."""
import os
from pathlib import Path
import sys
import tempfile
import unittest

from parser_evidence import Check, NativeReview, Observation, observe, review


class HostControls(unittest.TestCase):
    def test_nonzero_exit_cannot_be_admitted_by_a_marker(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            (root / "source").write_text("revision")
            check = Check("lean-model", (sys.executable, "-c", "print('OK'); exit(1)"),
                          ("source",), ("OK",))
            result = observe(root, check)
            self.assertEqual((result.outcome, result.exit_code), ("failed", 1))
            self.assertEqual(result.output.strip(), b"OK")

    def test_missing_marker_and_source_mutation_are_rejected(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            (root / "source").write_text("revision")
            for code, expected in (("print('partial')", "failed"),
                                   ("open('source','w').write('new'); print('OK')", "changed-inputs")):
                check = Check("quint-protocol", (sys.executable, "-c", code), ("source",), ("OK",))
                self.assertEqual(observe(root, check).outcome, expected)

    def test_timeout_is_retained(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            (root / "source").write_text("revision")
            check = Check("lean-model", (sys.executable, "-c", "import time; time.sleep(2)"),
                          ("source",), ("OK",), timeout=1)
            result = observe(root, check)
            self.assertEqual((result.outcome, result.exit_code), ("timeout", None))


class NativeReviewControls(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.engine = NativeReview(Path(os.environ["ASCENT_ENGINE_LIBRARY"]))

    def setUp(self):
        self.folder = tempfile.TemporaryDirectory()
        self.addCleanup(self.folder.cleanup)
        self.root = Path(self.folder.name)
        (self.root / "source").write_text("revision")
        self.check = Check("lean-model", (sys.executable, "-c", "print('OK')"),
                           ("source",), ("OK",))
        self.item = observe(self.root, self.check)
        self.item.save(self.root)

    def rows(self, items=None, dependencies=()):
        return review(self.engine, self.root, items or (self.item,),
                      dependencies=dependencies)["review"]["result"]["relations"]

    def test_partial_bundle_exposes_all_missing_layers(self):
        rows = self.rows()
        self.assertEqual({row[1] for row in rows["Missing"]},
                         {"quint-protocol", "scheme-conformance", "matched-performance"})
        self.assertEqual(rows["Reusable"], [[self.item.identity, "lean-model"]])

    def test_source_and_log_mutations_invalidate_reuse(self):
        (self.root / "source").write_text("new revision")
        self.assertEqual(self.rows()["Reusable"], [])
        (self.root / "source").write_text("revision")
        (self.root / self.item.save(self.root)).write_bytes(b"tampered")
        self.assertEqual(self.rows()["Reusable"], [])

    def test_metadata_mutation_invalidates_reuse(self):
        metadata = self.root / f".data/parser-evidence/{self.item.identity}.json"
        metadata.write_text("{}")
        self.assertEqual(self.rows()["Reusable"], [])

    def test_cycle_and_missing_dependencies_invalidate_reuse(self):
        for dependency in (self.item.identity, "absent"):
            rows = self.rows(dependencies=((self.item.identity, dependency),))
            self.assertEqual(rows["Reusable"], [])

    def test_failed_dependency_invalidates_transitive_dependents(self):
        failed = Observation(self.check, self.item.inputs, b"failure", 1, "failed", self.item.executable)
        failed.save(self.root)
        other = Observation(self.check, self.item.inputs, b"OK again", 0, "passed", self.item.executable)
        other.save(self.root)
        rows = self.rows((self.item, other, failed),
                         ((self.item.identity, other.identity), (other.identity, failed.identity)))
        self.assertEqual(rows["Reusable"], [])

    def test_complete_runtime_bundle_requires_successful_build(self):
        items = []
        for claim in ("lean-model", "quint-protocol", "scheme-build",
                      "scheme-conformance", "matched-performance"):
            check = Check(claim, self.check.argv, self.check.inputs, self.check.markers)
            item = observe(self.root, check)
            item.save(self.root)
            items.append(item)
        build, semantics, measurement = items[2:]
        edges = ((semantics.identity, build.identity),
                 (measurement.identity, semantics.identity))
        self.assertEqual(self.rows(tuple(items), edges)["Missing"], [])
        failed = Observation(build.check, build.inputs, b"failure", 1, "failed", build.executable)
        failed.save(self.root)
        edges = ((semantics.identity, failed.identity),
                 (measurement.identity, semantics.identity))
        rows = self.rows(tuple(items[:2] + [failed, semantics, measurement]), edges)
        self.assertEqual({row[1] for row in rows["Missing"]},
                         {"scheme-conformance", "matched-performance"})

    def test_measurement_receipt_cannot_establish_cpu_improvement(self):
        check = Check("matched-performance", self.check.argv, self.check.inputs, self.check.markers)
        measured = observe(self.root, check)
        measured.save(self.root)
        required = ("matched-performance", "record-cpu-improvement")
        rows = review(self.engine, self.root, (measured,), required=required)["review"]["result"]["relations"]
        self.assertEqual({row[1] for row in rows["Missing"]}, {"record-cpu-improvement"})
        check = Check("record-cpu-improvement", self.check.argv, self.check.inputs, self.check.markers)
        admitted = observe(self.root, check)
        admitted.save(self.root)
        edges = ((admitted.identity, measured.identity),)
        rows = review(self.engine, self.root, (measured, admitted), required=required,
                      dependencies=edges)["review"]["result"]["relations"]
        self.assertEqual(rows["Missing"], [])
        failed = Observation(check, admitted.inputs, b"rejected", 1, "failed", admitted.executable)
        failed.save(self.root)
        rows = review(self.engine, self.root, (measured, failed), required=required)["review"]["result"]["relations"]
        self.assertEqual({row[1] for row in rows["Missing"]}, {"record-cpu-improvement"})

    def test_review_never_authorizes_execution(self):
        result = review(self.engine, self.root, (self.item,))
        self.assertFalse(result["review"]["executionAuthorized"])
        with self.assertRaises(ValueError):
            review(self.engine, self.root, (self.item,), required=())


if __name__ == "__main__":
    unittest.main()
