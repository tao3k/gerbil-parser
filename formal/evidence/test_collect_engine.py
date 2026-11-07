"""Measured native candidates cannot be reused after source or product drift."""
import hashlib
import json
from pathlib import Path
import tempfile
import unittest

from collect_engine import MEASUREMENT_MODULES, measured_candidate_inputs, shared_measurement_inputs


class MeasuredCandidateInputs(unittest.TestCase):
    def test_source_and_native_product_drift_are_rejected(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            source = root / "src/token.ss"
            source.parent.mkdir()
            source.write_text("original source")
            study = root / "study"
            product = study / "candidate/lib/module.o1"
            product.parent.mkdir(parents=True)
            product.write_bytes(b"original native product")
            digest = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
            manifest = {"sourceSha256": {"src/token.ss": digest(source)},
                        "selectedNativeModules": {"module": {"file": "module.o1",
                                                  "sha256": digest(product)}}}
            baseline_modules = {}
            for owner in MEASUREMENT_MODULES:
                for module in (owner, owner + "~0"):
                    for role in ("baseline", "candidate"):
                        image = study / role / "lib" / (module + ".o1")
                        image.parent.mkdir(parents=True, exist_ok=True)
                        image.write_bytes(b"shared measurement image")
                    record = {"file": module + ".o1", "sha256": digest(image)}
                    baseline_modules[module] = record.copy()
                    manifest["selectedNativeModules"][module] = record.copy()
            (study / "baseline.json").write_text(json.dumps({"selectedNativeModules": baseline_modules}))
            (study / "candidate.json").write_text(json.dumps(manifest))
            admitted = measured_candidate_inputs(root, study)
            self.assertIn("study/candidate/lib/module.o1", admitted)
            self.assertIn("study/candidate.json", admitted)
            self.assertIn("study/baseline.json", admitted)
            owner = MEASUREMENT_MODULES[0]
            manifest["selectedNativeModules"][owner]["sha256"] = "different"
            with self.assertRaisesRegex(ValueError, "differs between roles"):
                shared_measurement_inputs(root, study, manifest)
            manifest["selectedNativeModules"][owner]["sha256"] = baseline_modules[owner]["sha256"]
            reference = study / "baseline/lib" / baseline_modules[owner]["file"]
            reference.write_bytes(b"changed reference image")
            with self.assertRaisesRegex(ValueError, "reference product changed"):
                shared_measurement_inputs(root, study, manifest)
            reference.write_bytes(b"shared measurement image")
            source.write_text("later source")
            with self.assertRaisesRegex(ValueError, "source changed"):
                measured_candidate_inputs(root, study)
            source.write_text("original source")
            product.write_bytes(b"later native product")
            with self.assertRaisesRegex(ValueError, "product changed"):
                measured_candidate_inputs(root, study)


if __name__ == "__main__":
    unittest.main()
