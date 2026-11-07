"""Join current Fold model and source-owned Scheme controls in native ASCENT.

This collector deliberately supplies no performance observation: engine
differential controls cannot stand in for the real Org 10 ms preflight.
Python transports observations; Just owns compilation/tests and ASCENT owns
gap inference. It does not start a benchmark matrix or a second runtime.
"""
import argparse
import json
import os
from pathlib import Path

from collect_engine import engine_inputs
from collect_models import model_checks
from parser_evidence import Check, NativeReview, observe, review, source_path


def collect(root: Path, output: str, load_path: str, real_product: str | None = None):
    products = source_path(root, output)
    if products.exists():
        raise ValueError("fresh Fold output required; do not reuse a running qualification")
    inputs = engine_inputs(root) + ("justfile",)
    recipe = "qualify-fold-plan"
    arguments = (str(products), load_path)
    if real_product is not None:
        actual = source_path(root, real_product)
        artifacts = tuple(str(path.relative_to(root)) for path in sorted(actual.iterdir()) if path.is_file())
        for name in ("org-fold", "org-fold-controls.ss", "fold-input-program.ss"):
            if not (actual / name).is_file():
                raise ValueError(f"missing real AOT artifact: {name}")
        inputs += artifacts
        recipe = "qualify-fold-plan-and-real"
        arguments += (str(actual),)
    checks = model_checks(root) + (
        Check("scheme-conformance",
              ("just", recipe) + arguments,
              inputs, ("OK\n",), timeout=240),
    )
    observations = []
    for check in checks:
        item = observe(root, check)
        item.save(root)
        observations.append(item)
        print(f"FOLD-EVIDENCE {check.claim} {item.outcome} {item.identity}", flush=True)
        if item.outcome != "passed":
            break
    result = review(NativeReview(Path(os.environ["ASCENT_ENGINE_LIBRARY"])),
                    root, tuple(observations))
    result["scope"] = ("Fold compiler model and native differential controls only; "
                       "no real Org performance, FFI, Tokio or downstream acceptance.")
    result["realAotControlsBound"] = real_product is not None
    packet = root / ".data/parser-evidence/fold-controls.json"
    packet.parent.mkdir(parents=True, exist_ok=True)
    packet.write_text(json.dumps(result, indent=2) + "\n")
    rows = result["review"]["result"]["relations"]
    print(json.dumps({"established": rows["Established"], "missing": rows["Missing"],
                      "executionAuthorized": result["review"]["executionAuthorized"]}, indent=2))
    if any(item.outcome != "passed" for item in observations):
        raise SystemExit(1)
    if {row[1] for row in rows["Missing"]} != {"matched-performance"}:
        raise RuntimeError("unexpected Fold evidence closure")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", help="fresh repository-relative product directory")
    parser.add_argument("load_path", help="prepared compiler dependency search path")
    parser.add_argument("--real-product", help="repository-local copied real AOT artifacts")
    args = parser.parse_args()
    collect(Path(__file__).resolve().parents[2], args.output, args.load_path, args.real_product)
