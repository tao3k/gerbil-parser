"""Execute the model layer and ask native ASCENT for the remaining evidence gaps."""
import json
import os
from pathlib import Path

from parser_evidence import Check, NativeReview, observe, review


def model_checks(root: Path):
    lean_inputs = tuple(str(path.relative_to(root)) for path in sorted((root / "formal").rglob("*.lean"))
                        if ".lake" not in path.parts)
    lean_inputs += ("formal/lean-toolchain", "formal/lakefile.toml", "formal/lake-manifest.json",
                    "src/runtime/reduce.ss", "src/runtime/reduction-plan.ss", "src/runtime/funcs.ss")
    # Materialized dependency contents are bound separately from the lockfile pin.
    dependency = root / "formal/.lake/packages/LeanPoo/LeanPoo"
    lean_inputs += tuple(str(path.relative_to(root)) for path in sorted(dependency.rglob("*.lean")))
    if not dependency.is_dir():
        raise RuntimeError("prepare the locked Lean dependency with just formal first")
    policy_inputs = ("formal/evidence/parser_evidence.py", "formal/evidence/collect_models.py",
                     "formal/evidence/pyproject.toml", "formal/evidence/uv.lock")
    return (
        Check("lean-model", ("lake", "build"), lean_inputs + policy_inputs,
              ("Build completed successfully",), cwd="formal"),
        Check("quint-protocol", ("npm", "test"),
              ("formal/quint/Publication.qnt", "formal/quint/package.json",
               "formal/quint/package-lock.json", "formal/quint/node_modules/@informalsystems/quint/package.json") + policy_inputs,
              ("7 passing",), cwd="formal/quint"),
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
