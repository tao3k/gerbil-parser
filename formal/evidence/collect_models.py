"""Execute the model layer and ask native ASCENT for the remaining evidence gaps."""
import json
import os
from pathlib import Path

from parser_evidence import Check, NativeReview, observe, review
from model_inputs import SHARED_OWNERS, SHARED_INPUTS, LEAN_INPUTS, QUINT_INPUTS


def model_checks(root: Path):
    """Discover model modules and bind the shared native/policy owner catalogue."""
    dependency = root / "formal/.lake/packages/LeanPoo/LeanPoo"
    if not dependency.is_dir():
        raise RuntimeError("prepare the locked Lean dependency with just formal first")

    def discover(folder, pattern):
        return tuple(str(path.relative_to(root)) for path in sorted((root / folder).rglob(pattern))
                     if ".lake" not in path.relative_to(root / folder).parts)

    def inputs(*groups):
        return tuple(sorted(set(path for group in groups for path in group)))

    owners = tuple(str(path.relative_to(root)) for pattern in SHARED_OWNERS
                   for path in root.glob(pattern) if path.is_file())
    shared = inputs(SHARED_INPUTS, owners)
    dependency_inputs = tuple(str(path.relative_to(root)) for path in sorted(dependency.rglob("*.lean")))
    return (
        Check("lean-model", ("lake", "build"),
              inputs(discover("formal", "*.lean"), dependency_inputs, LEAN_INPUTS, shared),
              ("Build completed successfully",), cwd="formal"),
        Check("quint-protocol", ("npm", "test"),
              inputs(discover("formal/quint", "*.qnt"), QUINT_INPUTS, shared),
              ("No violation found",), cwd="formal/quint"),
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
