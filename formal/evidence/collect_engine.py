"""Execute model, Scheme and request checks; native ASCENT joins their receipts."""
import json
import os
from pathlib import Path

from collect_models import model_checks
from parser_evidence import Check, NativeReview, observe, review


TESTS = (
    "t/lr-stack-reduction-test.ss", "t/fused-reduction-test.ss",
    "t/recognition-sequence-test.ss", "t/artifact-publication-cost-test.ss",
    "t/gql/benchmark-profile-test.ss", "t/language-abi-test.ss",
)


def engine_inputs(root):
    # Bind the declared source closure, including workload and admission owners.
    paths = set()
    for folder in ("src", "languages", "t", "scripts", "formal/evidence"):
        paths.update(str(path.relative_to(root)) for path in (root / folder).rglob("*")
                     if path.is_file() and path.suffix in (".ss", ".py", ".toml", ".lock")
                     and "__pycache__" not in path.parts and ".venv" not in path.parts)
    paths.update(str(path.relative_to(root)) for path in root.glob("*.ss"))
    paths.add("gerbil.pkg")
    return tuple(sorted(paths))


def supervised(claim, name, command, inputs, markers=(), build=False):
    budget, idle = (240, 30) if build else (120, 5)
    argv = ("gerbil", "env", "gxi", "qualify.ss", "run", "--timeout", str(budget),
            "--idle-timeout", str(idle), "--log", f".data/parser-evidence/{name}.log")
    for marker in markers:
        argv += ("--require", marker)
    return Check(claim, argv + ("--",) + command, inputs,
                 ("PROCESS-RESULT",) + tuple(marker.strip("^$") for marker in markers), timeout=budget + 20)


def collect(root):
    inputs = engine_inputs(root)
    observations = []
    dependencies = []

    def execute(check):
        item = observe(root, check)
        item.save(root)
        observations.append(item)
        print(f"EVIDENCE {check.claim} {item.outcome} {item.identity}", flush=True)
        return item

    for check in model_checks(root):
        execute(check)
    builds = (
        ("source-build", ("gxi", "build.ss", "compile")),
        ("test-driver-build", ("gxi", "build-test-driver.ss", "compile")),
        ("ffi-build", ("gxi", "build-ffi-tests.ss", "compile")),
        ("request-build", ("gxi", "t/benchmarks/build.ss", "compile")),
    )
    prepared = []
    for name, command in builds:
        item = execute(supervised("scheme-build", name, command, inputs, build=True))
        prepared.append(item)
        if item.outcome != "passed":
            break
    if len(prepared) == len(builds) and all(item.outcome == "passed" for item in prepared):
        # Bind the actual prepared products as well as their source declarations.
        products = tuple(sorted(str(path.relative_to(root))
                         for path in (root / ".gerbil/lib").rglob("*") if path.is_file()))
        runtime_inputs = inputs + products
        semantics = execute(supervised("scheme-conformance", "conformance",
            ("gxi", "-e", '(load "t/fixtures/tla-sany-differential/preload.ss") '
             '(prefer-compiled-interfaces!) '
             '(preload-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")',
             "-e", '(import :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process) '
             '(load "t/fixtures/tla-sany-differential/test-suite.ss") '
             '(eval (quote (main ' + " ".join(json.dumps(file) for file in TESTS) + '))) '
             '(test-child-process-exit! 0)'),
            runtime_inputs, ("TEST-SUITE-OK modules=6", "^OK$")))
        benchmark = execute(supervised("matched-performance", "requests",
            ("gxi", "-e", '(load "t/fixtures/tla-sany-differential/preload.ss") '
             '(prefer-compiled-interfaces!) '
             '(preload-module "gerbil-parser/t/benchmarks/parser-evidence") '
             '(preload-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")',
             "-e", '(import :gerbil-parser/t/benchmarks/parser-evidence '
             ':gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process) '
             '(main) (test-child-process-exit! 0)'), runtime_inputs,
            ("GQL-STAGES-OK", "LR-REQUEST-BACKENDS-OK", "PARSER-EVIDENCE-REQUESTS-OK")))
        dependencies.extend((semantics.identity, item.identity) for item in prepared)
        dependencies.extend((benchmark.identity, item.identity) for item in (*prepared, semantics))
    engine = NativeReview(Path(os.environ["ASCENT_ENGINE_LIBRARY"]))
    result = review(engine, root, tuple(observations), dependencies=tuple(dependencies))
    output = root / ".data/parser-evidence/engine.json"
    output.write_text(json.dumps(result, indent=2) + "\n")
    rows = result["review"]["result"]["relations"]
    print(json.dumps({"established": rows["Established"], "missing": rows["Missing"],
                      "executionAuthorized": result["review"]["executionAuthorized"]}, indent=2))
    if rows["Missing"] or any(item.outcome != "passed" for item in observations):
        raise SystemExit(1)


if __name__ == "__main__":
    collect(Path(__file__).resolve().parents[2])
