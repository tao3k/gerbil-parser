"""Execute model, Scheme and request checks; native ASCENT joins their receipts."""
import argparse
import json
import os
from pathlib import Path

from collect_models import model_checks
from parser_evidence import Check, NativeReview, REQUIREMENTS, observe, review


TESTS = (
    "t/lr-stack-reduction-test.ss", "t/fused-reduction-test.ss",
    "t/lr-action-preparation-test.ss",
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


def collect(root, record_study=None):
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
            runtime_inputs, (f"TEST-SUITE-OK modules={len(TESTS)}", "^OK$")))
        benchmark = execute(supervised("matched-performance", "requests",
            ("gxi", "-e", '(load "t/fixtures/tla-sany-differential/preload.ss") '
             '(prefer-compiled-interfaces!) '
             '(preload-runtime-module "gerbil-parser/t/benchmarks/parser-evidence") '
             '(preload-runtime-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")',
             "-e", '(gerbil-parser/t/benchmarks/parser-evidence#evidence-main) '
             '(gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process#test-child-process-exit! 0)'), runtime_inputs,
            ("GQL-STAGES-OK", "LR-REQUEST-BACKENDS-OK", "PARSER-EVIDENCE-REQUESTS-OK")))
        dependencies.extend((semantics.identity, item.identity) for item in prepared)
        dependencies.extend((benchmark.identity, item.identity) for item in (*prepared, semantics))
        if record_study is not None:
            # Scheme recomputes admission from raw samples. Python transports
            # receipts and both product closures, without computing statistics.
            study = root / record_study
            receipts = tuple(str((study / f"{name}.log").relative_to(root))
                             for name in ("reference-a", "candidate-a", "candidate-b", "reference-b"))
            baseline = tuple(sorted(str(path.relative_to(root))
                             for path in (study / "baseline/lib").rglob("*") if path.is_file()))
            if not baseline:
                raise ValueError("record admission requires an independently retained baseline")
            admission_inputs = runtime_inputs + receipts + baseline + (str((study / "baseline.json").relative_to(root)),)
            admission = execute(supervised("record-cpu-improvement", "record-admission",
                ("gxi", "-e", '(load "t/fixtures/tla-sany-differential/preload.ss") '
                 '(prefer-compiled-interfaces!) '
                 '(preload-runtime-module "gerbil-parser/t/benchmarks/lr-request-stack/record-evidence") '
                 '(preload-runtime-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process")',
                 "-e", '(gerbil-parser/t/benchmarks/lr-request-stack/record-evidence#main '
                 + " ".join(json.dumps(path) for path in receipts) + ') '
                 '(gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process#test-child-process-exit! 0)'),
                admission_inputs, ("RECORD-CPU-ADMISSION-OK",)))
            dependencies.extend((admission.identity, item.identity) for item in (*prepared, semantics, benchmark))
    engine = NativeReview(Path(os.environ["ASCENT_ENGINE_LIBRARY"]))
    required = REQUIREMENTS + (("record-cpu-improvement",) if record_study is not None else ())
    result = review(engine, root, tuple(observations), required=required, dependencies=tuple(dependencies))
    output = root / ".data/parser-evidence/engine.json"
    output.write_text(json.dumps(result, indent=2) + "\n")
    rows = result["review"]["result"]["relations"]
    print(json.dumps({"established": rows["Established"], "missing": rows["Missing"],
                      "executionAuthorized": result["review"]["executionAuthorized"]}, indent=2))
    if rows["Missing"] or any(item.outcome != "passed" for item in observations):
        raise SystemExit(1)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--record-study", help="repository-relative independent CPU receipt directory")
    args = parser.parse_args()
    collect(Path(__file__).resolve().parents[2], args.record_study)
