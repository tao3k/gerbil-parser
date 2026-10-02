#!/usr/bin/env python3
"""Compare bounded TLA+ candidate expression trees with pinned SANY 1.7.4."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import xml.etree.ElementTree as ET

JAR_SHA256 = "936a262061c914694dfd669a543be24573c45d5aa0ff20a8b96b23d01e050e88"
FIXTURE_DIR = Path(__file__).resolve().parent
CASES = (
    ("TemporalPrecedence", {"P", "Q", "R", "S", "T"}),
    ("ArithmeticPrecedence", {"P", "Q", "R"}),
)
OPERATOR_ALIASES = {"\\land": "/\\", "\\lor": "\\/"}


def checked_run(argv, timeout):
    return subprocess.run(
        argv, cwd=FIXTURE_DIR, capture_output=True, text=True, timeout=timeout,
        check=False, env=os.environ.copy(),
    )


def sany_shapes(xml_text, module_name):
    root = ET.fromstring(xml_text)
    if root.tag != "modules" or root.findtext("RootModule") != module_name:
        raise ValueError("SANY XML has the wrong root module")
    context = {}
    for entry in root.findall("./context/entry"):
        uid = entry.findtext("UID")
        nodes = [child for child in entry if child.tag != "UID"]
        if not uid or len(nodes) != 1 or uid in context:
            raise ValueError("SANY XML has malformed context entries")
        context[uid] = nodes[0]
    modules = [
        node for node in context.values()
        if node.tag == "ModuleNode" and node.findtext("uniquename") == module_name
    ]
    if len(modules) != 1:
        raise ValueError("SANY XML is missing the unique root module")
    module = modules[0]

    def expression(node):
        if node.tag != "OpApplNode":
            raise ValueError(f"SANY expression node is {node.tag}, not OpApplNode")
        operator = node.find("operator")
        if operator is None or len(operator) != 1:
            raise ValueError("SANY application has no unique operator")
        reference = operator[0]
        target = context.get(reference.findtext("UID"))
        if target is None:
            raise ValueError("SANY operator reference is unresolved")
        name = target.findtext("uniquename")
        if not name:
            raise ValueError("SANY operator has no name")
        name = OPERATOR_ALIASES.get(name, name)
        operands = node.find("operands")
        if operands is None:
            raise ValueError("SANY application has no operand list")
        children = [expression(child) for child in operands]
        return [name, *children] if children else name

    shapes = {}
    for reference in module.findall("UserDefinedOpKindRef"):
        definition = context.get(reference.findtext("UID"))
        if definition is None or definition.tag != "UserDefinedOpKind":
            raise ValueError("SANY module has a malformed definition reference")
        if definition.findtext("./location/filename") != module_name:
            continue
        name = definition.findtext("uniquename")
        body = definition.find("body")
        if not name or body is None or len(body) != 1 or name in shapes:
            raise ValueError("SANY module has a malformed expression definition")
        shapes[name] = expression(body[0])
    return shapes


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--sany-jar", required=True, type=Path)
    args = parser.parse_args()
    jar = args.sany_jar.resolve()
    if not jar.is_file():
        raise ValueError(f"pinned SANY jar missing: {jar}")
    digest = hashlib.sha256(jar.read_bytes()).hexdigest()
    if digest != JAR_SHA256:
        raise ValueError(f"wrong SANY jar digest: {digest}")
    for module_name, expected_names in CASES:
        path = FIXTURE_DIR / f"{module_name}.tla"
        sany = checked_run(
            ["java", "-cp", str(jar), "tla2sany.xml.XMLExporter", "-o", str(path)],
            timeout=30,
        )
        candidate = checked_run(
            ["gerbil", str(FIXTURE_DIR / "candidate-shape.ss"), str(path)],
            timeout=180,
        )
        if candidate.returncode != 0:
            raise ValueError(f"candidate helper failed on {module_name}: {candidate.stderr}")
        try:
            observed = json.loads(candidate.stdout.strip())
        except json.JSONDecodeError as error:
            raise ValueError(f"candidate emitted no single JSON receipt on {module_name}") from error
        if set(observed) != {"accepted", "shapes"}:
            raise ValueError(f"candidate receipt fields differ on {module_name}: {observed}")
        if observed["accepted"] is not True:
            raise ValueError(f"candidate acceptance differs on {module_name}")
        if sany.returncode != 0:
            raise ValueError(f"SANY rejected {module_name}: {sany.stderr}")
        actual = sany_shapes(sany.stdout, module_name)
        if set(actual) != expected_names:
            raise ValueError(f"SANY definition set differs: {sorted(actual)}")
        if set(observed["shapes"]) != expected_names:
            raise ValueError(f"candidate definition set differs: {sorted(observed['shapes'])}")
        if observed["shapes"] != actual:
            raise ValueError(
                f"structural difference on {module_name}:\n"
                f"SANY {json.dumps(actual, sort_keys=True)}\n"
                f"candidate {json.dumps(observed['shapes'], sort_keys=True)}"
            )
        print(f"SANY-DIFF-OK {module_name}: {len(actual)} expression trees")
    print("SANY-DIFF-OK pinned jar, acceptance, and structure")


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, subprocess.TimeoutExpired, ET.ParseError) as error:
        print(f"SANY-DIFF-ERROR {error}", file=sys.stderr)
        sys.exit(1)
