"""Ensure shared preparation preserves both diagnostic executions and fences."""
import contextlib
import importlib.util
import io
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

SPEC = importlib.util.spec_from_file_location("test_local", Path(__file__).resolve().parents[1] / "test-local.py")
LOCAL = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(LOCAL)

class DiagnosticPlanTests(unittest.TestCase):
    def execute(self, directory, extra=()):
        commands = []
        def run(command, **kwargs):
            commands.append(command)
            return type("Result", (), {"returncode": 0})()
        argv = ["test-local", "--suite", "gql-profile", "--suite", "gql-actors",
                "--suite", "gql-profile", "--gerbil-path", directory,
                "--log-dir", directory] + list(extra)
        with patch.object(sys, "argv", argv), patch.object(LOCAL, "native_environment", return_value={"PATH": "/usr/bin:/bin"}), patch.object(LOCAL.os, "chdir"), patch.object(LOCAL.subprocess, "run", side_effect=run), contextlib.redirect_stdout(io.StringIO()):
            LOCAL.main()
        return commands

    def test_union_build_and_each_diagnostic_once(self):
        with tempfile.TemporaryDirectory() as directory:
            commands = self.execute(directory)
        builds = [c[c.index("--")+1:] for c in commands if "gxc" in c]
        self.assertEqual(len(builds), 1)
        self.assertEqual(len(builds[0][2:]), 4)
        self.assertEqual(len(set(builds[0][2:])), 4)
        executions = [c for c in commands if "--require" in c]
        self.assertEqual(len(executions), 2)
        for c in executions:
            self.assertEqual(c[c.index("--idle-timeout")+1], "5")
            self.assertEqual(c[c.index("--timeout")+1], "120")

    def test_prepared_products_keep_executions(self):
        with tempfile.TemporaryDirectory() as directory:
            base = Path(directory) / "lib/gerbil-parser/t/benchmarks/gql/runtime"
            base.mkdir(parents=True)
            for name in ("reduction-counts", "execution-counts", "matched-stages", "actors"):
                (base / (name + ".o1")).touch()
            commands = self.execute(directory, ["--prepared-diagnostics"])
        self.assertEqual(len(commands), 2)
        self.assertTrue(all("--require" in c for c in commands))

    def test_missing_preparation_fails_before_execution(self):
        with tempfile.TemporaryDirectory() as directory:
            with self.assertRaisesRegex(SystemExit, "missing prepared diagnostic"):
                self.execute(directory, ["--prepared-diagnostics"])
