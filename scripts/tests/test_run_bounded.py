#!/usr/bin/env python3
"""Exercise evidence completeness and failure propagation through the process runner."""

from pathlib import Path
import os
import subprocess
import sys
import tempfile
import time
import unittest

RUNNER = Path(__file__).resolve().parents[1] / "run-bounded.py"


class BoundedEvidenceTests(unittest.TestCase):
    def run_child(self, program, *, options=(), required=()):
        with tempfile.TemporaryDirectory(prefix="parser-bounded-test-") as directory:
            log = Path(directory) / "child.log"
            command = [sys.executable, str(RUNNER), "--timeout", "5", "--log", str(log)]
            command.extend(options)
            for pattern in required:
                command.extend(["--require", pattern])
            command.extend(["--", sys.executable, "-u", "-c", program])
            result = subprocess.run(command, capture_output=True, text=True, timeout=15)
            return result, log.read_text()

    def test_full_receipt_and_unicode_survive_stdout_and_log(self):
        receipt = "receipt " + "λ" * 2000 + " accepted"
        result, log = self.run_child(
            f"print({receipt!r}); print('CASE-OK owner'); print('OK')",
            options=["--full-output"], required=["^CASE-OK ", "^OK$"])
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn(receipt + "\n", result.stdout)
        self.assertIn(receipt + "\n", log)

    def test_backpressure_preserves_queued_evidence_past_idle_interval(self):
        with tempfile.TemporaryDirectory(prefix="parser-bounded-pipe-") as directory:
            log = Path(directory) / "child.log"
            receipt = "PIPE-PROGRESS " + "λ" * 1000
            program = f"print(({receipt!r} + '\\n') * 2000, end=''); print('OK')"
            readfd, writefd = os.pipe()
            os.set_blocking(writefd, False)
            process = subprocess.Popen(
                [sys.executable, str(RUNNER), "--timeout", "5", "--idle-timeout", "0.2",
                 "--log", str(log), "--full-output", "--require", "^OK$", "--",
                 sys.executable, "-u", "-c", program],
                stdout=writefd, stderr=subprocess.PIPE)
            os.close(writefd)
            process.stdout = os.fdopen(readfd, "rb")
            try:
                # Forwarding blocks longer than the idle interval while child bytes
                # remain queued. Console backpressure must not imply child silence.
                time.sleep(0.5)
                stdout, stderr = process.communicate(timeout=10)
            finally:
                if process.poll() is None:
                    process.kill()
                    process.communicate()
            self.assertEqual(process.returncode, 0, stderr.decode())
            self.assertEqual(stdout.decode().count(receipt + "\n"), 2000)
            self.assertEqual(log.read_text().count(receipt + "\n"), 2000)
            self.assertIn(b"reason=completed", stdout)

    def test_zero_exit_without_final_ok_is_rejected(self):
        result, _ = self.run_child("print('CASE-OK owner'); print('HARNESS-OK')",
                                   required=["^CASE-OK ", "^OK$"])
        self.assertEqual(result.returncode, 1)
        self.assertIn("missing=['^OK$']", result.stdout)

    def test_nonzero_exit_cannot_be_hidden_by_ok_markers(self):
        result, _ = self.run_child("print('CASE-OK owner'); print('OK'); raise SystemExit(42)",
                                   required=["^CASE-OK ", "^OK$"])
        self.assertEqual(result.returncode, 1)
        self.assertIn("exit=42", result.stdout)

    def test_error_output_cannot_be_hidden_by_zero_exit(self):
        result, log = self.run_child("print('ERROR swallowed test failure')")
        self.assertEqual(result.returncode, 1)
        self.assertIn("ERROR swallowed test failure", log)
        self.assertIn("reason=error output", result.stdout)

    def test_silent_child_reaches_idle_gate(self):
        result, _ = self.run_child("import time; time.sleep(10)",
                                   options=["--idle-timeout", "0.2"])
        self.assertEqual(result.returncode, 124)
        self.assertIn("reason=output idle timeout", result.stdout)

    def test_timeout_stops_child_in_separate_session(self):
        # Model script(1): the wrapper waits for a child with a new process group.
        with tempfile.TemporaryDirectory(prefix="parser-bounded-session-") as directory:
            marker = Path(directory) / "escaped-child-finished"
            child = ("import time; from pathlib import Path; time.sleep(1.5); "
                     f"Path({str(marker)!r}).touch()")
            program = ("import subprocess, sys; "
                       f"subprocess.run([sys.executable, '-c', {child!r}], "
                       "start_new_session=True)")
            result, _ = self.run_child(program, options=["--timeout", "0.3"])
            self.assertEqual(result.returncode, 124, result.stdout + result.stderr)
            time.sleep(1.5)
            self.assertFalse(marker.exists(), "timed-out child survived its wrapper")

    def test_continuous_output_cannot_bypass_total_gate(self):
        result, _ = self.run_child(
            "import time\nwhile True:\n print('work'); time.sleep(0.01)",
            options=["--timeout", "0.5"])
        self.assertEqual(result.returncode, 124)
        self.assertIn("reason=total timeout", result.stdout)


if __name__ == "__main__":
    unittest.main(verbosity=2)
