"""Check that the rustc progress proxy preserves native process contracts."""

from pathlib import Path
import os
import subprocess
import sys
import tempfile
import unittest

WRAPPER = Path(__file__).resolve().parents[1] / "rustc-progress.py"


class NativeCompilerProxyTest(unittest.TestCase):
    def test_jobserver_fds_stdout_and_failure_status_survive_proxy(self):
        with tempfile.TemporaryDirectory() as directory:
            child = Path(directory) / "compiler.py"
            child.write_text('''import os,sys
from pathlib import Path
os.write(int(os.environ["TEST_JOBSERVER_FD"]), b"jobserver-ok")
Path(sys.argv[sys.argv.index("--out-dir")+1], "libfixture-contract.rmeta").write_bytes(b"native-output")
print('{"native":true}', flush=True)
sys.exit(42)
''')
            read_fd, write_fd = os.pipe()
            try:
                environment = dict(os.environ, TEST_JOBSERVER_FD=str(write_fd))
                result = subprocess.run(
                    [sys.executable, str(WRAPPER), sys.executable, str(child),
                     "--out-dir", directory, "--crate-name", "fixture",
                     "-C", "extra-filename=-contract"],
                    env=environment, pass_fds=(write_fd,), capture_output=True, text=True,
                    timeout=5)
                self.assertEqual(result.returncode, 42)
                self.assertEqual(result.stdout, '{"native":true}\n')
                self.assertEqual(os.read(read_fd, 12), b"jobserver-ok")
                self.assertIn("libfixture-contract.rmeta bytes=13", result.stderr)
            finally:
                os.close(read_fd)
                os.close(write_fd)

    def test_compiler_probe_stdout_is_not_modified(self):
        result = subprocess.run([sys.executable, str(WRAPPER), sys.executable,
                                 "-c", 'print("compiler-probe")'],
                                capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "compiler-probe\n")
        self.assertEqual(result.stderr, "")


if __name__ == "__main__":
    unittest.main()
