"""Source-bound tool observations; native Scheme ASCENT owns inference.

This host records completed commands, not parser semantics or performance
statistics. Supplied input sets and claim scopes still require human review.
"""
from dataclasses import dataclass
from hashlib import sha256
import json
from pathlib import Path
import subprocess
import shutil


REQUIREMENTS = (
    "lean-model", "quint-protocol", "scheme-conformance", "matched-performance",
)


def digest(value: bytes) -> str:
    return sha256(value).hexdigest()


def canonical(value) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), allow_nan=False).encode()


def source_path(root: Path, relative: str) -> Path:
    name = Path(relative)
    if name.is_absolute() or ".." in name.parts or not relative:
        raise ValueError("repository-relative input required")
    path = root / name
    if not path.resolve().is_relative_to(root.resolve()):
        raise ValueError("input escapes repository")
    return path


def snapshot(root: Path, inputs: tuple[str, ...]) -> tuple[tuple[str, str], ...]:
    if not inputs or len(set(inputs)) != len(inputs):
        raise ValueError("nonempty distinct input set required")
    return tuple((name, digest(source_path(root, name).read_bytes())) for name in sorted(inputs))


def executable_identity(command: str) -> str:
    path = shutil.which(command)
    if path is None:
        raise FileNotFoundError(command)
    return digest(Path(path).resolve().read_bytes())


@dataclass(frozen=True)
class Check:
    claim: str
    argv: tuple[str, ...]
    inputs: tuple[str, ...]
    markers: tuple[str, ...]
    cwd: str = "."
    timeout: int = 120

    def __post_init__(self):
        if self.claim not in (*REQUIREMENTS, "scheme-build") or not self.argv or not self.markers:
            raise ValueError("known claim, command and explicit success markers required")
        if any(not marker for marker in self.markers) or self.timeout <= 0:
            raise ValueError("nonempty markers and positive deadline required")


@dataclass(frozen=True)
class Observation:
    check: Check
    inputs: tuple[tuple[str, str], ...]
    output: bytes
    exit_code: int | None
    outcome: str
    executable: tuple[str, str]

    @property
    def identity(self) -> str:
        return digest(canonical(self.record()))

    def record(self):
        return {"claim": self.check.claim, "argv": self.check.argv,
                "cwd": self.check.cwd, "deadline": self.check.timeout,
                "markers": self.check.markers, "inputs": self.inputs,
                "outputSha256": digest(self.output), "exitCode": self.exit_code,
                "outcome": self.outcome, "executable": self.executable}

    def save(self, root: Path) -> str:
        relative = f".data/parser-evidence/{self.identity}.log"
        path = source_path(root, relative)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(self.output)
        path.with_suffix(".json").write_bytes(canonical(self.record()) + b"\n")
        return relative


def observe(root: Path, check: Check) -> Observation:
    """Run an explicit checker without a shell and retain failures/timeouts.

    This is a direct-child deadline, not a descendant-process supervisor.
    Scheme callers use their existing qualification owner for process control.
    Tool/dependency locks must be included in the supplied input set.
    """
    before = snapshot(root, check.inputs)
    tool = (check.argv[0], executable_identity(check.argv[0]))
    try:
        result = subprocess.run(check.argv, cwd=source_path(root, check.cwd),
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                timeout=check.timeout, check=False)
        output, code = result.stdout, result.returncode
        outcome = "passed" if code == 0 and all(
            marker.encode() in output for marker in check.markers) else "failed"
    except subprocess.TimeoutExpired as failure:
        output, code, outcome = failure.stdout or b"", None, "timeout"
    try:
        unchanged = snapshot(root, check.inputs) == before and executable_identity(check.argv[0]) == tool[1]
    except FileNotFoundError:
        unchanged = False
    if not unchanged:
        outcome = "changed-inputs"
    return Observation(check, before, output, code, outcome, tool)


class NativeReview:
    """Load one source-owner-built library after checking its artifact receipt."""
    def __init__(self, library: Path):
        import ascent_engine
        from ascent_engine import Engine

        sidecar = library.with_name(library.name + ".json")
        metadata = json.loads(sidecar.read_bytes())
        library_digest = digest(library.read_bytes())
        if metadata.get("abi") != 1 or metadata.get("librarySha256") != library_digest:
            raise ValueError("native library receipt mismatch")
        self.runtime = Engine(str(library))
        if digest(library.read_bytes()) != library_digest:
            raise ValueError("native library changed while loading")
        binding = Path(ascent_engine.__file__).parent
        self.identity = {"backend": self.runtime.backend, "librarySha256": library_digest,
                         "buildReceiptSha256": digest(sidecar.read_bytes()),
                         "bindingSources": {path.name: digest(path.read_bytes())
                                            for path in sorted(binding.glob("*.py"))}}


def review(engine, root: Path, observations: tuple[Observation, ...],
           required: tuple[str, ...] = REQUIREMENTS,
           dependencies: tuple[tuple[str, str], ...] = ()):
    """Explain finite evidence gaps. No execution permission or speed assertion.

    Receipt dependencies use content IDs. Missing, stale, circular and failed
    dependencies are derived by upstream EvidenceReview, never Python rules.
    """
    import ascent_engine.review as native_review
    EvidenceReview = native_review.EvidenceReview

    if not required or len(set(required)) != len(required) or any(
            claim not in REQUIREMENTS for claim in required):
        raise ValueError("explicit distinct known requirements required")
    # Transport one canonical input-set commitment per observation. Detailed
    # per-file digests remain in its record. Native inference checks this root
    # exactly as it checks individual inputs; no host relational rules are added.
    current = []
    for item in observations:
        key = "inputs:" + digest(canonical(item.inputs))
        try:
            refreshed = snapshot(root, tuple(name for name, _ in item.inputs))
            row = [key, digest(canonical(refreshed))]
            if row not in current:
                current.append(row)
        except FileNotFoundError:
            pass
    for command in sorted({item.executable[0] for item in observations}):
        try:
            current.append(["tool:" + command, executable_identity(command)])
        except FileNotFoundError:
            pass
    candidate = digest(canonical(current))
    facts = {"Candidate": [[candidate, "parser-engine", candidate]],
             "CurrentInput": current, "Passed": [], "ReceiptInput": [],
             "Requires": [[candidate, claim] for claim in required],
             "ReceiptDepends": [list(edge) for edge in dependencies]}
    for item in observations:
        receipt = item.identity
        log = f".data/parser-evidence/{receipt}.log"
        path = source_path(root, log)
        if path.is_file():
            facts["CurrentInput"].append([log, digest(path.read_bytes())])
        facts["ReceiptInput"].append([receipt, "inputs:" + digest(canonical(item.inputs)),
                                      digest(canonical(item.inputs))])
        facts["ReceiptInput"].append([receipt, log, digest(item.output)])
        metadata = f".data/parser-evidence/{receipt}.json"
        metadata_path = source_path(root, metadata)
        if metadata_path.is_file():
            facts["CurrentInput"].append([metadata, digest(metadata_path.read_bytes())])
        facts["ReceiptInput"].append([receipt, metadata, digest(canonical(item.record()) + b"\n")])
        facts["ReceiptInput"].append([receipt, "tool:" + item.executable[0], item.executable[1]])
        if item.outcome == "passed":
            facts["Passed"].append([receipt, item.check.claim])
    with EvidenceReview(engine.runtime, facts) as evidence:
        result = evidence.review()
    # Serialize a traceable source-fact packet alongside the native derivation.
    return {"reviewEngine": engine.identity,
            "facts": facts, "review": result,
            "observations": [{"id": item.identity, **item.record()} for item in observations],
            "scope": "Finite host observations; not Scheme refinement, exhaustive protocol proof or speedup."}
