"""Source ownership for model observations; semantics stay in Scheme/Lean/Quint.

Owners are discovered by responsibility, so adding a module or fixture does not
require editing parallel path lists. Shared owners are conservative source
bindings, not proofs that either model covers every discovered implementation.
"""

SHARED_OWNERS = (
    "src/**/*.ss",
    "build*.ss",
    "dev/**/*.ss",
    ".github/actions/**/*.yml",
    "language-support/*.ss",
    "t/**/*.ss",
    "t/fixtures/**/*.ssi",
    "scripts/tests/*.ss",
    "formal/evidence/*.py",
    "docs/research/parser-engine-formal-model/*.org",
)

SHARED_INPUTS = (
    "gerbil.pkg",
    "dev/gerbil.pkg",
    "init.ss",
    "formal/ParserEngine/NativeFacts.lean",
    "src/schema.ss",
    "src/declarations.ss",
    "language-test-support.ss",
    "t/research-main.ss",
    "build-language-conformance.ss",
    ".github/workflows/ci.yml",
    ".github/workflows/scheme.yml",
    "docs/research/dense-lr-item-space.org",
    "docs/research/parser-engine-formal-model.org",
    "docs/research/dsl-test-preparation.org",
    "docs/research/lr-request-model-foundation.org",
    "docs/research/capture-fold-cost-model.org",
    "docs/research/scheme-dsl-author-interface.org",
    "formal/evidence/pyproject.toml",
    "formal/evidence/uv.lock",
)

LEAN_INPUTS = (
    "formal/lake-manifest.json",
    "formal/lakefile.toml",
    "formal/lean-toolchain",
)

QUINT_INPUTS = (
    "formal/quint/node_modules/@informalsystems/quint/package.json",
    "formal/quint/package-lock.json",
    "formal/quint/package.json",
)
