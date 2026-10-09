#!/usr/bin/env bash
# Preserve compiler output and keep the C compiler in the bounded process group.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
# Report actual import and GC work from the compiler during cold preparation.
command=(gxi
  -e '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-compiled-interfaces!) (gc-report-set! #t)'
  -e '(call-with-compiled-interface-trace (lambda () (load "scripts/preparation-cache.ss"))) (prepare-conformance!) (exit 0)')
exec "${command[@]}"
