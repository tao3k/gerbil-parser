#!/usr/bin/env bash
# Preserve compiler output and keep the C compiler in the bounded process group.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
command=(gxi
  -e '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-compiled-interfaces!)'
  -e '(load "scripts/preparation-cache.ss") (prepare-conformance!) (exit 0)')
exec "${command[@]}"
