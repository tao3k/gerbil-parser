#!/usr/bin/env bash
# Preserve compiler output and keep the native compiler in the bounded process group.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
command=(gxi
  -e '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-native-interfaces!)'
  -e '(load "scripts/native-preparation-cache.ss") (prepare-native-conformance!) (exit 0)')
exec "${command[@]}"
