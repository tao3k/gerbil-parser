#!/usr/bin/env bash
# Preserve compiler output and keep the native compiler in the bounded process group.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
command=(gxi
  -e '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-native-interfaces!)'
  -e '(call-with-native-interface-trace (lambda () (load "build-language-conformance.ss") (eval (quote (compile-static-conformance!)))))'
  -e '(call-with-native-interface-trace (lambda () (load "build-language-conformance-link.ss") (eval (quote (main))))) (exit 0)')
exec "${command[@]}"
