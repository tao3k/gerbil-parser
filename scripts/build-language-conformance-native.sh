#!/usr/bin/env bash
# Preserve real compiler output and its exit status through a terminal stream.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
command=(gxi
  -e '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-native-interfaces!)'
  -e '(call-with-native-interface-trace (lambda () (load "build-language-conformance.ss") (eval (quote (compile-static-conformance!)))))'
  -e '(call-with-native-interface-trace (lambda () (load "build-language-conformance-link.ss") (eval (quote (main))))) (exit 0)')
if [[ $(uname -s) == Darwin ]]; then
  exec script -q /dev/null "${command[@]}"
else
  printf -v terminal_command '%q ' "${command[@]}"
  exec script -q -e -c "$terminal_command" /dev/null
fi
