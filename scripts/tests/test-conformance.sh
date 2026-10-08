#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root"
logs=${1:-.data/conformance-controls}
mkdir -p "$logs"
gxi scripts/tests/conformance-runner-test.ss success > "$logs/success.log" 2>&1
rg -q '^CONFORMANCE-CONTROL-OK success$' "$logs/success.log"
echo CONFORMANCE-CONTROL-OK success
for mode in duplicate failure timeout; do
  set +e
  gxi scripts/tests/conformance-runner-test.ss "$mode" > "$logs/$mode.log" 2>&1
  status=$?
  set -e
  case "$mode" in
    duplicate) test "$status" -eq 70; rg -q 'duplicate conformance Suite' "$logs/$mode.log";;
    failure) test "$status" -eq 1; rg -q 'negative control' "$logs/$mode.log";;
    timeout) test "$status" -eq 70; rg -q '^CONFORMANCE-GROUP-TIMEOUT timeout$' "$logs/$mode.log";;
  esac
  echo "CONFORMANCE-CONTROL-OK $mode"
done
echo OK
