#!/usr/bin/env bash
set -euo pipefail
runner=t/fixtures/tla-sany-differential/native-suite.ss
fixtures=t/fixtures/tla-sany-differential
GERBIL_TEST_CORES=2 gxi "$runner" "$fixtures/worker-left.ss" "$fixtures/worker-right.ss"
echo WORKER-CONTROL-OK shared-runtime
GERBIL_TEST_CORES=2 gxi "$runner" "$fixtures/affinity-left.ss" "$fixtures/affinity-right.ss"
echo WORKER-CONTROL-OK runtime-affinity
set +e
GERBIL_TEST_CORES=2 gxi "$runner" "$fixtures/worker-failure.ss"
status=$?
set -e
test "$status" -ne 0
echo WORKER-CONTROL-OK failure-propagation
set +e
GERBIL_TEST_CORES=2 gxi "$runner" "$fixtures/worker-timeout.ss"
status=$?
set -e
test "$status" -eq 70
echo WORKER-CONTROL-OK silence-watchdog
set +e
GERBIL_TEST_CORES=2 gxi "$runner" "$fixtures/worker-left.ss" "$fixtures/worker-left.ss"
status=$?
set -e
test "$status" -ne 0
echo WORKER-CONTROL-OK duplicate-ownership
echo OK
