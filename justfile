# Run one native suite with its heap fence established before SDK imports.
gerbil_test_runtime_options := "-:max-heap=1G,debug=q"

# The same SDK helper owner used by CI supplies absolute test-module imports.
# Its publication guard admits the complete resulting helper generation.
build-test-support:
    gerbil env gxi {{ gerbil_test_runtime_options }} build-test-support.ss compile

test-file path: build-qualification build-test-support
    #!/usr/bin/env bash
    set -euo pipefail
    mkdir -p .gerbil/test-results
    output_file="$(mktemp .gerbil/test-results/native.XXXXXX)"
    timeout --signal=TERM --kill-after=5s 120s gerbil env gxi {{ gerbil_test_runtime_options }} :gerbil/tools/gxtest -v 3 "{{ path }}" 2>&1 | tee "$output_file"
    if grep -E 'ERROR (CHECK|CASE|HARNESS|MODULE)|Heap overflow|Stack overflow' "$output_file" >/dev/null; then exit 1; fi
    grep -F 'MODULE-OK ' "$output_file" >/dev/null
    grep -F 'HARNESS-OK' "$output_file" >/dev/null
    grep -Fx 'OK' "$output_file" >/dev/null

set shell := ["sh", "-eu", "-c"]

# Same native gxtest suite values, linked before the bounded runtime gate.
test-runtime-organization:
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gxi build-qualification.ss compile --optimized
    ASP_GERBIL_SCHEME_POLICY=1 GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gxi t/build-runtime-organization-tests.ss
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gxi t/check-runtime-organization.ss

# Public native declaration lowering; test-all discovers this suite once.
test-line-events output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/modules/parser/line-structure-objects.ss src/modules/parser/source-event-scope-types.ss src/modules/parser/source-event-scope-objects.ss src/modules/parser/source-event-scope-funs.ss src/modules/parser/line-event-funs.ss src/modules/parser/list-event-funs.ss src/modules/parser/inline-link-event-funs.ss src/modules/parser/source-boundary-types.ss src/modules/parser/source-boundary-objects.ss src/modules/parser/source-boundary-funs.ss src/modules/parser/interface.ss t/line-event-test.ss t/inline-link-event-test.ss t/source-boundary-test.ss t/source-event-scope-test.ss t/list-event-test.ss
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/line-event-test.ss t/inline-link-event-test.ss t/source-boundary-test.ss t/source-event-scope-test.ss t/list-event-test.ss 2>&1 | tee "{{output}}/line-events-test.log"
    rg --quiet '^HARNESS-OK' "{{output}}/line-events-test.log"
    rg --quiet '^OK$' "{{output}}/line-events-test.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/line-events-test.log"

test-source-priority:
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gerbil test -v 3 t/source-priority-test.ss

build-source-priority output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/modules/parser/source-pattern-funs.ss src/modules/parser/funcs.ss src/modules/parser/interface.ss

build-process-fixture: build-qualification
    mkdir -p .data/process-controls
    ${CC:-cc} -std=c11 -Wall -Wextra -Werror t/fixtures/qualification/process-fixture.c -o .data/process-controls/fixture

# Real initialization/load phases; never substitutes for strict controls.
diagnose-process-startup: build-process-fixture
    gerbil env gxi t/fixtures/qualification/startup-controls.ss "$PWD/.data/process-controls/fixture"

test-process-contracts: build-process-fixture
    gerbil env gxi t/fixtures/qualification/process-controls.ss "$PWD/.data/process-controls/fixture"

build-qualification:
    gerbil env gxi {{ gerbil_test_runtime_options }} build-qualification.ss compile

# Focused native suites; test-all discovers the same suites once.
test-qualification-suites: build-qualification
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gerbil test -v 3 t/qualification-plans-test.ss t/bootstrap-test.ss

# Exercise the downstream admission caller after support-module relocation.
test-process-admission: build-qualification
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gerbil env gxi scripts/tests/process-qualification-test.ss

test-compiler-process:
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gerbil test -v 3 t/compiler-process-test.ss

# Cache admission uses the same macro owner and shared runtime as CI.
test-preparation:
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=3G,debug=q" gerbil env gxi t/fixtures/tla-sany-differential/test-suite.ss --workers 4 t/preparation-cache-test.ss

# Build the shared helper through its compiled module owner before loading source suites.
test-workers output=".data/worker-controls": build-qualification
    gerbil compile -O t/fixtures/tla-sany-differential/worker-control.ss
    gerbil env gxi test-processes.ss workers "{{output}}"

# Compile the suite's own fixtures, not only its import declarations.
build-fold-attribution output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/runtime/scan.ss src/runtime/byte-spans.ss src/compiler/event-list-marker.ss src/runtime/source-lines.ss src/runtime/event-fold-lines.ss src/compiler/event-fold-future.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/runtime/fold-cost.ss t/fold-cost-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-cost-test.ss > "{{output}}/fold-cost-test.log" 2>&1
    rg '^CASE-OK |^HARNESS-OK|^OK$' "{{output}}/fold-cost-test.log"
    rg --quiet '^OK$' "{{output}}/fold-cost-test.log"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/compiler/event-fold-analysis.ss src/compiler/event-fold-byte-set.ss src/compiler/event-fold-byte-dispatch.ss src/compiler/event-fold-zero-state.ss src/compiler/event-fold-consumer-state.ss src/compiler/event-fold-ordinary-run.ss src/compiler/event-fold-shared-probes.ss src/compiler/event-fold-scheme-context.ss src/compiler/event-fold-call-plan.ss src/compiler/event-fold-prefix-family.ss src/compiler/event-fold-scheme-expressions.ss src/compiler/event-fold-scheme-runs.ss src/compiler/event-fold-scheme-statements.ss src/compiler/event-fold-scheme.ss src/compiler/event-fold-scheme-modules.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/fold-zero-state-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-zero-state-test.ss > "{{output}}/fold-zero-state-test.log" 2>&1
    rg '^CASE-OK |^HARNESS-OK|^OK$' "{{output}}/fold-zero-state-test.log"
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-zero-state-test.log')" = 8
    rg --quiet '^OK$' "{{output}}/fold-zero-state-test.log"

test-fold-consumer-state output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/fold-consumer-state-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-consumer-state-test.ss > "{{output}}/fold-consumer-state-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-consumer-state-test.log')" = 5
    rg --quiet '^OK$' "{{output}}/fold-consumer-state-test.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/fold-consumer-state-test.log"

test-fold-tape-tail output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/fold-tape-tail-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-tape-tail-test.ss > "{{output}}/fold-tape-tail-test.log" 2>&1
    rg '^CASE-OK |^HARNESS-OK|^OK$' "{{output}}/fold-tape-tail-test.log"
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-tape-tail-test.log')" = 2
    rg --quiet '^OK$' "{{output}}/fold-tape-tail-test.log"

inspect-fold-call-sites product:
    gxi t/benchmarks/parser-throughput/call-graph.ss "{{product}}"

inspect-fold-ordinary-effects program load_path:
    GERBIL_LOADPATH="{{load_path}}" gxi t/benchmarks/parser-throughput/ordinary-plan.ss "{{program}}"

inspect-fold-runs program load_path:
    GERBIL_LOADPATH="{{load_path}}" gxi t/benchmarks/parser-throughput/run-plan.ss "{{program}}"

test-fold-runs output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/fold-run-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-run-test.ss > "{{output}}/fold-run-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-run-test.log')" = 13
    rg '^CASE-OK |^HARNESS-OK|^OK$' "{{output}}/fold-run-test.log"
    rg --quiet '^OK$' "{{output}}/fold-run-test.log"

test-fold-statement-scans output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/fold-statement-scans-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-statement-scans-test.ss > "{{output}}/fold-statement-scans-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-statement-scans-test.log')" = 5
    rg '^CASE-OK |^HARNESS-OK|^OK$' "{{output}}/fold-statement-scans-test.log"
    rg --quiet '^OK$' "{{output}}/fold-statement-scans-test.log"

# Rebuild package dependencies only; installed SDK and global libraries stay read-only.
test-fold-buffer-consumer output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/runtime/byte-spans.ss src/runtime/source-lines.ss src/runtime/fold-tape.ss t/event-strategy-fixture.ss t/fold-tape-test.ss t/fold-buffer-consumer-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-tape-test.ss t/fold-buffer-consumer-test.ss > "{{output}}/fold-buffer-consumer-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-buffer-consumer-test.log')" = 8
    rg '^CASE-OK |^HARNESS-OK|^OK$' "{{output}}/fold-buffer-consumer-test.log"
    rg --quiet '^OK$' "{{output}}/fold-buffer-consumer-test.log"

prepare-fold-buffer-compiler output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/runtime/byte-spans.ss src/compiler/event-fold-scheme-expressions.ss src/compiler/event-fold-scheme-runs.ss src/compiler/event-fold-scheme-statements.ss src/compiler/event-fold-scheme.ss src/compiler/event-fold-scheme-modules.ss

prepare-fold-dependency source output load_path:
    mkdir -p "{{output}}"
    cd "{{source}}" && GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil env gxi build.ss compile --optimized

prepare-real-fold-inputs output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/compiler/event-fold-state-frame.ss src/compiler/event-fold-program.ss src/compiler/event-fold-runtime.ss src/compiler/event-fold-ir.ss src/runtime/fold-tape.ss t/fold-tape-oracle.ss src/compiler/event-fold-scheme-build.ss

test-fold-inert output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/fold-inert-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-inert-test.ss > "{{output}}/fold-inert-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-inert-test.log')" = 4
    rg '^CASE-OK |^HARNESS-OK|^OK$' "{{output}}/fold-inert-test.log"
    rg --quiet '^OK$' "{{output}}/fold-inert-test.log"

link-fold-attribution output load_path:
    gxi t/benchmarks/parser-throughput/link-locks.ss "{{output}}/lib/static" "{{output}}/../lib/static"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/fold-profile" t/benchmarks/parser-throughput/profile.ss

run-fold-prepared-counts binary corpus digest receipt:
    test ! -e "{{receipt}}"
    GAMBOPT=max-heap=1G,debug=q "{{binary}}" "{{corpus}}" prepared-counts "{{digest}}" > "{{receipt}}" 2>&1
    rg --quiet '^DIAGNOSTIC-OK real-fold; NOT PERFORMANCE ACCEPTANCE' "{{receipt}}"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{receipt}}"
    rg '^\(diagnostic-only|^FIELDS|^DIAGNOSTIC-OK' "{{receipt}}"

link-real-fold-throughput output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/benchmarks/parser-throughput/protocol.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/real-fold" t/benchmarks/parser-throughput/real.ss

link-real-fold-tape-throughput output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/benchmarks/parser-throughput/protocol.ss t/fold-tape-oracle.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/real-tape-fold" t/benchmarks/parser-throughput/real-tape.ss

# Independent interpreter preparation is separated from the native measurement.
prepare-real-fold-tape-preflight fixtures program digest corpus load_path:
    test ! -e "{{corpus}}"
    GERBIL_LOADPATH="{{load_path}}" gxi t/benchmarks/parser-throughput/prepare-tape-preflight.ss "{{fixtures}}" "{{program}}" "{{digest}}" "{{corpus}}"

prepare-fold-throughput-protocol output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/benchmarks/parser-throughput/protocol.ss

link-prepared-fold-tape-preflight output load_path:
    mkdir -p "{{output}}"
    gxi t/benchmarks/parser-throughput/link-locks.ss "{{output}}/lib/static" "{{output}}/../lib/static"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/benchmarks/parser-throughput/protocol.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/prepared-tape-fold" t/benchmarks/parser-throughput/prepared-tape.ss

link-fold-stalls output load_path:
    mkdir -p "{{output}}"
    gxi t/benchmarks/parser-throughput/link-locks.ss "{{output}}/lib/static" "{{output}}/../lib/static"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/fold-stalls" t/benchmarks/parser-throughput/stalls.ss

link-fold-allocation-diagnostic output load_path:
    mkdir -p "{{output}}"
    gxi t/benchmarks/parser-throughput/link-locks.ss "{{output}}/lib/static" "{{output}}/../lib/static"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/allocation-diagnostic" t/benchmarks/parser-throughput/allocation.ss

run-fold-stalls binary corpus receipt:
    test ! -e "{{receipt}}"
    GAMBOPT=max-heap=1G,debug=q "{{binary}}" "{{corpus}}" > "{{receipt}}" 2>&1
    rg --quiet '^FOLD-STALLS-OK' "{{receipt}}"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{receipt}}"
    rg '^BATCH-DIAGNOSTIC|^FIELDS|^\(|^FOLD-STALLS-OK' "{{receipt}}"

# One existing AOT process; OS sampling is diagnostic, never acceptance.
[script]
profile-fold-stacks binary corpus receipt sample:
    #!/usr/bin/env bash
    set -euo pipefail
    test ! -e "{{receipt}}"
    test ! -e "{{sample}}"
    GAMBOPT=max-heap=1G,debug=q "{{binary}}" "{{corpus}}" profile > "{{receipt}}" 2>&1 &
    task_pid=$!
    trap 'kill "$task_pid" 2>/dev/null || true' EXIT
    ready=false
    for attempt in {1..200}; do
        if rg --quiet '^FOLD-PROFILE-READY' "{{receipt}}"; then ready=true; break; fi
        kill -0 "$task_pid"
        sleep 0.01
    done
    test "$ready" = true
    /usr/bin/sample "$task_pid" 1 1 -file "{{sample}}"
    wait "$task_pid"
    trap - EXIT
    rg --quiet '^FOLD-PROFILE-OK' "{{receipt}}"

run-fold-allocation-diagnostic binary corpus receipt:
    test ! -e "{{receipt}}"
    GAMBOPT=max-heap=1G,debug=q "{{binary}}" "{{corpus}}" > "{{receipt}}" 2>&1
    rg --quiet '^ALLOCATION-DIAGNOSTIC-OK' "{{receipt}}"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{receipt}}"
    rg '^FIELDS|^\([0-2] |^ALLOCATION-DIAGNOSTIC-OK' "{{receipt}}"

run-prepared-fold-tape-preflight binary corpus digest receipt:
    test ! -e "{{receipt}}"
    GAMBOPT=max-heap=1G,debug=q /usr/bin/time {{if os() == "macos" { "-l" } else { "-v" }}} "{{binary}}" "{{corpus}}" "{{digest}}" > "{{receipt}}" 2>&1
    rg 'parser-throughput.v1|PARSER-THROUGHPUT-PREFLIGHT-OK' "{{receipt}}"

# Admit an already linked actual-Org product; never rebuild it for a path fix.
test-real-fold-controls binary controls receipt:
    test ! -e "{{receipt}}"
    GAMBOPT=max-heap=1G,debug=q "{{binary}}" "{{controls}}" > "{{receipt}}" 2>&1
    test "$(rg --count '^CASE-OK native-org$' '{{receipt}}')" = 24
    rg --quiet '^OK$' "{{receipt}}"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{receipt}}"

run-real-fold-throughput binary fixtures program digest receipt mode="preflight":
    test ! -e "{{receipt}}"
    GAMBOPT=max-heap=1G,debug=q "{{binary}}" "{{fixtures}}" "{{program}}" "{{digest}}" "{{mode}}" > "{{receipt}}" 2>&1
    rg '^PARSER-THROUGHPUT' "{{receipt}}"

# Process RSS includes the retained independent oracle and input corpus.
# Preserve the failed preflight exit code; resource output is not admission.
run-real-fold-resources binary fixtures program digest receipt:
    test ! -e "{{receipt}}"
    GAMBOPT=max-heap=1G,debug=q /usr/bin/time {{if os() == "macos" { "-l" } else { "-v" }}} "{{binary}}" "{{fixtures}}" "{{program}}" "{{digest}}" preflight > "{{receipt}}" 2>&1
    rg '^PARSER-THROUGHPUT' "{{receipt}}"

run-real-fold-tape-resources binary fixtures program digest receipt consumer="direct":
    test ! -e "{{receipt}}"
    GAMBOPT=max-heap=1G,debug=q /usr/bin/time {{if os() == "macos" { "-l" } else { "-v" }}} "{{binary}}" "{{fixtures}}" "{{program}}" "{{digest}}" preflight "{{consumer}}" > "{{receipt}}" 2>&1
    rg '^PARSER-THROUGHPUT' "{{receipt}}"


run-fold-attribution binary controls receipt mode="timed":
    test ! -e "{{receipt}}"
    GAMBOPT=max-heap=1G,debug=q /usr/bin/time {{if os() == "macos" { "-l" } else { "-v" }}} "{{binary}}" "{{controls}}" "{{mode}}" > "{{receipt}}" 2>&1
    rg '^\(diagnostic-only|^DIAGNOSTIC-OK|maximum resident|Maximum resident' "{{receipt}}"

build-event-fold-controls output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/runtime/fold-tape.ss t/fold-tape-oracle.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/compiler/event-fold-analysis.ss src/compiler/event-fold-byte-dispatch.ss src/compiler/event-fold-zero-state.ss src/compiler/event-fold-consumer-state.ss src/compiler/event-fold-ordinary-run.ss src/compiler/event-fold-shared-probes.ss t/event-fold-analysis-test.ss t/fold-zero-state-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-zero-state-test.ss > "{{output}}/fold-zero-state-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-zero-state-test.log')" = 8
    rg --quiet '^OK$' "{{output}}/fold-zero-state-test.log"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/event-fold-analysis-test.ss > "{{output}}/fold-analysis-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-analysis-test.log')" = 3
    rg --quiet '^HARNESS-OK' "{{output}}/fold-analysis-test.log"
    rg --quiet '^OK$' "{{output}}/fold-analysis-test.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/fold-analysis-test.log"
    just test-fold-byte-dispatch "{{output}}" "{{load_path}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/fold-shared-probes-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-shared-probes-test.ss > "{{output}}/fold-shared-probes-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-shared-probes-test.log')" = 2
    rg --quiet '^OK$' "{{output}}/fold-shared-probes-test.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/fold-shared-probes-test.log"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/compiler/event-fold-future.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/runtime/byte-spans.ss src/runtime/source-lines.ss src/runtime/event-fold-lines.ss src/compiler/event-strategy-aot.ss src/compiler/event-fold-program.ss src/compiler/event-fold-runtime.ss src/compiler/event-fold-ir.ss src/compiler/event-fold-scheme-context.ss src/compiler/event-fold-call-plan.ss src/compiler/event-fold-prefix-family.ss src/compiler/event-fold-scheme-expressions.ss src/compiler/event-fold-scheme-statements.ss src/compiler/event-fold-scheme.ss t/event-strategy-fixture.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/fold-tape-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-tape-test.ss > "{{output}}/fold-tape-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-tape-test.log')" = 3
    rg --quiet '^OK$' "{{output}}/fold-tape-test.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/fold-tape-test.log"
    just generate-event-fold-executable "{{output}}" "{{load_path}}"

# One owner: any failed compile or semantic gate stops subsequent steps.
test-fold-byte-dispatch output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/fold-byte-dispatch-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-byte-dispatch-test.ss > "{{output}}/fold-byte-dispatch-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-byte-dispatch-test.log')" = 2
    rg --quiet '^OK$' "{{output}}/fold-byte-dispatch-test.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/fold-byte-dispatch-test.log"

qualify-fold-plan output load_path:
    just test-aot-link-locks
    just build-event-fold-controls "{{output}}" "{{load_path}}"
    just test-fold-consumer-state "{{output}}" "{{load_path}}"
    just test-fold-condition "{{output}}" "{{load_path}}"
    just test-fold-call-environment "{{output}}" "{{load_path}}"
    just test-fold-call-plan "{{output}}" "{{load_path}}"
    just test-fold-frame-facts "{{output}}" "{{load_path}}"
    just test-fold-statement-scans "{{output}}" "{{load_path}}"
    just test-fold-tape-tail "{{output}}" "{{load_path}}"
    just test-fold-inert "{{output}}" "{{load_path}}"
    just test-fold-runs "{{output}}" "{{load_path}}"
    just test-fold-source-controls "{{output}}" "{{load_path}}"
    just link-event-fold-controls "{{output}}" "{{load_path}}"
    just test-event-fold-controls "{{output}}" "{{load_path}}"

# Copy immutable generated artifacts, not dependency manifests or SDK state.
test-fold-call-plan output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/compiler/event-fold-call-plan.ss t/fold-call-plan-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-call-plan-test.ss > "{{output}}/fold-call-plan-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-call-plan-test.log')" = 6
    rg --quiet '^OK$' "{{output}}/fold-call-plan-test.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/fold-call-plan-test.log"

# Copy immutable generated artifacts, not dependency manifests or SDK state.
stage-real-fold-evidence source output:
    test ! -e "{{output}}"
    mkdir -p "{{output}}"
    cp "{{source}}/org-fold" "{{source}}/org-fold-controls.ss" "{{source}}/fold-input-program.ss" "{{output}}/"
    cp "{{source}}"/parse-org-compiled-events*.ss "{{output}}/"

qualify-fold-plan-and-real output load_path actual:
    just qualify-fold-plan "{{output}}" "{{load_path}}"
    just test-real-fold-controls "{{actual}}/org-fold" "{{actual}}/org-fold-controls.ss" "{{output}}/actual-org-aot.log"

generate-event-fold-executable output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil env gxi t/generate-event-fold-executable.ss "{{output}}/event-fold-generated.ss"
    ! rg --quiet 'fold-statements|fold-predicate|fold-offset |fold-uint |run-event-fold|eval ' "{{output}}/event-fold-generated.ss"

# Native std/test caller for the source-owned Fold, scanner and LR suites.
test-fold-source-controls output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/compiler/event-fold-scheme-modules.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/event-strategy-fixture.ss t/event-fold-scheme-test.ss
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/event-fold-scheme-test.ss > "{{output}}/fold-source-controls.log" 2>&1
    rg '^CASE-OK |^OK$' "{{output}}/fold-source-controls.log"
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-source-controls.log')" = 6
    rg --quiet '^HARNESS-OK' "{{output}}/fold-source-controls.log"
    rg --quiet '^OK$' "{{output}}/fold-source-controls.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/fold-source-controls.log"

build-engine-controls output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/runtime/scan.ss src/compiler/event-list-marker.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/runtime/source-lines.ss src/compiler/event-fold-future.ss src/compiler/event-fold-scheme-expressions.ss src/compiler/event-fold-scheme.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/event-strategy-fixture.ss t/event-fold-scheme-test.ss t/ranked-regular-scanner-test.ss t/parser-runtime-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/engine-controls" t/engine-controls.ss

test-engine-controls output:
    GAMBOPT=max-heap=1G,debug=q "{{output}}/engine-controls" > "{{output}}/engine-controls.log" 2>&1
    rg '^CASE-OK |^OK$' "{{output}}/engine-controls.log"
    test "$(rg --count '^MODULE-OK ' '{{output}}/engine-controls.log')" = 3
    test "$(rg --count '^CASE-OK ' '{{output}}/engine-controls.log')" = 22
    rg --quiet '^OK$' "{{output}}/engine-controls.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/engine-controls.log"

build-event-fold-modules output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/compiler/event-fold-scheme-modules.ss src/compiler/event-fold-scheme-build.ss

# Native engine-only frame costs; compilation is outside measured requests.
build-parser-throughput output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/runtime/parse-cost.ss t/benchmarks/parser-throughput/protocol.ss t/parser-throughput-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil env gxi t/benchmarks/parser-throughput/generate.ss "{{output}}/parser-throughput.ss"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/parser-throughput" "{{output}}/parser-throughput.ss"

test-parser-throughput-protocol output load_path:
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/parser-throughput-test.ss > "{{output}}/parser-throughput-test.log" 2>&1
    rg '^CASE-OK |^OK$' "{{output}}/parser-throughput-test.log"
    test "$(rg --count '^CASE-OK ' '{{output}}/parser-throughput-test.log')" = 4
    rg --quiet '^HARNESS-OK' "{{output}}/parser-throughput-test.log"
    rg --quiet '^OK$' "{{output}}/parser-throughput-test.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/parser-throughput-test.log"

benchmark-parser-throughput binary mode="preflight" receipt="":
    test ! -e "{{if receipt == "" { binary + "." + mode + ".log" } else { receipt }}}"
    GAMBOPT=max-heap=1G,debug=q "{{binary}}" "{{mode}}" > "{{if receipt == "" { binary + "." + mode + ".log" } else { receipt }}}" 2>&1
    rg '^PARSER-THROUGHPUT-' "{{if receipt == "" { binary + "." + mode + ".log" } else { receipt }}}"

report-parser-throughput receipt:
    gerbil env gxi t/benchmarks/parser-throughput/report.ss "{{receipt}}"

build-event-fold-frame-benchmark output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/event-fold-frame-benchmark" t/benchmarks/event-fold-frames/benchmark.ss

generate-prefix-family-benchmark output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil env gxi t/benchmarks/prefix-families/generate.ss "{{output}}/prefix-family-benchmark.ss"

benchmark-prefix-families binary samples="8":
    GAMBOPT=max-heap=1G,debug=q "{{binary}}" "{{samples}}" 1000 2000 10000

benchmark-event-fold-frames binary samples="7":
    GAMBOPT=max-heap=1G,debug=q "{{binary}}" "{{samples}}" 1000 2000 10000

link-event-fold-controls output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/compiler/event-list-marker.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/event-fold-controls" "{{output}}/event-fold-generated.ss"

test-event-fold-controls output load_path:
    GAMBOPT=max-heap=1G,debug=q GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" "{{output}}/event-fold-controls" > "{{output}}/event-fold-controls.log" 2>&1
    rg '^CASE-OK |^OK$' "{{output}}/event-fold-controls.log"
    test "$(rg --count '^CASE-OK ' '{{output}}/event-fold-controls.log')" = 100
    test "$(rg --count '^CASE-TAPE-OK ' '{{output}}/event-fold-controls.log')" = 90
    rg --quiet '^OK$' "{{output}}/event-fold-controls.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/event-fold-controls.log"

build-event-fold-compiled output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/runtime/byte-spans.ss src/runtime/source-lines.ss src/runtime/event-fold-lines.ss src/compiler/event-strategy-aot.ss src/compiler/event-fold-state-frame.ss src/compiler/event-fold-program.ss src/compiler/event-fold-runtime.ss src/compiler/event-fold-aot.ss rust-runtime-event-support.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/compiler/event-fold-ir.ss src/compiler/event-fold-analysis.ss src/compiler/event-fold-byte-dispatch.ss src/compiler/event-fold-zero-state.ss src/compiler/event-fold-consumer-state.ss src/compiler/event-fold-ordinary-run.ss src/compiler/event-fold-shared-probes.ss src/compiler/event-fold-scheme-context.ss src/compiler/event-fold-call-plan.ss src/compiler/event-fold-prefix-family.ss src/compiler/event-fold-scheme-expressions.ss src/compiler/event-fold-scheme-runs.ss src/compiler/event-fold-scheme-statements.ss src/compiler/event-fold-scheme.ss src/compiler/event-fold-scheme-modules.ss t/event-strategy-fixture.ss t/event-fold-fixture.ss t/event-fold-test.ss t/event-fold-scheme-test.ss

test-event-fold-compiled binary output: build-qualification
    gerbil env gxi qualify.ss compiled-event-fold "{{binary}}" "{{output}}"

# Fast lexical algorithm loop; keep the POO Flow Case heap fence pre-import.
test-lexer:
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gerbil test -v 3 t/ranked-regular-scanner-test.ss

# LR mode admission is checked separately from the inner scanner transition.
test-lexical-mode:
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gerbil test -v 3 t/grammar-composition-contract-test.ss t/grammar-composition-execution-test.ss t/grammar-composition-lowering-test.ss

# Full qualification after the focused algorithm loop.
test-build-product-contract output load_path:
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 3 t/build-product-contract-test.ss

test-all: build-qualification
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gerbil test -v 3 t/... languages/...

# All contracts execute from current source; every suite has a hard deadline.
test-performance-before-compile: build-qualification
    gerbil env gxi qualify.ss performance

regenerate-language-abi-fixtures: build-qualification
    GERBIL_LOADPATH="$PWD/.gerbil/lib:$PWD/.." gerbil env gxi qualify.ss run --timeout 90 --idle-timeout 5 --log .data/regenerate-language-abi-fixtures.log --require LANGUAGE-ABI-FIXTURES-GENERATED -- gxi \
      -e '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-compiled-interfaces!) (preload-test-imports "t/generate-language-abi-alignment.ss")' \
      -e '(import (only-in :gerbil-parser/t/generate-language-abi-alignment write-language-abi-fixtures!)) (write-language-abi-fixtures! "rust/gerbil-parser-artifact/tests/fixtures/languages.json")' \
      -e '(import (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!)) (test-child-process-exit! 0)'

build-test-driver:
    gerbil env gxi build-test-driver.ss compile

build-ffi-tests:
    gerbil env gxi build-ffi-tests.ss compile

test-language-abi-alignment: build-qualification build-test-driver build-ffi-tests
    mkdir -p .data
    gerbil env gxi qualify.ss run --timeout 180 --idle-timeout 5 --log .data/language-abi-alignment-test.log --require '^OK$' -- gxi -e '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-compiled-interfaces!) (run-observed-sdk-tests! "-v" "5" "t/language-abi-alignment-test.ss")'
    test "$(grep -c '^NATIVE-ALIGNMENT-CASE ' .data/language-abi-alignment-test.log)" = 20
    grep -q '^CASE-OK accepted and rejected language products match the Rust fixture exactly$' .data/language-abi-alignment-test.log
    grep -q '^OK$' .data/language-abi-alignment-test.log
    ! grep -Eq 'ERROR|FAILED|FAILURE' .data/language-abi-alignment-test.log
    grep -E '^CASE-OK |^HARNESS-OK|^OK$' .data/language-abi-alignment-test.log

build-rust-backend-baseline output:
    mkdir -p "{{output}}"
    cargo bench --locked -p gerbil-parser-runtime --bench backend-baseline --no-run --message-format=json > "{{output}}/cargo-build.jsonl"

build-scheme-backend-baseline output:
    mkdir -p "{{output}}"
    gerbil compile -O t/benchmarks/parser-throughput/protocol.ss
    gerbil compile -O -exe -o "{{output}}/scheme-vm" t/benchmarks/backend-baseline/scheme.ss

run-backend-baseline binary requests receipt:
    test ! -e "{{receipt}}"
    test ! -e "{{receipt}}.resources"
    /usr/bin/time {{if os() == "macos" { "-l" } else { "-v" }}} "{{binary}}" "{{requests}}" 20 > "{{receipt}}" 2> "{{receipt}}.resources"
    grep -q '^BACKEND-BASELINE-COMPLETE$' "{{receipt}}"
    test "$(grep -c '^[(]backend-baseline.v1 ' '{{receipt}}')" = 20

report-backend-baseline directory:
    gxi t/benchmarks/backend-baseline/report.ss "{{directory}}" > "{{directory}}/report.log" 2>&1
    grep -q '^MATCHED-BASELINE-REPORT-OK;' "{{directory}}/report.log"
    ! grep -Eq 'ERROR|FAILED|FAILURE' "{{directory}}/report.log"
    cat "{{directory}}/report.log"

prepare-org-backend-baseline program output:
    mkdir -p "{{output}}"
    gxi t/benchmarks/backend-baseline/prepare-org.ss "{{program}}" "{{output}}"

emit-org-rust-baseline ir output:
    cargo run --locked -p gerbil-parser-build-support --example emit_org_fold -- "{{ir}}" > "{{output}}"

link-org-scheme-baseline output load_path:
    GERBIL_PATH="{{output}}/sdk" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/scheme-vm" t/benchmarks/backend-baseline/org-scheme.ss

link-org-rust-baseline output runtime sha:
    ORG_FOLD_RUST_SOURCE="{{output}}/org-fold.rs" rustc --edition=2024 -C opt-level=3 -C lto=thin -L dependency="{{parent_directory(runtime)}}" --extern gerbil_parser_runtime="{{runtime}}" --extern sha2="{{sha}}" t/benchmarks/backend-baseline/org-driver.rs -o "{{output}}/rust-aot"

run-org-backend-baseline binary requests fixtures grammar receipt:
    test ! -e "{{receipt}}"
    test ! -e "{{receipt}}.resources"
    /usr/bin/time {{if os() == "macos" { "-l" } else { "-v" }}} "{{binary}}" "{{requests}}" 20 "{{fixtures}}" "{{grammar}}" > "{{receipt}}" 2> "{{receipt}}.resources"
    grep -q '^BACKEND-BASELINE-COMPLETE$' "{{receipt}}"
    test "$(grep -c '^[(]backend-baseline.v1 ' '{{receipt}}')" = 20

test-backend-baseline-report directory:
    gxi t/benchmarks/backend-baseline/report-controls.ss "{{directory}}" > "{{directory}}/report-controls.log" 2>&1
    grep -q '^BACKEND-REPORT-CONTROLS-OK$' "{{directory}}/report-controls.log"
    test "$(grep -c '^REPORT-REJECT-OK ' '{{directory}}/report-controls.log')" = 4

diagnose-org-fold-baseline binary fixtures grammar receipt:
    test ! -e "{{receipt}}"
    /usr/bin/time {{if os() == "macos" { "-l" } else { "-v" }}} "{{binary}}" 1000 20 "{{fixtures}}" "{{grammar}}" diagnostic > "{{receipt}}" 2> "{{receipt}}.resources"
    grep -q '^BACKEND-BASELINE-COMPLETE$' "{{receipt}}"
    test "$(grep -c '^[(]org-fold-diagnostic.v1 ' '{{receipt}}')" = 20

report-org-fold-cost directory:
    gxi t/benchmarks/backend-baseline/org-cost-report.ss "{{directory}}"

# Reports each complete language batch sample without a shell timing wrapper.
benchmark-versioned-matched label="current" samples="20":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/versioned-languages/matched-batch.ss {{label}} {{samples}}

# Compare long deterministic parses between separate v0.19 package revisions.
benchmark-arithmetic-matched terms="3200" samples="30":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/arithmetic-scale/matched-stream.ss {{terms}} {{samples}}

# Parsed input has one addition term per line; the parser is already built.
benchmark-arithmetic-lines lines="1024" samples="30":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/arithmetic-scale/matched-stream.ss {{lines}} {{samples}} lines

# Prepared-token stages show where CPU work goes; they are not streaming totals.
benchmark-arithmetic-stages terms="3200":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/arithmetic-scale/benchmark.ss {{terms}}

# Complete LR construction at a scale where partition costs are visible.
benchmark-lr-grammar-construction contexts="256" samples="3":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/lr1-partition/context-scale.ss {{contexts}} {{samples}}

# Complete immutable Examples syntax and lossless artifact receipt.
tla-sany-corpus corpus: build-qualification
    GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" GERBIL_PARSER_LR_TRACE=1 gerbil env gxi qualify.ss run --timeout 600 --idle-timeout 5 --log .data/tla-source.log -- gerbil env gxi -e '(load "t/fixtures/tla-sany-differential/preload.ss") (preload-module "gerbil-parser/languages/tla-plus/sany-candidate")' t/fixtures/tla-sany-differential/corpus.ss {{quote(corpus)}}
# Current-source scanner execution; plan-batch includes one plan and requests bindings.
benchmark-contextual-scanner axes="16" samples="20" words="128" literals="0" phase="scan" requests="1": build-qualification
    GERBIL_LOADPATH=.. GERBIL_PATH=$PWD/.gerbil GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gerbil env gxi qualify.ss run --timeout 120 --idle-timeout 5 --log /private/tmp/parser-contextual-scanner-scale.log --require SCANNER-SCALE-OK -- gxi t/benchmarks/contextual-scanner/matched-scale.ss {{axes}} {{samples}} {{words}} {{literals}} {{quote(phase)}} {{requests}}

# Complete contextual ParseArtifact pairs; plan creation is included per batch.
benchmark-contextual-parser requests="16" samples="5": build-qualification
    GERBIL_LOADPATH=.. GERBIL_PATH=$PWD/.gerbil GERBIL_PARSER_LR_TRACE=1 GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gerbil env gxi qualify.ss run --timeout 120 --idle-timeout 5 --log /private/tmp/parser-contextual-parser-scale.log --require CONTEXTUAL-PARSER-SCALE-OK -- gxi -e '(load "t/fixtures/tla-sany-differential/preload.ss") (preload-module "gerbil/tools/gxtest")' t/benchmarks/contextual-scanner/matched-parser.ss {{requests}} {{samples}}

# Complete deferred batches; each sample reports real token and byte coverage.
benchmark-contextual-deferred delimiters="512" samples="5": build-qualification
    GERBIL_LOADPATH=.. GERBIL_PATH=$PWD/.gerbil GAMBOPT="${GAMBOPT:+${GAMBOPT},}max-heap=1G,debug=q" gerbil env gxi qualify.ss run --timeout 120 --idle-timeout 5 --log /private/tmp/parser-contextual-deferred-scale.log --require DEFERRED-SCALE-OK -- gxi t/benchmarks/contextual-scanner/deferred-scale.ss {{delimiters}} {{samples}}

# Host-native toolchain; compiler admission and real test progress stay separate.
test-local suite="rust" gerbil_path=".gerbil": build-qualification
    gerbil env gxi qualify.ss local --suite {{quote(suite)}} --gerbil-path {{quote(gerbil_path)}}

# Pure Rust projection semantics: no Scheme SDK compilation or runtime startup.
[private]
test-graph-projection:
    cargo test --locked -p gerbil-parser-runtime --lib structural_lines_tests::graph_cases -- --list | rg --quiet 'prepared_graph_projection_reuses_admission_across_changing_documents: test'
    cargo test --locked -p gerbil-parser-runtime --lib structural_lines_tests::graph_cases

[private]
test-runtime-library:
    cargo test --locked -p gerbil-parser-runtime
# Real TLA+ grammar through the default LALR route; complete LRSpec stability.
benchmark-lalr-construction samples="20":
    gerbil env gxi -:max-heap=2G,debug=q t/benchmarks/lr1-partition/lalr-construction.ss {{samples}}

# Complete output equality and inverse edits before CPU sampling.
benchmark-incremental-topology sizes="400 800 1600 3200":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/incremental-session/benchmark.ss topology {{sizes}}

# Independent siblings distinguish avoidable suffix replay from expression ancestry.
benchmark-incremental-siblings sizes="100 200 400 800":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/incremental-session/benchmark.ss topology-hcl {{sizes}}

# Capture gate reevaluates productions and compares both forward/inverse artifacts.
benchmark-incremental-capture sizes="2 400 800 1600 3200":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/incremental-session/benchmark.ss topology-capture {{sizes}}

benchmark-incremental-siblings-capture sizes="2 100 200 400 800":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/incremental-session/benchmark.ss topology-hcl-capture {{sizes}}

# Stage controls and uninstrumented public entry; 40 x 100, native-only.
benchmark-gql gerbil_path=".gerbil": build-qualification
    gerbil env gxi qualify.ss local --suite gql-profile --gerbil-path {{quote(gerbil_path)}}

# Optional application-owned actor library; 1/4 clients, 1/4 actors.
benchmark-gql-actors gerbil_path=".gerbil": build-qualification
    gerbil env gxi qualify.ss local --suite gql-actors --gerbil-path {{quote(gerbil_path)}}

# Real C ABI edits, full payload parity, natural GC; phase tracing is separate.
benchmark-source-edits samples="20" lines="128": build-qualification
    gerbil env gxi qualify.ss run --timeout 90 --idle-timeout 5 --log /private/tmp/parser-source-edit-cost.log --require SOURCE-EDIT-BENCHMARK-OK -- gxi -e '(load "t/fixtures/tla-sany-differential/preload.ss") (call-with-compiled-interface-trace (lambda () (eval (quote (import :gerbil-parser/t/benchmarks/source-edits/benchmark))) (eval (quote (main "{{samples}}" "{{lines}}")))))'

# Verification-only model; production parser stays Scheme.
formal:
    cd formal && lake build

# Standalone Fold operation model; no dependency download or runtime build.
formal-fold:
    cd formal && lean ParserEngine/Fold.lean

test-fold-call-environment output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/fold-call-environment-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-call-environment-test.ss > "{{output}}/fold-call-environment-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-call-environment-test.log')" = 4
    rg '^CASE-OK |^HARNESS-OK|^OK$' "{{output}}/fold-call-environment-test.log"
    rg --quiet '^OK$' "{{output}}/fold-call-environment-test.log"

# Materialize the exact manifest pin from existing objects; no SDK/network build.
prepare-locked-lean-dependency source:
    test ! -e formal/.lake/packages/LeanPoo
    git -C "{{source}}" cat-file -e "$(jq -r '.packages[] | select(.name == "LeanPoo") | .rev' formal/lake-manifest.json)^{commit}"
    git clone --no-hardlinks --no-checkout "{{source}}" formal/.lake/packages/LeanPoo
    git -C formal/.lake/packages/LeanPoo remote set-url origin https://github.com/tao3k/lean-poo.git
    git -C formal/.lake/packages/LeanPoo checkout --detach "$(jq -r '.packages[] | select(.name == "LeanPoo") | .rev' formal/lake-manifest.json)"
    test "$(git -C formal/.lake/packages/LeanPoo rev-parse HEAD)" = "$(jq -r '.packages[] | select(.name == "LeanPoo") | .rev' formal/lake-manifest.json)"

formal-run-plan:
    quint test formal/quint/RunPlan.qnt --backend typescript --seed 1 --max-samples 1
    quint run formal/quint/RunPlan.qnt --backend typescript --seed 1 --max-samples 1000 --max-steps 20 --invariant RunSafety

test-fold-condition output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/fold-condition-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-condition-test.ss

test-fold-frame-facts output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O t/fold-frame-facts-test.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/fold-frame-facts-test.ss > "{{output}}/fold-frame-facts-test.log" 2>&1
    test "$(rg --count '^CASE-OK ' '{{output}}/fold-frame-facts-test.log')" = 5
    rg '^CASE-OK |^HARNESS-OK|^OK$' "{{output}}/fold-frame-facts-test.log"
    rg --quiet '^OK$' "{{output}}/fold-frame-facts-test.log"

# Actual consumer declarations; engine owns generated-unit compilation.
prepare-org-fold-candidate consumer output load_path mode="tape-run":
    test ! -e "{{output}}"
    mkdir -p "{{output}}"
    cd "{{consumer}}" && GERBIL_LOADPATH="{{load_path}}" gerbil env gxi t/generate-org-fold.ss "{{output}}/org-fold.ss" "{{output}}/org-fold-controls.ss" gerbil-parser/benchmarks/real-fold "{{mode}}"
    GERBIL_PATH="{{output}}/sdk" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil env gxi "{{output}}/build-fold.ss"
    just link-org-fold-candidate "{{output}}" "{{load_path}}"

# Resume an existing generated closure without regenerating it or racing links.
test-aot-link-locks:
    gxi t/benchmarks/parser-throughput/link-locks-test.ss

link-org-fold-candidate output load_path:
    gxi t/benchmarks/parser-throughput/link-locks.ss "{{output}}/lib/static" "{{output}}/sdk/lib/static"
    GERBIL_PATH="{{output}}/sdk" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/org-fold" "{{output}}/org-fold.ss"

# Native ASCENT is supplied by its upstream build owner, never rebuilt here.
evidence-controls:
    uv run --project formal/evidence --locked python -m unittest discover -s formal/evidence -v

evidence-models:
    uv run --project formal/evidence --locked python formal/evidence/collect_models.py

# Python joins actual Just-owned Fold controls; missing performance stays open.
evidence-fold-controls output load_path:
    uv run --project formal/evidence --locked python formal/evidence/collect_fold_controls.py "{{output}}" "{{load_path}}"

evidence-real-fold-controls output load_path actual:
    uv run --project formal/evidence --locked python formal/evidence/collect_fold_controls.py "{{output}}" "{{load_path}}" --real-product "{{actual}}"

evidence-engine:
    uv run --project formal/evidence --locked python formal/evidence/collect_engine.py

formal-protocol:
    npm --prefix formal/quint test

# SDK-owned module caching, source changes and initializer failure recovery.
test-module-loading: build-qualification
    gerbil env gxi qualify.ss run --timeout 90 --idle-timeout 5 --log .data/module-loading-controls.log --require '^TEST-SUITE-OK modules=1$' -- gxi t/fixtures/tla-sany-differential/test-suite.ss --workers 4 t/module-loading-test.ss

benchmark-module-loading mode="generation": build-qualification
    gerbil env gxi qualify.ss run --timeout 90 --idle-timeout 5 --log .data/module-loading-{{mode}}.log --require '^MODULE-LOADING-CPU-PROOF-OK {{mode}}$' -- gxi t/benchmarks/module-loading/benchmark.ss {{mode}}

prepare-formal-protocol:
    npm --prefix formal/quint ci --offline --ignore-scripts

# Same-owner callbacks cannot overwrite a newer committed Source history.
test-history-commit: build-qualification
    gerbil env gxi qualify.ss run --timeout 90 --idle-timeout 5 --log .data/history-commit-controls.log --require HISTORY-COMMIT-CONTROLS-OK -- gxi scripts/tests/history-commit-test.ss

# Frozen Git source, normal SDK compiler, shared immutable request dependencies.
prepare-capture-reference revision output=".data/selector-input/reference" scope="selector-input":
    mkdir -p {{quote(output)}}/project
    git archive {{quote(revision)}} gerbil.pkg src | tar -x -C {{quote(output)}}/project
    cmp gerbil.pkg {{quote(output)}}/project/gerbil.pkg
    gerbil env gxi t/benchmarks/capture-prefix/build-reference.ss {{quote(output)}}/project/src/runtime/lr-parser/recognition.ss {{quote(output)}}/lib {{quote(scope)}}

benchmark-capture-prefix reference=".data/selector-input/reference/lib" scope="selector-input": build-qualification
    gerbil env gxi qualify.ss run --timeout 90 --idle-timeout 5 --log .data/capture-prefix-cpu.log --require '^CAPTURE-COMPLETE-CPU-PROOF-OK$' -- gxi t/benchmarks/capture-prefix/benchmark.ss {{quote(reference)}} {{quote(scope)}}

# Recorded lexical queries and LR replays diagnose costs; this is not a speedup gate.
diagnose-capture-cost: build-qualification
    gerbil env gerbil compile -O t/benchmarks/capture-prefix/phase-cost.ss
    gerbil env gxi qualify.ss run --timeout 90 --idle-timeout 5 --log .data/capture-phase-cost.log --require '^CAPTURE-COST-REPLAY-OK tokens=256 significant=256$' -- gxi -e '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-compiled-interfaces!) (preload-module "gerbil-parser/t/benchmarks/capture-prefix/phase-cost") (preload-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process") (eval (quote (import :gerbil-parser/t/benchmarks/capture-prefix/phase-cost :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process))) (eval (quote (main))) (eval (quote (test-child-process-exit! 0)))'

# Inclusive diagnosis is separate from the matched complete-request CPU gate.
diagnose-capture-input: build-qualification
    gerbil env gerbil compile -O t/benchmarks/capture-prefix/profile.ss
    gerbil env gxi qualify.ss run --timeout 90 --idle-timeout 5 --log .data/capture-input-stages.log --require SELECTOR-STAGE-DIAGNOSIS-OK -- gxi -e '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-compiled-interfaces!) (preload-module "gerbil-parser/t/benchmarks/capture-prefix/profile") (preload-module "gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process") (eval (quote (import :gerbil-parser/t/benchmarks/capture-prefix/profile :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process))) (eval (quote (main))) (eval (quote (test-child-process-exit! 0)))'

# The production package has no ASP dependency; tools are explicitly installed.
prepare-dev:
    cd dev && GERBIL_PATH="${GERBIL_PATH:-$PWD/../.gerbil}" gerbil deps --install

check-policy:
    timeout 240 gerbil env gxi dev/build.ss compile
