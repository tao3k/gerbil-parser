#!/usr/bin/env bash
# Reproduce Scheme declaration and native POO controls with bounded child exits.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
log_dir=${1:-"$root/.data/dsl-research"}
mkdir -p "$log_dir"
log_dir=$(cd "$log_dir" && pwd)
runner_dir=$(mktemp -d)
trap 'rm -rf "$runner_dir"' EXIT
export GERBIL_PATH=${GERBIL_PATH:-"$root/.gerbil"}
export GERBIL_LOADPATH=${GERBIL_LOADPATH:-"$(dirname "$root")"}
if command -v gtimeout >/dev/null; then
  timeout_bin=gtimeout
elif command -v timeout >/dev/null; then
  timeout_bin=timeout
else
  printf 'A GNU timeout command is required.\n' >&2
  exit 2
fi
: > "$log_dir/exits.tsv"
run_program() {
  local name=$1 program=$2 status
  printf 'RUN %s\n' "$name"
  set +e
  "$timeout_bin" -k 2 10 "${GXI:-gxi}" "$program" 2>&1 | tee "$log_dir/$name.log"
  local statuses=("${PIPESTATUS[@]}")
  set -e
  status=${statuses[0]}
  if [ "${statuses[1]}" -ne 0 ]; then status=${statuses[1]}; fi
  printf '%s\t%s\n' "$name" "$status" >> "$log_dir/exits.tsv"
  if [ "$status" -ne 0 ]; then return "$status"; fi
}
run_suite() {
  local name=$1 imports=$2 suites=$3
  local program="$runner_dir/$name.ss"
  cat > "$program" <<SS
(import (only-in :std/test/base TestConfig current-test-config VERBOSITY-CASE
                 test-suite! test-result-ok?)
        $imports)
(for-each
 (lambda (suite)
   (let (result
         (parameterize ((current-test-config
                         (TestConfig verbosity: VERBOSITY-CASE capture-output?: #f)))
           (test-suite! suite)))
     (unless (test-result-ok? result) (exit 1))))
 $suites)
(displayln "DSL-SUITE-OK: $name") (force-output) (exit 0)
SS
  run_program "$name" "$program"
}
# Serial execution preserves each unchanged ten-second completion fence.
run_suite diagnostics '(only-in :gerbil-parser/t/language-diagnostics-test language-diagnostics-test)' '(list language-diagnostics-test)'
run_suite surface '(only-in :gerbil-parser/t/language-surface-test language-surface-test)' '(list language-surface-test)'
run_suite composition '(only-in :gerbil-parser/t/grammar-composition-test grammar-composition-test)' '(list grammar-composition-test)'
run_suite publication '(only-in :gerbil-parser/t/language-artifact-test language-artifact-tests)' '(list language-artifact-tests)'
run_suite entry-boundaries '(only-in :gerbil-parser/t/language-entry-boundary-test language-entry-boundary-test)' '(list language-entry-boundary-test)'
run_suite topology '(only-in :gerbil-parser/t/language-topology-test language-topology-test)' '(list language-topology-test)'
run_suite loaders '(only-in :gerbil-parser/t/language-loader-test language-loader-test)
        (only-in :gerbil-parser/t/language-loader-value-test language-loader-value-test)' '(list language-loader-test language-loader-value-test)'
run_suite gql '(only-in :gerbil-parser/t/antlr4-source-test antlr4-source-test)
        (only-in :gerbil-parser/languages/gql/parser-test gql-parser-test)' '(list antlr4-source-test gql-parser-test)'
run_suite gql-profile '(only-in :gerbil-parser/t/gql/benchmark-profile-test gql-benchmark-profile-test)' '(list gql-benchmark-profile-test)'
run_suite pack-metadata '(only-in :gerbil-parser/t/language-pack-metadata-test language-pack-metadata-test)' '(list language-pack-metadata-test)'
run_suite source-services '(only-in :gerbil-parser/t/source-strategy-test source-strategy-test)' '(list source-strategy-test)'
run_suite build-services '(only-in :gerbil-parser/t/build-strategy-test build-strategy-test)' '(list build-strategy-test)'
run_suite concise-package '(only-in :gerbil-parser/t/fixtures/language-pack-research/package-expression-parser-test package-expression-parser-test)' '(list package-expression-parser-test)'
run_suite composed-package '(only-in :gerbil-parser/t/fixtures/language-pack-research/list-parser-test list-parser-test)' '(list list-parser-test)'
run_program concise-runtime t/fixtures/language-pack-research/package-expression-runtime.ss
run_program history t/fixtures/language-pack-research/composition-history.ss
run_program native-poo-runtime t/fixtures/language-pack-research/list-runtime.ss
run_program provenance t/fixtures/language-pack-research/provenance-language.ss
printf 'DSL-RESEARCH-CLOSURE-OK: all child processes exited 0\n'
