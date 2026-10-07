#!/usr/bin/env bash
# Reproduce Scheme declaration and native POO controls with bounded child exits.
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"
log_dir=${1:-"$root/.data/dsl-research"}
mkdir -p "$log_dir"
log_dir=$(cd "$log_dir" && pwd)
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
run_child() {
  local name=$1 status
  shift
  printf 'RUN %s\n' "$name"
  set +e
  "$timeout_bin" -k 2 10 "$@" 2>&1 | tee "$log_dir/$name.log"
  local statuses=("${PIPESTATUS[@]}")
  set -e
  status=${statuses[0]}
  if [ "${statuses[1]}" -ne 0 ]; then status=${statuses[1]}; fi
  printf '%s\t%s\n' "$name" "$status" >> "$log_dir/exits.tsv"
  if [ "$status" -ne 0 ]; then return "$status"; fi
}
run_program() {
  local name=$1 program=$2
  shift 2
  run_child "$name" "${GXI:-gxi}" \
    -e '(load "t/fixtures/tla-sany-differential/preload.ss") (prefer-native-interfaces!)' \
    "$program" "$@"
}
run_suite() {
  run_child "$1" "$GERBIL_PATH/bin/gerbil-parser-conformance" "$1"
}
# Serial execution preserves each unchanged ten-second completion fence.
run_suite test-style
run_suite diagnostics
run_suite surface
run_suite composition
run_suite publication
run_suite entry-boundaries
run_suite topology
run_suite loaders
run_suite gql
run_suite arithmetic
run_suite bash
run_suite cypher
run_suite fhirpath
run_suite hcl
run_suite hl7
run_suite tla-plus
run_suite gql-profile
run_suite pack-metadata
run_suite source-services
run_suite build-services
run_suite concise-package
run_suite composed-package
run_suite downstream-records
run_program concise-runtime t/fixtures/language-pack-research/package-expression-runtime.ss
run_program history t/fixtures/language-pack-research/composition-history.ss
run_program native-poo-runtime t/fixtures/language-pack-research/list-runtime.ss
run_program provenance t/fixtures/language-pack-research/provenance-language.ss
run_suite all-language-benchmark
printf 'DSL-RESEARCH-CLOSURE-OK: all child processes exited 0\n'
