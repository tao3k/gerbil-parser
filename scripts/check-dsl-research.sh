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
run_suite() {
  run_child "$1" "$GERBIL_PATH/bin/gerbil-parser-conformance" "$1"
}
# Semantic groups share one compiled runtime, with their own 10s fences.
python3 scripts/run-bounded.py --timeout 240 --idle-timeout 5 \
  --log "$log_dir/semantic.log" --require '^CONFORMANCE-SEMANTIC-OK groups=' \
  -- "$GERBIL_PATH/bin/gerbil-parser-conformance" semantic
run_suite concise-runtime
run_suite history
run_suite native-poo-runtime
run_suite provenance
run_suite all-language-benchmark
printf 'DSL-RESEARCH-CLOSURE-OK: all child processes exited 0\n'
