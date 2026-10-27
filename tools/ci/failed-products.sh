#!/usr/bin/env bash
# Preserve failed native outputs for diagnosis; this does not issue approval.
set -euo pipefail
archive=${1:?expected failure archive path}
prefix="${GERBIL_PATH:-$PWD/.gerbil}"
mkdir -p "$(dirname "$archive")"
{
  printf 'FAILED-PRODUCTS admission=none status=%s\n' "${2:?expected failed command status}"
  printf 'source=%s\nprefix=%s\nworkers=%s\nruntime=%s\nsdk-sha256=%s\n' \
    "$(git rev-parse HEAD)" "$prefix" "${GERBIL_BUILD_CORES:-unset}" "${GAMBOPT:-unset}" "${GERBIL_SDK_SHA256:-unavailable}"
  git status --short
  for source in "$prefix"/pkg/github.com/*/*; do
    if [ -e "$source/.git" ]; then
      printf 'package=%s revision=%s\n' "$source" "$(git -C "$source" rev-parse HEAD)"
      git -C "$source" status --short
      git -C "$source" submodule status --recursive
    fi
  done
} > "$archive.manifest.log"
members=()
for name in lib bin test-lib pkg/TAGS pkg/tags; do
  if [ -e "$prefix/$name" ]; then members+=("$name"); fi
done
test "${#members[@]}" -gt 0
# Exclude source checkouts/.git credentials and SDK files. Native tar retains
# object bytes/modes/times (and platform metadata where supported).
# This is custody after failure, not an atomic publication or load environment.
tar -czf "$archive" -C "$prefix" "${members[@]}"
shasum -a 256 "$archive" >> "$archive.manifest.log"
echo "FAILED-PRODUCTS-SAVED admission=none $archive"
