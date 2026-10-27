#!/usr/bin/env bash
# Run-scoped compiled products retain paths, symlinks, permissions and mtimes.
set -euo pipefail
operation=${1:?expected pack or restore}
archive=${2:?expected archive path}
# Failed products are evidence only: they never contain the restore identity.
if [ "$operation" = failure ]; then
  prefix="${GERBIL_PATH:-$PWD/.gerbil}"
  mkdir -p "$(dirname "$archive")"
  {
    printf 'FAILED-PRODUCTS admission=none status=%s\n' "${3:?expected failed command status}"
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
  for name in lib bin test-lib; do
    if [ -e "$prefix/$name" ]; then members+=("$name"); fi
  done
  test "${#members[@]}" -gt 0
  # Exclude source checkouts/.git credentials and SDK files. Native tar retains
  # object bytes/modes/times (and platform metadata where supported).
  # This is custody after failure, not an atomic publication or load environment.
  tar -czf "$archive" -C "$prefix" "${members[@]}"
  shasum -a 256 "$archive" >> "$archive.manifest.log"
  echo "FAILED-PRODUCTS-SAVED admission=none $archive"
  exit 0
fi
: "${GERBIL_PREPARED_HEAD:?}" "${GERBIL_SDK_SHA256:?}" "${RUNNER_OS:?}" "${RUNNER_ARCH:?}"
identity() {
  printf '%s\n' "$GERBIL_PREPARED_HEAD" "$GERBIL_SDK_SHA256" "$RUNNER_OS" "$RUNNER_ARCH" "$PWD"
}
case "$operation" in
  pack)
    test "$(git rev-parse HEAD)" = "$GERBIL_PREPARED_HEAD"
    mkdir -p .gerbil/ci
    identity > .gerbil/ci/identity
    tar -czf "$archive" .gerbil
    ;;
  restore)
    test "$(git rev-parse HEAD)" = "$GERBIL_PREPARED_HEAD"
    # Validate ownership before extracting or loading compiled Scheme products.
    identity > "$RUNNER_TEMP/expected-scheme-identity"
    tar -xOf "$archive" .gerbil/ci/identity > "$RUNNER_TEMP/actual-scheme-identity"
    cmp "$RUNNER_TEMP/expected-scheme-identity" "$RUNNER_TEMP/actual-scheme-identity"
    # Publication snapshots bind permissions as well as bytes. The consumer's
    # umask must not silently change the admitted producer generation.
    tar -xpzf "$archive"
    mkdir -p "${GERBIL_PATH:-$PWD/.gerbil}/pkg"
    gerbil pkg dir --add git.cons.io/mighty-gerbils/gerbil-directory
    ;;
  *) echo "unknown prepared-product operation: $operation" >&2; exit 2 ;;
esac
echo "PREPARED-SCHEME-OK $operation"
