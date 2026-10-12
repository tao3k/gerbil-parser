#!/usr/bin/env bash
# Run-scoped compiled products retain paths, symlinks, permissions and mtimes.
set -euo pipefail
operation=${1:?expected pack or restore}
archive=${2:?expected archive path}
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
    tar -xzf "$archive"
    mkdir -p "$HOME/.gerbil/pkg"
    gerbil pkg dir --add git.cons.io/mighty-gerbils/gerbil-directory
    ;;
  *) echo "unknown prepared-product operation: $operation" >&2; exit 2 ;;
esac
echo "PREPARED-SCHEME-OK $operation"
