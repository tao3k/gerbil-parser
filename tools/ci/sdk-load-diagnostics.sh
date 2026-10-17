#!/usr/bin/env bash
# Preserve loader failure evidence without changing or rebuilding the SDK.
set -euo pipefail
: "${GERBIL_HOME:?}" "${RUNNER_TEMP:?}"
object="$GERBIL_HOME/lib/std/io/interface/base~1.o1"
file "$object"
stat -f 'inode=%i size=%z modified=%m' "$object"
shasum -a 256 "$object"
# Verification failures are evidence: keep collecting while returning their code.
signature_status=0
codesign --verify --strict --verbose=4 "$object" || signature_status=$?
codesign --display --verbose=4 "$object" 2>&1
archive="$RUNNER_TEMP/gerbil-sdk.tar.gz"
member=$(tar -tzf "$archive" | awk '/\/lib\/std\/io\/interface\/base~1\.o1$/')
test -n "$member"
test "$(printf '%s\n' "$member" | wc -l | tr -d ' ')" = 1
tar -xOzf "$archive" "$member" > "$RUNNER_TEMP/sdk-loader-archive-object"
cmp "$object" "$RUNNER_TEMP/sdk-loader-archive-object"
echo 'SDK-LOADER-ARCHIVE-BYTES-MATCH'
# A bounded system-log query can distinguish mapping/signature rejection from
# a missing file. Failure to read the system log must remain visible.
/usr/bin/log show --last 5m --style compact --predicate \
  'eventMessage CONTAINS "base~1.o1" OR eventMessage CONTAINS "CODE SIGNING"' \
  | tail -80
exit "$signature_status"
