#!/usr/bin/env bash
# Preserve loader failure evidence without changing or rebuilding the SDK.
set -euo pipefail
: "${GERBIL_HOME:?}" "${RUNNER_TEMP:?}"
mode=${1:-failure}
case "$mode" in baseline|failure) ;; *) exit 2 ;; esac
object="$GERBIL_HOME/lib/std/io/interface/base~1.o1"
object_directory=$(cd "$(dirname "$object")" && pwd -P)
object_real="$object_directory/$(basename "$object")"
/usr/bin/file "$object"
/usr/bin/stat -f 'inode=%i size=%z modified=%m' "$object"
shasum -a 256 "$object"
# Verification failures are evidence: keep collecting while returning their code.
signature_status=0
/usr/bin/codesign --verify --strict --verbose=4 "$object" || signature_status=$?
display_status=0
/usr/bin/codesign --display --verbose=4 "$object" 2>&1 || display_status=$?
archive="$RUNNER_TEMP/gerbil-sdk.tar.gz"
member=$(tar -tzf "$archive" | awk '/\/lib\/std\/io\/interface\/base~1\.o1$/')
test -n "$member"
test "$(printf '%s\n' "$member" | wc -l | tr -d ' ')" = 1
tar -xOzf "$archive" "$member" > "$RUNNER_TEMP/sdk-loader-archive-object"
archive_status=0
if cmp "$object" "$RUNNER_TEMP/sdk-loader-archive-object"; then
  echo 'SDK-LOADER-ARCHIVE-BYTES-MATCH'
else
  archive_status=$?
fi
# Darwin errno 84 is EOVERFLOW. Retain mapping dimensions and resource
# limits as well as signature evidence; these are distinct failure classes.
/usr/bin/otool -l "$object" | awk '/segname|vmaddr|vmsize|fileoff|filesize|initprot|maxprot/ {print}'
/usr/sbin/sysctl kern.osversion kern.osrelease hw.pagesize
ulimit -a
if [ "$mode" = baseline ]; then
  exit "$((signature_status | display_status | archive_status))"
fi
# A bounded system-log query can distinguish mapping/signature rejection from
# a missing file. Failure to read the system log must remain visible.
/usr/bin/log show --last 5m --style compact --predicate \
  "eventMessage CONTAINS \"$object\" OR eventMessage CONTAINS \"$object_real\"" \
  | tail -80
test "$signature_status" -eq 0 && test "$display_status" -eq 0 && test "$archive_status" -eq 0
