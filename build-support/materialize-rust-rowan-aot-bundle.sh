#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 5 ]]; then
  echo "usage: materialize-rust-rowan-aot-bundle.sh" \
    "GENERATOR PROJECT_LIB GERBIL_LIB GRAMMAR.ss OUTPUT" >&2
  exit 64
fi

generator=$1
project_lib=$2
gerbil_lib=$3
grammar=$4
output=$5

for required_file in "$generator" "$grammar"; do
  if [[ ! -f "$required_file" ]]; then
    echo "required file not found: $required_file" >&2
    exit 66
  fi
done

for required_directory in "$project_lib" "$gerbil_lib"; do
  if [[ ! -d "$required_directory" ]]; then
    echo "required directory not found: $required_directory" >&2
    exit 66
  fi
done

if [[ -e "$output" ]]; then
  echo "output already exists: $output" >&2
  exit 73
fi

temporary_directory=$(mktemp -d "${TMPDIR:-/tmp}/gerbil-parser-rowan-aot.XXXXXX")
trap 'rm -rf "$temporary_directory"' EXIT
trace_file="$temporary_directory/modules.txt"
trace_output="$temporary_directory/generated.rs"

GERBIL_PATH=$(dirname "$project_lib") \
GERBIL_PARSER_ROWAN_AOT_LIB="$project_lib:$gerbil_lib" \
GERBIL_PARSER_ROWAN_AOT_TRACE_MODULES=1 \
  "$generator" "$grammar" "$trace_output" 2>"$trace_file"

if [[ ! -s "$trace_output" || ! -s "$trace_file" ]]; then
  echo "generator did not produce a source module and module trace" >&2
  exit 70
fi

mkdir -p "$output/bin" "$output/lib"
cp -p "$generator" "$output/bin/gerbil-parser-rowan-aot"

copy_ssi_tree() {
  local root=$1
  while IFS= read -r -d '' source_path; do
    local relative_path=${source_path#"$root"/}
    mkdir -p "$output/lib/$(dirname "$relative_path")"
    cp -p "$source_path" "$output/lib/$relative_path"
  done < <(find "$root" -type f -name '*.ssi' -print0)
}

copy_ssi_tree "$gerbil_lib"
copy_ssi_tree "$project_lib"

canonicalized_object_count=0
while IFS= read -r source_path; do
  case "$source_path" in
    "$project_lib"/*)
      root=$project_lib
      ;;
    "$gerbil_lib"/*)
      root=$gerbil_lib
      ;;
    *)
      echo "traced module is outside admitted roots: $source_path" >&2
      exit 70
      ;;
  esac

  relative_path=${source_path#"$root"/}
  module_path=${relative_path%.*}
  destination="$output/lib/$module_path.o1"
  mkdir -p "$(dirname "$destination")"

  if [[ "$source_path" == *.o1 ]]; then
    cp -p "$source_path" "$destination"
  else
    generated_scheme="$root/$module_path.scm"
    if [[ ! -f "$generated_scheme" ]]; then
      echo "non-canonical object has no generated Scheme owner: $source_path" >&2
      exit 70
    fi
    gsc -dynamic -o "$destination" "$generated_scheme"
    canonicalized_object_count=$((canonicalized_object_count + 1))
  fi
done < <(sort -u "$trace_file")

dynamic_object_count=$(sort -u "$trace_file" | wc -l | tr -d ' ')
ssi_count=$(find "$output/lib" -type f -name '*.ssi' | wc -l | tr -d ' ')
if command -v sha256sum >/dev/null 2>&1; then
  generator_sha256=$(sha256sum "$output/bin/gerbil-parser-rowan-aot" | awk '{print $1}')
else
  generator_sha256=$(shasum -a 256 "$output/bin/gerbil-parser-rowan-aot" | awk '{print $1}')
fi

cat >"$output/receipt.v1" <<EOF
schema=gerbil-parser.rowan-aot-bundle.v1
target=$(uname -s)-$(uname -m)
dynamic_object_count=$dynamic_object_count
canonicalized_object_count=$canonicalized_object_count
ssi_count=$ssi_count
generator_sha256=$generator_sha256
EOF

case $(uname -s) in
  Darwin)
    otool -L "$output/bin/gerbil-parser-rowan-aot" >"$output/native-dependencies.txt"
    ;;
  Linux)
    ldd "$output/bin/gerbil-parser-rowan-aot" >"$output/native-dependencies.txt"
    ;;
  *)
    echo "unsupported bundle target: $(uname -s)" >&2
    exit 69
    ;;
esac
