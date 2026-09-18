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

project_lib=$(cd "$project_lib" && pwd -P)
gerbil_lib=$(cd "$gerbil_lib" && pwd -P)

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

sha256_file() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  else
    shasum -a 256 "$1" | awk '{print $1}'
  fi
}

native_library_count=0

if [[ $(uname -s) == Darwin ]]; then
  native_directory="$output/lib/native"
  native_manifest="$temporary_directory/native-libraries.tsv"
  mkdir -p "$native_directory"
  : >"$native_manifest"

  macho_dependencies() {
    otool -L "$1" | tail -n +2 | awk '{print $1}'
  }

  collect_macos_dependency() {
    local dependency=$1
    case "$dependency" in
      /usr/lib/* | /System/Library/* | @*)
        return
        ;;
    esac

    if [[ ! -f "$dependency" ]]; then
      echo "native dependency not found: $dependency" >&2
      exit 70
    fi

    local name
    local existing_source
    name=$(basename "$dependency")
    existing_source=$(awk -F '\t' -v name="$name" '$1 == name { print $2; exit }' \
      "$native_manifest")
    if [[ -n "$existing_source" ]]; then
      if ! cmp -s "$dependency" "$existing_source"; then
        echo "native dependency basename collision: $name" >&2
        exit 70
      fi
      return
    fi

    cp -pL "$dependency" "$native_directory/$name"
    printf '%s\t%s\n' "$name" "$dependency" >>"$native_manifest"
    while IFS= read -r nested_dependency; do
      collect_macos_dependency "$nested_dependency"
    done < <(macho_dependencies "$dependency")
  }

  rewrite_macos_dependencies() {
    local consumer=$1
    local prefix=$2
    local dependency
    local name
    while IFS= read -r dependency; do
      case "$dependency" in
        /usr/lib/* | /System/Library/* | @*)
          continue
          ;;
      esac
      name=$(basename "$dependency")
      install_name_tool -change "$dependency" "$prefix/$name" "$consumer"
    done < <(macho_dependencies "$consumer")
  }

  while IFS= read -r dependency; do
    collect_macos_dependency "$dependency"
  done < <(macho_dependencies "$output/bin/gerbil-parser-rowan-aot")

  rewrite_macos_dependencies \
    "$output/bin/gerbil-parser-rowan-aot" \
    '@executable_path/../lib/native'
  while IFS=$'\t' read -r name _source; do
    native_library="$native_directory/$name"
    rewrite_macos_dependencies "$native_library" '@loader_path'
    install_name_tool -id "@loader_path/$name" "$native_library"
    codesign --force --sign - "$native_library"
  done <"$native_manifest"
  codesign --force --sign - "$output/bin/gerbil-parser-rowan-aot"
  codesign --verify --deep --strict "$output/bin/gerbil-parser-rowan-aot"

  {
    echo "schema=gerbil-parser.rowan-aot-native-libraries.v1"
    while IFS=$'\t' read -r name _source; do
      printf '%s  %s\n' "$(sha256_file "$native_directory/$name")" "$name"
    done <"$native_manifest"
  } >"$output/native-libraries.v1"

  native_library_count=$(wc -l <"$native_manifest" | tr -d ' ')
  if otool -L "$output/bin/gerbil-parser-rowan-aot" "$native_directory"/*.dylib \
    | grep -E '/opt/homebrew|/usr/local|/nix/store'; then
    echo "bundle retains a host-specific native dependency" >&2
    exit 70
  fi
fi

generator_sha256=$(sha256_file "$output/bin/gerbil-parser-rowan-aot")

cat >"$output/receipt.v1" <<EOF
schema=gerbil-parser.rowan-aot-bundle.v1
target=$(uname -s)-$(uname -m)
dynamic_object_count=$dynamic_object_count
canonicalized_object_count=$canonicalized_object_count
ssi_count=$ssi_count
native_library_count=$native_library_count
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
