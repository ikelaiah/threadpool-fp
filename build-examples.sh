#!/usr/bin/env sh

set -eu

usage() {
  echo "Usage: sh build-examples.sh [Default|Release] [--rebuild]" >&2
}

BUILD_MODE="${1:-Release}"
if [ "$#" -gt 0 ]; then
  shift
fi

case "$BUILD_MODE" in
  Default|Release)
    ;;
  *)
    usage
    exit 2
    ;;
esac

REBUILD=0
if [ "$#" -gt 0 ]; then
  if [ "$1" != "--rebuild" ] || [ "$#" -ne 1 ]; then
    usage
    exit 2
  fi
  REBUILD=1
fi

if ! command -v lazbuild >/dev/null 2>&1; then
  echo "lazbuild was not found in PATH. Install Lazarus and try again." >&2
  exit 127
fi

SCRIPT_DIRECTORY=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
EXAMPLES_DIRECTORY="$SCRIPT_DIRECTORY/examples"
OUTPUT_DIRECTORY="$SCRIPT_DIRECTORY/example-bin"

mkdir -p "$OUTPUT_DIRECTORY"
set -- "$EXAMPLES_DIRECTORY"/*/*.lpi
if [ ! -f "$1" ]; then
  echo "No Lazarus example projects found under '$EXAMPLES_DIRECTORY'." >&2
  exit 1
fi

PROJECT_COUNT=0
for PROJECT in "$@"; do
  RELATIVE_PROJECT=${PROJECT#"$SCRIPT_DIRECTORY"/}
  echo "==> Building $RELATIVE_PROJECT ($BUILD_MODE)"
  if [ "$REBUILD" -eq 1 ]; then
    lazbuild --build-mode="$BUILD_MODE" --no-write-project --build-all "$PROJECT"
  else
    lazbuild --build-mode="$BUILD_MODE" --no-write-project "$PROJECT"
  fi
  PROJECT_COUNT=$((PROJECT_COUNT + 1))
done

echo "Built $PROJECT_COUNT examples into '$OUTPUT_DIRECTORY'."
