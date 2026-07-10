#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")/.."

# Get version from cabal file and create release tag
VERSION=$(awk '$1 == "version:" {print $2}' blogroll.cabal)

if [ -z "$VERSION" ]; then
  echo "Could not read version from blogroll.cabal" >&2
  exit 1
fi

git tag "v$VERSION"
git push origin "v$VERSION"

echo "Released v$VERSION"
