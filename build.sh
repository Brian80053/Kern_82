#!/bin/sh
set -eu
cd "$(dirname "$0")"
mkdir -p build/autogen bin
version=$(sed -n 's/^version: *\([0-9.]*\).*/\1/p' kern82.cabal | tr '.' ',')
test -n "$version"
cat > build/autogen/Paths_kern82.hs <<EOF
module Paths_kern82 (version) where
import Data.Version (Version, makeVersion)
version :: Version
version = makeVersion [$version]
EOF
ghc -O2 -Wall -isrc -iapp -ibuild/autogen -outputdir build app/Main.hs -o build/kern
cp build/kern build/kern82
cp build/kern bin/kern
cp build/kern82 bin/kern82
