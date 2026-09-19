param([string]$Ghc = "ghc")
$ErrorActionPreference = "Stop"
Push-Location $PSScriptRoot
try {
    New-Item -ItemType Directory -Force -Path build/autogen, bin | Out-Null
    # Cabal generates Paths_kern82 itself; direct GHC builds read the same version.
    $packageText = Get-Content -LiteralPath kern82.cabal -Raw
    $versionMatch = [regex]::Match($packageText, '(?m)^version:\s*([0-9]+(?:\.[0-9]+)*)\s*$')
    if (-not $versionMatch.Success) { throw "Package version was not found" }
    $versionParts = $versionMatch.Groups[1].Value.Replace('.', ',')
    @"
module Paths_kern82 (version) where
import Data.Version (Version, makeVersion)
version :: Version
version = makeVersion [$versionParts]
"@ | Set-Content -LiteralPath build/autogen/Paths_kern82.hs -Encoding utf8
    & $Ghc -O2 -Wall -isrc -iapp -ibuild/autogen -outputdir build app/Main.hs -o build/kern.exe
    if ($LASTEXITCODE -ne 0) { throw "Haskell build failed" }
    Copy-Item -LiteralPath build/kern.exe -Destination build/kern82.exe
    Copy-Item -LiteralPath build/kern.exe -Destination bin/kern.exe
    Copy-Item -LiteralPath build/kern82.exe -Destination bin/kern82.exe
} finally { Pop-Location }
