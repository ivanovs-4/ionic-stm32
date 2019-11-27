#!/usr/bin/env bash
set -euo pipefail

mkdir -p _shake

# Build `_shake/build` binary from Build.hs
nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [shake])" \
  --run 'ghc --make Build.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=../_shake -o _shake/build'

# Build project with shake
_shake/build "$@"
