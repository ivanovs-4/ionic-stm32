#!/usr/bin/env sh
mkdir -p _shake
set -ex

# Build `_shake/build` binary from Build.hs
# nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [shake])" \
nix-shell ./shell-shake.nix \
  --run 'ghc --make Build.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_shake -o _shake/build'

# Build project with `_shake/build`
# nix-shell ./shell.nix --run '_shake/build '"$@"''
_shake/build "$@"
