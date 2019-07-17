#!/usr/bin/env sh
mkdir -p _shake
set -ex
nix-shell -shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [shake])"
  'ghc --make Build.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_shake -o _shake/build'
_shake/build "$@"
