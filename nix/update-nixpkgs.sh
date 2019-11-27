#!/usr/bin/env sh
nix-shell -p nix-prefetch-git --run \
  "nix-prefetch-git https://github.com/NixOS/nixpkgs.git | tee nixpkgs.json"
