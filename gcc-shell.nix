{ nixpkgs ? (import ./nix/nixpkgs.nix {}) }:
with nixpkgs.pkgs;
pkgs.stdenv.mkDerivation {
  name = "gcc-shell";
  buildInputs = [
    gcc-unwrapped
    gcc-arm-embedded
  ];
}
