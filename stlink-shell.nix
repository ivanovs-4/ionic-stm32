{ nixpkgs ? (import ./nix/nixpkgs.nix {}) }:
with nixpkgs.pkgs;
pkgs.stdenv.mkDerivation {
  name = "stlink-shell";
  buildInputs = [
    stlink
  ];
}
