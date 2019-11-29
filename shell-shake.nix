{ nixpkgs ? (import ./nix/nixpkgs.nix {}) }:
with nixpkgs.pkgs;
pkgs.stdenv.mkDerivation {
  name = "shake-shell";
  buildInputs = [
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [shake]))
  ];
}
