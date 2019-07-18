{ nixpkgs ? (import ((import <nixpkgs> {}).fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "2255f292063ccbe184ff8f9b35ce475c04d5ae69";
      sha256 = "0vw5gzql7y3g287qfxmlpqvwvbv7an7lixgm3z02qa7v8qasi4hn";
    }) {})
 }:
with nixpkgs.pkgs;
pkgs.stdenv.mkDerivation {
  name = "gcc-shell";
  buildInputs = [
    gcc-unwrapped
    gcc-arm-embedded
  ];
}
