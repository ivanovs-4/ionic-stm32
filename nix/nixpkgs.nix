import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo  = "nixpkgs";
  inherit (builtins.fromJSON (builtins.readFile ./nixpkgs.json)) rev sha256;
})
