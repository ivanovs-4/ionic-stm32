let release = import ./nix/release.nix;
    nixpkgs = release.nixpkgs;
in release.nixpkgs.haskellPackages.shellFor {
    nativeBuildInputs = with nixpkgs.haskellPackages; [
      cabal-install
      ghcid
    ];
    buildInputs = with nixpkgs; [
    ];
    packages = _: nixpkgs.lib.attrValues release.ionic-projects;
  }
