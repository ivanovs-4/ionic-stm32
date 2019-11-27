let release = import ./nix/release.nix;
    nixpkgs = release.nixpkgs;
in release.nixpkgs.haskellPackages.shellFor {
    nativeBuildInputs = with nixpkgs.haskellPackages; [
      cabal-install
    ];
    buildInputs = with nixpkgs; [
      # ghcid
    ];
    packages = _: nixpkgs.lib.attrValues release.ionic-projects;
  }
