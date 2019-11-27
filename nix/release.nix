let

  nixpkgs = import ./nixpkgs.nix { inherit config; };

  lib = nixpkgs.haskell.lib;

  config = {
    allowUnfree = true;
    packageOverrides = rpkgs: rec {
      haskellPackages = rpkgs.haskellPackages.override { overrides = haskOverrides; };
    };
  };

  haskOverrides = hpNew: hpOld:
      (lib.packagesFromDirectory { directory = ./derivations; } hpNew hpOld) //
      (projectPackages hpNew) //
      {
        # Overrides from nixpkgs
      };

  projectPackages = hsPkgs:
  let
    callInternal = name: path: hsPkgs.callCabal2nix name path { };
  in {
    ionic = callInternal "ionic" ../ionic ;
  };

  self = rec {
    inherit nixpkgs;
    ionic-projects = {
      inherit (nixpkgs.haskellPackages)
        ionic
        ;
      };
    };
in self
