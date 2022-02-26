with import <nixpkgs> {};

let
  config = {
    packageOverrides = pkgs: rec{
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          monument = haskellPackages.callPackage ./monument.nix {};
	};
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in 
{
  monument = pkgs.haskellPackages.monument;
}
