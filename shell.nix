{ pkgs ? import <nixpkgs> { } }:

# Help on getting the shell to work with `release.nix`
# https://github.com/TristanCacqueray/haskell-nix/commit/0da80ddbb433b5adf11b31fac1b534dd0f63f9c4

pkgs.haskellPackages.shellFor {
  withHoogle = true;
  packages = p: [ (import ./release.nix).monument ];
  buildInputs = [ pkgs.haskellPackages.hlint ];
}
