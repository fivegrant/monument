{
  description = "An interpreter/compiler for Monument";

  inputs = {
    rust-overlay.url = "github:oxalica/rust-overlay";
    crane.url = "github:ipetkov/crane";
    flake-utils.follows = "rust-overlay/flake-utils";
    nixpkgs.follows = "rust-overlay/nixpkgs";
  };

  outputs = inputs: with inputs;
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        code = pkgs.callPackage ./. { inherit nixpkgs system crane; };
      in rec {
        packages = {
          monument = code.monument;
          all = pkgs.symlinkJoin {
            name = "all";
            paths = with code; [ monument ];
          };
        default = packages.all;
        };
      }
    );
}
