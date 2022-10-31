{
  description = "An interpreter/compiler for Monument";

  inputs = {
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.follows = "rust-overlay/flake-utils";
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.follows = "rust-overlay/nixpkgs";
  };

  outputs = inputs: with inputs;
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        src = ./.;
        craneLib = crane.lib.${system};
        cargoArtifacts = craneLib.buildDepsOnly { inherit src; };
        monument = craneLib.buildPackage { inherit cargoArtifacts src; };
        monument-clippy =  craneLib.cargoClippy { inherit cargoArtifacts src; };
        monument-coverage =  craneLib.cargoTarpaulin { inherit cargoArtifacts src; };
      in rec {
        checks = {
          inherit
            monument
            monument-clippy
            monument-coverage;
        };
        packages = {
          monument = code.monument;
          all = pkgs.symlinkJoin {
            name = "all";
            paths = with code; [ monument ];
          };
          default = packages.all;
        };
        devShell = pkgs.mkShell {
          inputsFrom = builtins.attrValues self.checks;
          # Extra inputs can be added here
          nativeBuildInputs = with pkgs; [
            cargo
          ];

          buildInputs = with pkgs; [
            llvm
          ];
        };

      }
    );
}
