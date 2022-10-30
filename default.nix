{nixpkgs, system, crane}:

let
  craneLib = crane.mkLib pkgs;
  pkgs = nixpkgs.legacyPackages.${system};
in
{
  monument = craneLib.buildPackage {
    src = ./.;
    cargoExtraArgs = "-p monument";
    nativeBuildInputs = [ pkgs.pkg-config ];
    PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
  };
}
