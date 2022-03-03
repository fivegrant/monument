{ mkDerivation, base, containers, lib, parsec }:
mkDerivation {
  pname = "monument";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers parsec ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  description = "A basic term rewriting system";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}