{ stdenv, haskellPackages, lib }:
stdenv.mkDerivation {
  pname = "boolean-parser";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ haskellPackages.base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
