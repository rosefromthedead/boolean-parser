{
  description = "";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      rec {
        packages = {
          boolean-parser = pkgs.callPackage ./default.nix {};
        };
        defaultPackage = packages.boolean-parser;
        devShell = pkgs.mkShell {
          packages = with pkgs; [ ghc haskell-language-server ];
          inputsFrom = [ packages.boolean-parser ];
        };
      }
    );
}
