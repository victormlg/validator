{
  description = "Haskell dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          name = "haskell-dev";

          buildInputs = with pkgs; [
            # Haskell toolchain
            ghc
            cabal-install

            # Common native dependencies
            zlib
            pkg-config

            # Optional but useful
            haskell-language-server
            ghcid
          ];

          shellHook = ''
            echo "GHC $(ghc --version)"
            echo "Cabal $(cabal --version)"
          '';
        };
      }
    );
}
