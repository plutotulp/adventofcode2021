{
  description = "Some advent of Code 2021 solutions";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      hp = pkgs.haskellPackages;
      name = "adventofcode2021";
      dontCheck = pkgs.haskell.lib.dontCheck;
    in {
      # Using dontCheck because the test suite includes doctests with
      # doctest-parallel, which is not doing so well in nixpkgs at the
      # moment (2021-12).
      packages.${system}.${name} = dontCheck (hp.callCabal2nix name self {});

      defaultPackage.${system} = self.packages.${system}.${name};
      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          hp.haskell-language-server
          hp.hlint
          hp.ghcid
          hp.cabal-install
        ];
        inputsFrom = builtins.attrValues self.packages.${system};
      };
    };
}
