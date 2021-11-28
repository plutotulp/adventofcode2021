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
    in {
      packages.${system}.${name} = hp.callCabal2nix name self {};
      defaultPackage.${system} = self.packages.${system}.${name};
      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          hp.haskell-language-server
          hp.hlint
          hp.ghcid
          hp.cabal-install
          hp.brittany
          pkgs.vscode-fhsWithPackages [ pkgs.vscode-extensions.haskell ]
        ];
        inputsFrom = builtins.attrValues self.packages.${system};
      };
    };
}
