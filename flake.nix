{
  description = "A minimal RSS/Atom blogroll generator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
      # base ^>=4.21 and MultilineStrings require GHC 9.12
      haskellPackages = pkgs: pkgs.haskell.packages.ghc912;
    in
    {
      packages = forAllSystems (pkgs: rec {
        blogroll = (haskellPackages pkgs).callCabal2nix "blogroll" self { };
        default = blogroll;
      });

      devShells = forAllSystems (pkgs: {
        default = (haskellPackages pkgs).shellFor {
          packages = _: [ self.packages.${pkgs.stdenv.hostPlatform.system}.blogroll ];
          nativeBuildInputs = [
            (haskellPackages pkgs).cabal-install
          ];
        };
      });
    };
}
