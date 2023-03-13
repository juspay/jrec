{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-ghc88.url = "github:nixos/nixpkgs/76f2e271a2ef";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem = { self', inputs', pkgs, system, ... }:
        let
          overlay = self: super: {
            jrec = self.callCabal2nix "jrec" ./. { };
          };
          ghcVersions = {
            ghc88 = inputs.nixpkgs-ghc88.legacyPackages.${system}.haskellPackages.extend overlay;
            ghc92 = pkgs.haskellPackages.extend overlay;
          };
        in
        {
          packages.default = ghcVersions.ghc92.jrec;
          devShells.default = ghcVersions.ghc92.shellFor {
            packages = p: [ p.jrec ];
            buildInputs = with ghcVersions.ghc92; [
              cabal-install
              ghcid
              haskell-language-server
            ];
          };
          # Expose jrec built with GHC 8.8 so it can be tested in CI.
          packages.jrec-ghc88 = ghcVersions.ghc88.jrec;
        };
    };
}
