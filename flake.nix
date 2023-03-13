{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
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
          haskellPackages' = pkgs.haskellPackages.extend overlay;
        in
        {
          packages.default = haskellPackages'.jrec;
          devShells.default = haskellPackages'.shellFor {
            packages = p: [ p.jrec ];
            buildInputs = with haskellPackages'; [
              cabal-install
              haskell-language-server
            ];
          };
        };
    };
}
