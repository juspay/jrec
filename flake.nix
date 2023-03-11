{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    gitignore.url = "github:hercules-ci/gitignore.nix";
  };
  outputs = inputs@{ nixpkgs, flake-parts, gitignore, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem = { self', inputs', pkgs, system, ... }:
        let
          inherit (gitignore.lib) gitignoreSource;
          fixCyclicReference = drv:
            pkgs.haskell.lib.overrideCabal drv (_: {
              enableSeparateBinOutput = false;
            });
          overlay = self: super: {
            jrec = self.callCabal2nix "jrec" (gitignoreSource ./.) { };
            # Workaround for https://github.com/NixOS/nixpkgs/issues/140774
            haskell-language-server = super.haskell-language-server.overrideScope (lself: lsuper: {
              ormolu = fixCyclicReference super.ormolu;
            });
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
