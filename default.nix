let 
  pkgs = import <nixpkgs> { };
in 
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          # nix-shell --run "cabal-fmt -i *.cabal"
          cabal-fmt
          ghcid
        ]);
  }
