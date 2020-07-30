let 
  nixpkgsRev = "0f5ce2fac0c726036ca69a5524c59a49e2973dd4";
  nixpkgsSha = "0nkk492aa7pr0d30vv1aw192wc16wpa1j02925pldc09s9m9i0r3";
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = nixpkgsSha;
  }) {};
in 
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
        ]);
  }
