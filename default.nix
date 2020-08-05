let 
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/76f2e271a2ef.tar.gz";
    sha256 = "1r4da3kyghqq7vzya5yb295jpnha1fgp755bi6wda4fmhyaynnsp";
  };
  pkgs = import nixpkgsSrc {};
  gitignoreSrc = builtins.fetchTarball { 
    url = "https://github.com/hercules-ci/gitignore/archive/c4662e6.tar.gz";
    sha256 = "1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  inherit (import gitignoreSrc { inherit (pkgs); }) gitignoreSource;
in 
  pkgs.haskellPackages.developPackage {
    root = gitignoreSource ./.;
    name = "jrec";
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ormolu
          haskell-language-server
          ghcid
        ]);
  }
