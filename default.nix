let 
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/76f2e271a2ef.tar.gz";
    sha256 = "1r4da3kyghqq7vzya5yb295jpnha1fgp755bi6wda4fmhyaynnsp";
  };
  pkgs = import nixpkgsSrc {};
  gitignoreSrc = pkgs.fetchFromGitHub { 
    owner = "hercules-ci";
    repo = "gitignore";
    # put the latest commit sha of gitignore Nix library here:
    rev = "c4662e6";
    # use what nix suggests in the mismatch message here:
    sha256 = "sha256:1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
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
