let 
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/f9567594d5af.tar.gz";
    sha256 = "0vr2di6z31c5ng73f0cxj7rj9vqvlvx3wpqdmzl0bx3yl3wr39y6";
  };
  gitignoreSrc = builtins.fetchTarball { 
    url = "https://github.com/hercules-ci/gitignore/archive/c4662e6.tar.gz";
    sha256 = "1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  inherit (import gitignoreSrc { inherit (pkgs); }) gitignoreSource;
in { 
  compiler ? "default",
  pkgs ? import nixpkgsSrc {},
}:
  let 
    haskellPackages = 
      if compiler == "default" 
        then pkgs.haskellPackages 
        else pkgs.haskell.packages.${compiler};
    projectDrv = 
      haskellPackages.override {
        overrides = self: super: with pkgs.haskell.lib; {
          jrec = self.callCabal2nix "jrec" (gitignoreSource ./.) {};
        };
      };
    projectShell = 
      projectDrv.shellFor {
        packages = p: [ p.jrec ];
        buildInputs = with projectDrv; [
          cabal-install
          ormolu
          haskell-language-server
          ghcid
        ];
      };
  in 
    if pkgs.lib.inNixShell then projectShell else projectDrv.jrec
