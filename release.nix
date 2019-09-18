{ nixpkgs ? import <nixpkgs> {} }:

rec {
  imageservice = nixpkgs.haskellPackages.callPackage ./default.nix {};
  frontend = (nixpkgs.callPackage ./frontend {}).frontend;
  shell = nixpkgs.buildEnv {
    name = "shell";
    paths = [];
    buildInputs = with nixpkgs.haskellPackages; [
      hlint
      stylish-haskell
      ghcid
      hasktags
      cabal-install
      (ghcWithPackages (_: imageservice.buildInputs ++ imageservice.propagatedBuildInputs))
    ];
  };
}
