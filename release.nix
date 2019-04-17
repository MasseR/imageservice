{ nixpkgs ? import <nixpkgs> {} }:

rec {
  imageservice = nixpkgs.haskellPackages.callPackage ./default.nix {};
  shell = nixpkgs.buildEnv {
    name = "shell";
    paths = [];
    buildInputs = with nixpkgs.haskellPackages; [
      ghcid
      hasktags
      cabal-install
      (ghcWithPackages (_: imageservice.buildInputs ++ imageservice.propagatedBuildInputs))
    ];
  };
}
