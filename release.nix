{ nixpkgs ? import ./nixpkgs.nix {} }:

rec {
  imageduplicates = nixpkgs.haskellPackages.callPackage ./default.nix {};
  shell = nixpkgs.buildEnv {
    name = "shell";
    paths = [];
    buildInputs = with nixpkgs.haskellPackages; [
      ghcid
      hasktags
      cabal-install
      (ghcWithPackages (_: imageduplicates.buildInputs ++ imageduplicates.propagatedBuildInputs))
    ];
  };
}
