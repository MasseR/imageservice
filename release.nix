{ nixpkgs ? import <nixpkgs> {} }:

let
  hp = with builtins; nixpkgs.haskellPackages.extend (self: super: {
    QuickCheck-deriving = self.callPackage (nixpkgs.fetchFromGitHub {
      inherit (fromJSON (readFile ./nix/QuickCheck-deriving.json)) rev sha256;
      owner = "MasseR";
      repo = "QuickCheck-deriving";
    }) {};
    masse-prelude = self.callPackage (nixpkgs.fetchFromGitHub {
      inherit (fromJSON (readFile ./nix/masse-prelude.json)) rev sha256;
      owner = "MasseR";
      repo = "masse-prelude";
    }) {};
    imageservice = self.callPackage ./imageservice {};
  });

in

rec {
  inherit (hp) imageservice;
  shell = hp.shellFor {
    packages = h: [h.imageservice];
    buildInputs = with hp; [
      hlint
      stylish-haskell
      ghcid
      hasktags
      cabal-install
    ];
  };
}
