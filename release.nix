{ nixpkgs ? import <nixpkgs> {} }:

let
  hp = nixpkgs.haskellPackages.extend (self: super: {
    masse-prelude = self.callPackage (nixpkgs.fetchFromGitHub {
      owner = "MasseR";
      repo = "masse-prelude";
      rev = "ab431de3e5b091d9277d745e6e22e051573e8cae";
      sha256 = "0036rp5c1wfl68iq198zycj3avq2l0hxwnhjwp29ybi8q7pk3qfq";
    }) {};
  });

in

rec {
  inherit (hp) masse-prelude;
  imageservice = nixpkgs.callPackage ./default.nix { haskellPackages = hp; };
  frontend = (nixpkgs.callPackage ./frontend {}).frontend;
  shell = nixpkgs.buildEnv {
    name = "shell";
    paths = [];
    buildInputs = with hp; [
      hlint
      stylish-haskell
      ghcid
      hasktags
      cabal-install
      (ghcWithPackages (_: imageservice.buildInputs ++ imageservice.propagatedBuildInputs))
    ];
  };
}
