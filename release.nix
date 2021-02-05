{ nixpkgs ? import <nixpkgs> {} }:

let
  hp = with builtins; nixpkgs.haskellPackages.extend (self: super: {
    masse-prelude = self.callPackage (nixpkgs.fetchFromGitHub {
      inherit (fromJSON (readFile ./nix/masse-prelude.json)) rev sha256;
      owner = "MasseR";
      repo = "masse-prelude";
      # rev = "ab431de3e5b091d9277d745e6e22e051573e8cae";
      # sha256 = "0036rp5c1wfl68iq198zycj3avq2l0hxwnhjwp29ybi8q7pk3qfq";
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
