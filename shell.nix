with (import <nixpkgs> {});

(callPackage ./release.nix {}).shell
