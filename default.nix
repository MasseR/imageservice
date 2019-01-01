{ haskellPackages }:

haskellPackages.callCabal2nix "imageduplicates" ./. {}
