{ haskellPackages }:

haskellPackages.callCabal2nix "imageservice" ./. {}
