{ nixpkgs ? import ./nixpkgs.nix {} }:

{
  imageduplicates = nixpkgs.haskellPackages.callPackage ./default.nix {};
}
