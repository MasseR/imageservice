{ pkgs ? import <nixpkgs> {} }:

# I need to take a newer purescript than what's on 19.03

let
  _pkgs = with pkgs; import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "3553c9948cd466d622b89a8eeceba3b9ee060aa7";
    sha256 = "1wnf6fzqvasd0bpm5yzzj4k29c8wgys7ys87c7sibbgvd0ll2jw6";
  }) {};

in

_pkgs.callPackage ./frontend.nix { pkgs = _pkgs; }
