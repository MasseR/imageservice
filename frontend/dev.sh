#!/usr/bin/env bash

cmd="$1"
shift
case "$cmd" in
  shell)
    nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz -A shell "$@"
    ;;
  build)
    nix-build -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz -A frontend
    ;;
  run)
    nix-shell -p "python3.withPackages (_: [])" --run 'python -m http.server'
    ;;
  *)
    echo "Use shell or build"
esac
