#!/usr/bin/env bash

if [ $(nixos-container list | grep -c imageser) -eq 0 ]; then
  sudo nixos-container create imageser --config-file dev.nix
else
  sudo nixos-container update imageser
fi

sudo nixos-container start imageser
nixos-container show-ip imageser
