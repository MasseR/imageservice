{ nixpkgs ? import <nixpkgs> {} }:

import (nixpkgs.fetchFromGitHub {
  owner = "nixos";
  repo = "nixpkgs";
  rev = "bcab3daac7e1829de27f80ae18669607f6c19fdf";
  sha256 = "0dw9d0sx66fp2argsprrg2vl0kzji1drl69ji5fvyngwfad23l9x";
}) {}
