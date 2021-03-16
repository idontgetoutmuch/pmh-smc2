{ pkgs ? import <nixpkgs> { config.allowBroken = true; } }:

let

  myJulia = callPackage ./. {};

in

pkgs.mkShell {
  buildInputs = [
    myJulia
    pkgs.python3
    pkgs.python3Packages.numpy
  ];

  JULIA_DEPOT_PATH="/Users/dom/.julia";
}
