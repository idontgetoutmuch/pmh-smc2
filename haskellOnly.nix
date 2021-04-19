let

myHaskellPackageOverlay = self: super: {

  R = super.R.overrideAttrs (oldAttrs: {
    configureFlags = [ "--without-x" ];
  });

  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {

      random-fu = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        hself.callCabal2nixWithOptions "random-fu" (builtins.fetchGit {
          url = "https://github.com/lehins/random-fu";
          rev = "23d4390dbad60ae491b12ebd2cabb7a985302b55";
          ref = "switch-to-random";
        }) "--subpath random-fu" { }
      ))) [ ];

      rvar = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        hself.callCabal2nixWithOptions "rvar" (builtins.fetchGit {
          url = "https://github.com/lehins/random-fu";
          rev = "23d4390dbad60ae491b12ebd2cabb7a985302b55";
          ref = "switch-to-random";
        }) "--subpath rvar" { }
      ))) [ ];

      random = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        hself.callCabal2nixWithOptions "rvar" (builtins.fetchGit {
          url = "https://github.com/haskell/random";
          rev = "edae4f7908f3c7e00be1094034a4a09cd72ab35e";
        }) "" { }
      ))) [ ];

      hashable = super.haskell.lib.doJailbreak hsuper.hashable;

      mwc-random = hself.callHackage "mwc-random" "0.15.0.1" {};

      random-fu-multivariate =  hsuper.random-fu-multivariate;

      inline-r = super.haskell.lib.dontCheck (
        hself.callHackage "inline-r" "0.10.4" { R = self.R; });
    };
  };
};

in

{ nixpkgs ? import <nixpkgs> { config.allowBroken = true; overlays = [ myHaskellPackageOverlay ]; }, compiler ? "default", doBenchmark ? false }:

let

  haskellDeps = ps: with ps; [
    base hmatrix hvega massiv mtl random random-fu random-fu-multivariate
    vector
    mwc-random
    inline-r
  ];

in

  nixpkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = with nixpkgs.rPackages; [
    (nixpkgs.myHaskellPackages.ghcWithPackages haskellDeps)
    nixpkgs.R
    ggplot2
  ];
  }
