let

 myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {

      random-fu = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        hself.callCabal2nixWithOptions "random-fu" (builtins.fetchGit {
          url = "https://github.com/haskell-numerics/random-fu";
          rev = "42c72183a65c429230ad9d006534f59a6b72a329";
        }) "--subpath random-fu" { }
      ))) [ ];

      random-source = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        hself.callCabal2nixWithOptions "random-fu" (builtins.fetchGit {
          url = "https://github.com/haskell-numerics/random-fu";
          rev = "42c72183a65c429230ad9d006534f59a6b72a329";
        }) "--subpath random-source" { }
      ))) [ ];

      rvar = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        hself.callCabal2nixWithOptions "rvar" (builtins.fetchGit {
          url = "https://github.com/haskell-numerics/random-fu";
          rev = "42c72183a65c429230ad9d006534f59a6b72a329";
        }) "--subpath rvar" { }
      ))) [ ];

      random = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        hself.callCabal2nixWithOptions "rvar" (builtins.fetchGit {
          url = "https://github.com/haskell/random";
          rev = "edae4f7908f3c7e00be1094034a4a09cd72ab35e";
        }) "" { }
      ))) [ ];

      hashable = super.haskell.lib.doJailbreak hsuper.hashable;
    };
  };
};

in

{ nixpkgs ? import <nixpkgs> { config.allowBroken = true; overlays = [ myHaskellPackageOverlay ]; }, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hmatrix, hvega, lib, massiv, mtl, random
      , random-fu, random-fu-multivariate, random-source, vector
      }:
      mkDerivation {
        pname = "pmh-smc2-haskell";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base hmatrix hvega massiv mtl random random-fu random-fu-multivariate
          random-source vector
        ];
        homepage = "https://github.com/idontgetoutmuch/whatever";
        description = "Whatever";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.myHaskellPackages
                       else pkgs.myHaskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
