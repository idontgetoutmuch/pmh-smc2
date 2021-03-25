let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
      accelerate =
        let accelerateSrc = builtins.fetchGit {
              url = "https://github.com/AccelerateHS/accelerate.git";
              rev = "49a39ea6e3d2d13cbfa8605dcb57a29ef13db1f9";
            };
            acc = hself.callCabal2nix "accelerate" accelerateSrc {};
        in
          super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck acc);

      accelerate-llvm =
        let accelerate-llvmSrc = builtins.fetchGit {
              url = "https://github.com/AccelerateHS/accelerate-llvm.git";
              rev = "6f93b9a1fb33b8ac7d86be46b6cafd2a939b9ff5";
            };
            acc-llvm = hself.callCabal2nixWithOptions "accelerate-llvm" accelerate-llvmSrc "--subpath accelerate-llvm" {};
        in
          super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck acc-llvm);

      accelerate-llvm-native = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        hself.callCabal2nixWithOptions "accelerate-llvm-native" (builtins.fetchGit {
          url = "https://github.com/AccelerateHS/accelerate-llvm";
          rev = "6f93b9a1fb33b8ac7d86be46b6cafd2a939b9ff5";
        }) "--subpath accelerate-llvm-native" { }
      ))) [ super.llvm_9 ];

      accelerate-blas =
        let accelerate-blasSrc = builtins.fetchGit {
              url = "https://github.com/tmcdonell/accelerate-blas.git";
              rev = "4e59e73f8545db76ea5d4570fb118af4080b0385";
            };
            acc-blas = hself.callCabal2nixWithOptions "accelerate-blas" accelerate-blasSrc "-f -llvm-ptx -f +llvm-cpu" {};
        in
          super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck acc-blas);

      blas-hs =
        let blas-hsSrc = builtins.fetchGit {
              url = "https://github.com/Rufflewind/blas-hs.git";
              rev = "d45aba711c7f59e525edaa6e626084dfe1f7e914";
            };
            bls = hself.callCabal2nixWithOptions "blas-hs" blas-hsSrc "-f +no-netlib -f +no-accelerate -f +openblas -f -mkl" { };
        in
          self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck bls)) [ super.openblas ];

    };
  };
};

in

{ nixpkgs ? import <nixpkgs> { config.allowBroken = true; overlays = [ myHaskellPackageOverlay ]; }, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, accelerate, accelerate-llvm-native,
        accelerate-blas, mwc-random-accelerate,
        base, stdenv, hmatrix, random-fu
      }:
      mkDerivation {
        pname = "lorri-test";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          accelerate
          accelerate-llvm-native
          accelerate-blas
          mwc-random-accelerate
          hmatrix
          random-fu
          base
        ];
        homepage = "https://github.com/idontgetoutmuch/whatever";
        description = "Whatever";
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.myHaskellPackages.callPackage f {
    accelerate = pkgs.myHaskellPackages.accelerate;
    accelerate-blas = pkgs.myHaskellPackages.accelerate-blas;
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
