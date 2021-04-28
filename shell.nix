let

nixpkgs = /Users/dom/nixpkgs;

in

{ pkgs ? import nixpkgs { config.allowBroken = true; } }:

let

  # The base Julia version
  baseJulia = pkgs.julia_15;

  # Extra libraries for Julia's LD_LIBRARY_PATH.
  # Recent Julia packages that use Artifacts.toml to specify their dependencies
  # shouldn't need this.
  # But if a package implicitly depends on some library being present, you can
  # add it here.
  extraLibs = [];

  # Wrapped Julia with libraries and environment variables.
  # Note: setting The PYTHON environment variable is recommended to prevent packages
  # from trying to obtain their own with Conda.
  julia = pkgs.runCommand "julia-wrapped" { buildInputs = [pkgs.makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${baseJulia}/bin/julia $out/bin/julia \
                --suffix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath extraLibs}" \
                --set PYTHON ${pkgs.python3}/bin/python
  '';

myJulia = pkgs.callPackage ./common.nix {
  inherit julia;

  # Run Pkg.precompile() to precompile all packages?
  precompile = true;

  # Extra arguments to makeWrapper when creating the final Julia wrapper.
  # By default, it will just put the new depot at the end of JULIA_DEPOT_PATH.
  # You can add additional flags here.
  makeWrapperArgs = "";
};

in

pkgs.mkShell {
  buildInputs = [
    myJulia
  ];

  JULIA_DEPOT_PATH="/Users/dom/.julia";
  MYVARIABLE = "hello";
}
