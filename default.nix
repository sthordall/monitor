{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, filemanip, filepath
      , optparse-applicative, process, scotty, stdenv, text, time, timeit
      , wai-extra
      }:
      mkDerivation {
        pname = "monitor";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base filemanip filepath optparse-applicative process scotty
          text time timeit wai-extra
        ];
        homepage = "https://github.com/kuznero/scripts/monitoring/monitor#README";
        description = "Monitoring aggregator";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
