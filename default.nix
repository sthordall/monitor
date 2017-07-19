{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, amqp, base, bytestring, containers
      , filemanip, filepath, hspec, network, optparse-applicative
      , process, QuickCheck, scotty, stdenv, text, time, timeit, wai-cors
      , wai-extra, wai-middleware-static
      }:
      mkDerivation {
        pname = "monitor-lib";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson amqp base bytestring containers filemanip filepath network
          process text time timeit
        ];
        executableHaskellDepends = [
          base optparse-applicative scotty text wai-cors wai-extra
          wai-middleware-static
        ];
        testHaskellDepends = [ base hspec network QuickCheck text ];
        homepage = "https://github.com/kuznero/monitor#README";
        description = "Monitoring aggregator";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
