{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, amqp, base, bytestring, containers
      , filemanip, filepath, network, optparse-applicative, process
      , scotty, stdenv, text, time, timeit, wai-cors, wai-extra
      , wai-middleware-static
      , ghc-mod, hlint, hoogle, hindent, stylish-haskell
      }:
      mkDerivation {
        pname = "monitor";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson amqp base bytestring containers filemanip filepath network
          process text timeit
        ];
        executableHaskellDepends = [
          base optparse-applicative scotty text time timeit wai-cors
          wai-extra wai-middleware-static
        ];
        testHaskellDepends = [ base ];
        buildDepends = [
          ghc-mod
          hindent
          hlint
          hoogle
          stylish-haskell
        ];
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
