{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv, filepath, filemanip
      , optparse-applicative, process, timeit, aeson
      , text, scotty, wai-extra, time
      , ghc-mod, hlint, hoogle, hindent, stylish-haskell
      }:
      mkDerivation {
        pname = "monitor";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base optparse-applicative filepath filemanip process timeit aeson text scotty wai-extra time ];
        buildDepends = [ ghc-mod hlint hoogle hindent stylish-haskell ];
        homepage = "https://github.com/kuznero/scripts/monitoring/monitor#README";
        description = "Monitoring aggregator";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = (if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler})
                    .override {
                      overrides = self: super:
                      {
                        ghc-syb-utils = pkgs.haskell.lib.dontCheck super.ghc-syb-utils;
                      };
                    };

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
