{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation
      , aeson
      , amqp
      , base
      , bytestring
      , containers
      , filemanip
      , filepath
      , network
      , optparse-applicative
      , process
      , scotty
      , stdenv
      , text
      , time
      , timeit
      , wai-cors
      , wai-extra
      , wai-middleware-static
      }:
      mkDerivation {
        pname = "monitor";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson
          amqp
          base
          bytestring
          containers
          filemanip
          filepath
          network
          optparse-applicative
          process
          scotty
          text
          time
          timeit
          wai-cors
          wai-extra
          wai-middleware-static
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
