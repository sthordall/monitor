{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  main =
    haskell.lib.dontCheck
      (haskell.lib.justStaticExecutables (import ./default.nix {}));
  checks = import ./checks.nix { inherit stdenv; };
in
  dockerTools.buildImage {
    name = "monitor";
    tag = "latest";
    contents = [
      main
      checks
      bash
      coreutils
      jq
      curl
      gnugrep
      gnused
      gawk
      docker
    ];
    runAsRoot = ''
      #!${stdenv.shell}
      ${dockerTools.shadowSetup}
      mkdir -p /checks
    '';
    config = {
      Env = [
        "DOCKER_HOST="
        "RABBITMQ_ADDRESS="
        "RABBITMQ_CREDS="
      ];
      WorkingDir = "/checks";
      ExposedPorts = {
        "3000/tcp" = {};
      };
      Volumes = {
        "/checks" = {};
      };
      Cmd = [ "${main}/bin/monitor" ];
    };
  }
