{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  main =
    haskell.lib.dontCheck
      (haskell.lib.justStaticExecutables (import ./default.nix {}));
  checks = import ./checks.nix { inherit stdenv; };
  static = import ./static.nix { inherit stdenv; };
in
  dockerTools.buildImage {
    name = "monitor";
    tag = "latest";
    contents = [
      bash
      checks
      coreutils
      curl
      docker
      gawk
      gnugrep
      gnused
      jq
      main
      static
    ];
    config = {
      Env = [
        "DOCKER_HOST="
        "RABBITMQ_ADDRESS="
        "RABBITMQ_CREDS="
        "RABBITMQ_CONNECTOR_INFO="
      ];
      WorkingDir = "/app";
      ExposedPorts = {
        "3000/tcp" = {};
      };
      Volumes = {
        "/checks" = {};
      };
      Cmd = [ "${main}/bin/monitor" "-m" "-p" "./checks" ];
    };
  }
