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
    fromImage = dockerTools.pullImage {
      imageName = "alpine";
      imageTag = "3.3";
      imageId = "f58d61a874bedb7cdcb5a409ebb0c53b0656b880695c14e78a69902873358d5f";
      sha256 = "0lvd5zxjgwp3jl5r8qgb2kapmxclpgdv1a7c03yiagxsil5gwb8c";
    };
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
