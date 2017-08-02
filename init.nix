{ stdenv }:

stdenv.mkDerivation {
  name = "init";
  src = ./scripts;
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/app
    cp -r -v . $out/app
  '';
}
