{ stdenv }:

stdenv.mkDerivation {
  name = "static";
  src = ./static;
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/app/static
    cp -r -v . $out/app/static
  '';
}
