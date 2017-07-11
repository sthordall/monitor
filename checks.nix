{ stdenv }:

stdenv.mkDerivation {
  name = "checks";
  src = ./checks;
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/checks
    cp -r -v . $out/checks
    patchShebangs $out/checks
    chmod +x $out/checks/**/*.sh
  '';
}
