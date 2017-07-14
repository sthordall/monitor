{ stdenv }:

stdenv.mkDerivation {
  name = "checks";
  src = ./checks;
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/app/checks
    cp -r -v . $out/app/checks
    patchShebangs $out/app/checks
    chmod +x $out/app/checks/**/*.sh
  '';
}
