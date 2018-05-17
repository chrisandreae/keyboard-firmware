{ stdenv, lib, qmake, qtbase, libusb, pkgconfig,
 withCompiler ? false, compiler ? null,
}:

assert withCompiler -> compiler != null;

stdenv.mkDerivation {
  name = "qtclient";
  src = ../qtclient;
  nativeBuildInputs = [ qmake pkgconfig ];
  buildInputs = [ libusb qtbase ];

  preConfigure = lib.optionalString withCompiler ''
    env GHC=${compiler.ghc} COMPILER=${compiler} ./integrate-compiler.sh > compiler.pri
  '';

  qmakeFlags = lib.optionalString withCompiler "USE_COMPILER=1";

  installPhase = if stdenv.isDarwin then ''
    mkdir -p $out/Applications
    mv KeyboardClient.app $out/Applications
  '' else ''
    mkdir -p $out/bin
    cp KeyboardClient $out/bin
  '';

  # See NixOS/patchelf#98; NixOS/nixpkgs#26209
  preFixup = ''
    rm -rf $(pwd)/../__nix_qt5__
  '';

  enableParallelBuilding = true;
}
