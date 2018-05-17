{ mkDerivation, base, bytestring, containers, fgl, hspec, mtl
, parsec, stdenv, ghc
}:
mkDerivation {
  pname = "Compiler";
  version = "0.0";
  src = ../compiler;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers fgl mtl parsec
  ];
  testHaskellDepends = [
    base bytestring containers fgl hspec mtl parsec
  ];
  license = stdenv.lib.licenses.gpl2;
  postInstall = ''
    mkdir -p $out/include
    cp dist/build/*.h $out/include
  '';
  passthru = {
    inherit ghc;
  };
}
