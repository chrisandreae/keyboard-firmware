{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  mkFirmware = callPackage ./nix/firmware.nix {};
in

rec {
  qtclient = libsForQt5.callPackage ./nix/qtclient.nix {
    withCompiler = true;
    inherit compiler;
  };

  compiler = haskell.packages.ghc802.callPackage ./nix/compiler.nix {};

  kinesis = mkFirmware {
    name = "kinesis";
    hardwareVariant = "KINESIS";
    hardwareLibrary = "vusb";
  };

  kinesis110 = kinesis.override {
    name = "kinesis110";
    hardwareVariant = "KINESIS110";
  };

  ergodox-nostorage = mkFirmware {
    name = "ergodox-nostorage";
    hardwareVariant = "ERGODOX";
    hardwareLibrary = "lufa";
    hasStorage = false;
  };

  ergodox-storage = ergodox-nostorage.override {
    name = "ergodox-storage";
    hasStorage = true;
  };
}
