{ stdenv, lib, buildEnv, runCommand, makeWrapper, avrgcc, avrlibc, avrbinutils,
  bundlerEnv, ruby }:

lib.makeOverridable ({ name, hardwareLibrary, hardwareVariant, hasStorage ? true }:

let
  # The prefix under which avrgcc is installed is expected to contain
  # avrlibc and uses some kind of magic based on this assumption to
  # find which paths to include. Nixpkgs split the distribution
  # without providing a convenience wrapper to make them automatically
  # work, with the fairly serious regression that the magic can no
  # longer pick include or library paths. So we include a half hearted
  # wrapper here instead, that assumes all targets are avr5.

  # If not for this hack, this file would be much more reasonable. The
  # discussion is at https://github.com/NixOS/nixpkgs/pull/29007.

  avrgcc-version = lib.removePrefix "avr-gcc" avrgcc.name;

  cflagsForArch = arch: lib.concatStringsSep " " [
    "-I" "${avrlibc}/avr/include"
    "-B" "${avrlibc}/avr/lib/${arch}"
    "-L" "${avrlibc}/avr/lib/${arch}"
    "-L" "${avrgcc}/lib/gcc/avr/${avrgcc-version}/${arch}"
  ];

  avrgcc-wrapper = runCommand "avrgcc-wrapper" {
    buildInputs = [ makeWrapper ];
  } ''
    mkdir -p $out/bin
    makeWrapper ${avrgcc}/bin/avr-gcc $out/bin/avr-gcc --add-flags \
      "${cflagsForArch "avr5"}"
  '';

  firmware = stdenv.mkDerivation {
    inherit name;
    src = stdenv.lib.cleanSource ./..;

    HARDWARE_VARIANT = hardwareVariant;
    HAS_STORAGE      = hasStorage;
    HARDWARE_LIBRARY = hardwareLibrary;

    buildInputs = [ avrgcc-wrapper avrbinutils ];

    buildPhase = ''
      make -f Makefile.$HARDWARE_LIBRARY $makeFlags
    '';

    installPhase = ''
      mkdir $out
      cp *.{hex,elf} $out/
    '';

    dontFixup = true;
  };

  extractDefaultMappingEnv = bundlerEnv {
    name = "extra-default-mapping";
    inherit ruby;
    gemdir = ../qtclient/extract-default-mapping;
  };

  mapping = firmware.overrideAttrs (attrs: {
    name = "${attrs.name}-default-mapping";
    buildInputs = attrs.buildInputs ++ [ extractDefaultMappingEnv.wrappedRuby ];
    buildPhase = ''
      make -f Makefile.$HARDWARE_LIBRARY obj/hardware.o
      ruby ./qtclient/extract-default-mapping/extract-default-mapping.rb ${name}_default_mapping > extracted_mapping.c
    '';
    installPhase = ''
      cp extracted_mapping.c $out
    '';
  });

in

lib.extendDerivation true { inherit mapping; } firmware

)
