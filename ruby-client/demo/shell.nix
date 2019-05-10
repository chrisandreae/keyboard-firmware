with (import <nixpkgs> {});

let
  pwd = toString ./.;

  absolute-gemdir = runCommand "absolute-gemdir" {
    src = lib.sourceByRegex ./. ["^Gemfile$" "^Gemfile\.lock$" "^gemset\.nix$"];
  } ''
    mkdir $out
    parent=$(dirname ${pwd})
    for file in Gemfile Gemfile.lock gemset.nix; do
      sed -e "s|\.\./gem|$parent/gem|" $src/$file > $out/$file
    done
  '';
in
(bundlerEnv {
  name = "ruby-client-bundler-env";
  gemdir = absolute-gemdir;
  gemConfig = pkgs.defaultGemConfig // {
    libusb = attrs: {
      buildInputs = [ libusb1 ];
      buildFlags = [ "--enable-system-libusb" ];
      postInstall = ''
        installPath=$(cat $out/nix-support/gem-meta/install-path)
        sed -i -e 's|ffi_lib(\(.*\))|ffi_lib("${lib.getLib libusb1}/lib/libusb-1.0.dylib")|' \
          $installPath/lib/libusb/call.rb
      '';
    };
  };
}).env
