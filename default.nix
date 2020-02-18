let
  pkgs = import ./nix;
  lib = pkgs.lib;
  pkgSet =
    pkgs.haskell-nix.mkStackPkgSet rec {
      stack-pkgs = import ./nix/stack/pkgs.nix;

      # TODO: we will need to patch some stuff for protobuf support
      pkg-def-extras = [];

      modules =
        [
          # Hack to give types-common-journal access to protobuf protoc
          { packages.types-common-journal.components.library.build-tools = [ pkgs.buildPackages.protobuf ]; }
          (
            let
              pkg-names = lib.attrNames (stack-pkgs.extras {}).packages;
              patch = name: {
                # Final hack, workaround for  https://github.com/input-output-hk/haskell.nix/issues/298
                # we know that local packages sure dont have postUnpack. We find out that they're local based
                # on their source beign local
                ${name} = { config, ... }: lib.mkIf (builtins.typeOf config.src == "path") {
                  package.cleanHpack = true;
                  postUnpack = ''
                    if [[ -e $sourceRoot/package.yaml ]]; then
                      substituteInPlace $sourceRoot/package.yaml --replace '../../package-defaults.yaml' "${./package-defaults.yaml}"
                    fi
                  '';
                  dontStrip = false;
                };
              };
            in
              {
                packages = lib.mkMerge (map patch pkg-names);
              }
          )
        ];
    };
in
  {
    galley = pkgSet.config.hsPkgs.galley.components.exes;

    # doesn't compile yet because of cryptobox!
    brig  = pkgSet.config.hsPkgs.brig.components.exes;
    inherit pkgSet;
  }
