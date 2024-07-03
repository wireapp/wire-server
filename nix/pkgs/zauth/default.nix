{ pkgs
}:

let
  # load the crate2nix crate tree
  crates = import ../../../libs/libzauth/libzauth-c/Cargo.nix {
    inherit pkgs;
    nixpkgs = pkgs.path;

    # per-crate overrides
    defaultCrateOverrides = pkgs.defaultCrateOverrides // {
      zauth-c = prev: {
        postFixup = ''
          find ${prev.src}
          mkdir -p $lib/include $lib/lib/pkgconfig
          sed -e "s~<<VERSION>>~${prev.version}~" \
            -e "s~<<PREFIX>>~$lib~" \
            ${prev.src}/src/libzauth.pc > $lib/lib/pkgconfig/libzauth.pc
          
          cp ${prev.src}/src/zauth.h $lib/include/
        '';
      };
    };
  };

in
crates.rootCrate.build
