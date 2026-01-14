{ pkgs
, libsodium
, pkg-config
, runCommand
, stdenv
}:

let
  # load the crate2nix crate tree
  crates = import ./Cargo.nix {
    inherit pkgs;
    nixpkgs = pkgs.path;

    # per-crate overrides
    defaultCrateOverrides = pkgs.defaultCrateOverrides // {
      libsodium-sys = prev: {
        nativeBuildInputs = prev.nativeBuildInputs or [ ] ++ [ pkg-config ];
        buildInputs = [ libsodium ];
      };
    };
  };

  rootCrate = crates.rootCrate.build;

  # Use the correct library extension based on platform
  libExt = if stdenv.isDarwin then "dylib" else "so";

in

# HACK: rather than providing the multi-output crate output, expose a single-
  # output structure in the format expected by cryptobox-haskell.
  # Note it expects the library file to be called libcryptobox, not
  # libcryptobox_c, and the cbox.h to be present.
  # In the future, we might want to rework this to instead have cryptobox-c crate
  # emit a .pc file, and all downstream tooling use pkg-config to discover things,
  # but today is not that day.
runCommand "cryptobox"
{
  nativeBuildInputs = if stdenv.isDarwin then [ pkgs.darwin.cctools ] else [ ];
}
  ''
    mkdir -p $out/lib $out/include
    cp ${rootCrate.lib}/lib/libcryptobox_c* $out/lib/

    # Fix install_name on Darwin to use absolute paths
    ${if stdenv.isDarwin then ''
      for lib in $out/lib/libcryptobox_c*.${libExt}; do
        libname=$(basename "$lib")
        install_name_tool -id "$out/lib/$libname" "$lib"
      done
    '' else ""}

    ln -sfn libcryptobox_c.${libExt} $out/lib/libcryptobox.${libExt}
    cp ${rootCrate.src}/src/cbox.h $out/include
  ''
