{ pkgs
, libsodium
, pkg-config
, runCommand
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

in

# HACK: rather than providing the multi-output crate output, expose a single-
  # output structure in the format expected by cryptobox-haskell.
  # Note it expects the .so file to be called libcryptobox.so, not
  # libcryptobox_c.so, and the cbox.h to be present.
  # In the future, we might want to rework this to instead have cryptobox-c crate
  # emit a .pc file, and all downstream tooling use pkg-config to discover things,
  # but today is not that day.
runCommand "cryptobox" { } ''
  mkdir -p $out/lib $out/include
  cp ${rootCrate.lib}/lib/libcryptobox_c* $out/lib/
  ln -sfn libcryptobox_c.so $out/lib/libcryptobox.so
  cp ${rootCrate.src}/src/cbox.h $out/include
''
