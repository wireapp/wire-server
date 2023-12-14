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
  ffi = crates.workspaceMembers.rusty-jwt-tools-ffi.build;
in

# HACK: rather than providing the multi-output crate output, expose a single-
  # output structure in the format expected by rusty-tools.
  # It wants a lib/librusty_jwt_tools_ffi.so to be present,
  # and that's usually symlink to other versioned libs.
  # In the future, we might want to rework this to instead have our crate also
  # emit a .pc file, and all downstream tooling use pkg-config to discover
  # things.
runCommand "rusty_jwt_tools" { } ''
  mkdir -p $out/lib
  cp ${ffi.lib}/lib/librusty_jwt_tools_ffi*.so $out/lib/
''
