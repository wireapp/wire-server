{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  # should probably follow but I want to avoid rebuild ghc right now
  inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=5df05c902cde398e056eb6271d5fe13e418db4c6";
  # inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  nixConfig.bash-prompt = "nix-develop $ ";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (import ./nix/wire-packages.nix)
        # (final: prev: {
        #   # This overlay adds our project to pkgs
        #   wireProject =
        #     final.haskell-nix.project' {
        #       src = ./.;
        #       # compiler-nix-name = "ghc8104";
        #     };
        # })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.wireHaskellPkgs.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."brig:exe:brig";

      # This is used by `nix develop .` to open a shell for use with
      # `cabal`, `hlint` and `haskell-language-server`
      devShell = pkgs.wireHaskellPkgs.shellFor {
        tools = {
          cabal = "latest";
          hlint = "latest";
          haskell-language-server = "latest";
        };

        nativeBuildInputs = with pkgs; (with darwin.apple_sdk.frameworks; [
          Cocoa
          CoreServices
        ]);

        # pkgs.stdenv.isDarwin 
      };
    });
}
