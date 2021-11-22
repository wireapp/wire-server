{ pkgs ? import ../../nix }:
let
  pinned = {
    stack2cabal =
      let source = pkgs.fetchFromGitHub {
          owner = "hasufell";
          repo = "stack2cabal";
          rev = "afa113beb77569ff21f03fade6ce39edc109598d";
          sha256 = "1zwg1xkqxn5b9mmqafg87rmgln47zsmpgdkly165xdzg38smhmng";
      };

      overlay = self: super: {
        "stack2cabal" = super.callCabal2nix "stack2cabal" source { };
      };

      haskellPackages = pkgs.haskell.packages.ghc884.override {
        overrides = overlay;
      };

      in pkgs.haskell.lib.doJailbreak haskellPackages.stack2cabal;

  };
in pkgs.mkShell {
  name = "shell";
  buildInputs = [
    pinned.stack2cabal
  ];
}
