let pkgs = import ./nix;
in
rec {
  inherit pkgs;
  lol =
    pkgs.haskell-nix.mkStackPkgSet {
      stack-pkgs = import ./nix/stack/pkgs.nix;
      pkg-def-extras = [];
      modules = [
        ({config,...}: {
          # Removes debug referecnes. Removes gcc from the closure :)
          dontStrip = false;

          doHaddock = false;
          doHoogle = false;

          # Relative paths can not reach outside of the component in nix. replace with absolute path
          packages.galley.preBuild = ''
            substituteInPlace package.yaml --replace '../../package-defaults.yaml' "${./package-defaults.yaml}"
          '';
        })
      ];
    };
  inherit (lol.config.hsPkgs.galley.components.exes) galley galley-integration galley-schema;
  container = pkgs.dockerTools.buildLayeredImage {
    name = "galley";
    contents = [ galley galley-integration galley-schema ];
  };
}

