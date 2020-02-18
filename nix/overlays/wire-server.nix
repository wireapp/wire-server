self: super: {
  wireServices = self.haskell-nix.stackProject {
    src = self.haskell-nix.haskellLib.cleanGit { src = ../../.; };
    modules = [
      {
        packages.galley.components.exes.all.dontStrip = false;
      }
    ];
  };
}
