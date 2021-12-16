self: super: {
  haskellPackages = super.haskellPackages.override({
    overrides = hself: hsuper: {
      haskell-language-server = self.haskell.lib.appendConfigureFlag hsuper.haskell-language-server "--enable-executable-dynamic --enable-shared";
      ghcide = self.haskell.lib.appendConfigureFlag hsuper.ghcide "--enable-executable-dynamic --enable-shared";
    };
  });
}
