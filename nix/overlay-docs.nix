self: super: rec {
  python315 = super.python315.override {
    packageOverrides = pself: psuper: {
      sphinxcontrib-kroki = pself.callPackage ./pkgs/python-docs/sphinxcontrib-kroki.nix { };
    };
  };

  python315Packages = python315.pkgs;
}
