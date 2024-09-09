self: super: rec {
  python312 = super.python312.override {
    packageOverrides = pself: psuper: {
      rst2pdf = pself.callPackage ./pkgs/python-docs/rst2pdf.nix { };
      sphinx-multiversion = pself.callPackage ./pkgs/python-docs/sphinx-multiversion.nix { };
      sphinx_reredirects = pself.callPackage ./pkgs/python-docs/sphinx_reredirects.nix { };
      sphinxcontrib-kroki = pself.callPackage ./pkgs/python-docs/sphinxcontrib-kroki.nix { };
      svg2rlg = pself.callPackage ./pkgs/python-docs/svg2rlg.nix { };
    };
  };

  python3Packages = python312.pkgs;
}
