self: super: rec {
  python3 = super.python3.override ({
    packageOverrides = pself: psuper: {
      arabic-reshaper = pself.callPackage ./pkgs/python-docs/arabic-reshaper.nix { };
      rst2pdf = pself.callPackage ./pkgs/python-docs/rst2pdf.nix { };
      python-bidi = pself.callPackage ./pkgs/python-docs/python-bidi.nix { };
      svg2rlg = pself.callPackage ./pkgs/python-docs/svg2rlg.nix { };
      svglib = pself.callPackage ./pkgs/python-docs/svglib.nix { };
      sphinx-multiversion = pself.callPackage ./pkgs/python-docs/sphinx-multiversion.nix { };
      sphinxcontrib-kroki = pself.callPackage ./pkgs/python-docs/sphinxcontrib-kroki.nix { };
    };
  });

  python3Packages = python3.pkgs;
}
