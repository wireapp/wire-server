self: super: {

  cryptobox = self.callPackage ./pkgs/cryptobox { };
  zauth = self.callPackage ./pkgs/zauth { };
  mls-test-cli = self.callPackage ./pkgs/mls-test-cli { };

  # Named like this so cabal2nix can find it
  rusty_jwt_tools_ffi = self.callPackage ./pkgs/rusty_jwt_tools_ffi { };

  nginxModules = super.nginxModules // {
    zauth = {
      name = "zauth";
      src = ../services/nginz/third_party/nginx-zauth-module;
      inputs = [ self.pkg-config self.zauth.lib ];
      meta = {
        license = [ self.lib.licenses.agpl3Only ];
      };
    };
  };

  nginz = super.nginx.override {
    modules = [
      self.nginxModules.vts
      self.nginxModules.moreheaders
      self.nginxModules.zauth
    ];
  };

  rabbitmqadmin = super.callPackage ./pkgs/rabbitmqadmin { };

  sbomqs = super.callPackage ./pkgs/sbomqs { };

  # Disable hlint in HLS to get around this bug:
  # https://github.com/haskell/haskell-language-server/issues/4674
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc910 = super.haskell.packages.ghc910.override {
        overrides = hfinal: hprev: {
          haskell-language-server = self.haskell.lib.disableCabalFlag hprev.haskell-language-server "hlint";
        };
      };
    };
  };
}
