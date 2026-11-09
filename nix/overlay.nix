self: super: {

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

  nginz = super.nginx.override ({
    modules = [
      self.nginxModules.vts
      self.nginxModules.moreheaders
      self.nginxModules.zauth
    ];
  } // self.lib.optionalAttrs self.stdenv.isDarwin {
    # The zauth nginx module uses GCC nested functions, which are not
    # supported by Clang (the default compiler on macOS). Use gccStdenv
    # to ensure GCC is used for the build.
    stdenv = self.gccStdenv;
  });

  rabbitmqadmin = super.callPackage ./pkgs/rabbitmqadmin { };

  # haskell-language-server 2.15 (nixos-26.05 has 2.13), built from the Hackage
  # versions below. The pins live in a private extension of the package set, so
  # they affect only HLS and its dependencies. Remove once nixpkgs catches up.
  #
  # Disable hlint in HLS to get around this bug:
  # https://github.com/haskell/haskell-language-server/issues/4674
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc910 = super.haskell.packages.ghc910.override {
        overrides = hfinal: hprev:
          let
            hlsPackages = hfinal.extend (hself: hsuper:
              let
                hackage = pkg: ver: sha256:
                  self.haskell.lib.dontCheck (hself.callHackageDirect { inherit pkg ver sha256; } { });
              in
              {
                ghcide = hackage "ghcide" "2.15.0.0" "sha256-vAYLQp008zlaKODfIDfF8KjG5/SfRjx7kGL0G7IfYTM=";
                hls-graph = hackage "hls-graph" "2.15.0.0" "sha256-7sPH2XFFJnC4GRF1fY44uUeCORH8NmRFCagqD2CCOwk=";
                hls-plugin-api = hackage "hls-plugin-api" "2.15.0.0" "sha256-78iCDsL2Yu+l0YZMgH94blRZufEiYw0WBgDdPjH8QYk=";
                hie-bios = hackage "hie-bios" "0.21.0" "sha256-tR8TFP2kvATLG/mHL0DTCIMnUO9x3Q9q/LJcMIceuLw=";
                hiedb = hackage "hiedb" "0.8.0.0" "sha256-Sa0tE8Sx373hCr6lcW3XZxtW61xAOSd52pPrKn8fUB4=";
                lsp = hackage "lsp" "2.8.0.0" "sha256-2R+B6KIl2ly9fQwotMhDVRl3c9acf6EKEMlpbSZXnfg=";
                unordered-containers = hackage "unordered-containers" "0.2.21" "sha256-YGklj5D14AvkiaVkJH44E7YjSb8wivPrHZz3vgsmSFk=";
                # HLS 2.15 needs Cabal(-syntax) 3.14 (GHC ships 3.12); all of its dependencies must agree.
                Cabal = hself.Cabal_3_14_2_0;
                Cabal-syntax = hself.Cabal-syntax_3_14_2_0;
                extensions = hackage "extensions" "0.1.1.0" "sha256-e/bhNgpB6i4tSmf4eXsP/YERAw3eP1IXusUntWLgeno=";
                cabal-install-parsers = hackage "cabal-install-parsers" "0.6.3" "sha256-u45XcHneit8vzn+y2qYlNAaviPomnVKH2S40y12NMkQ=";
                hls-test-utils = hackage "hls-test-utils" "2.15.0.0" "sha256-KpYSwD3aTIqT/JwoG3GVyXxmxSWRhbvfsXUZ95+THCw=";
                lsp-test = hackage "lsp-test" "0.18.0.0" "sha256-mOOwX3B3xFhGZfWIQvO9IT4ZnHez28sI/d7wVeUD6uA=";
                lsp-types = hackage "lsp-types" "2.4.0.0" "sha256-lYlaWPY+txdwEMOzlTSEo5H01hcoC/fGuS7xkaSlPPc=";
                haskell-language-server = hackage "haskell-language-server" "2.15.0.0" "sha256-eVRv8J1ZGIJIvaXryZnXv7rE/Jvd7N8oV1eR4H0UhLA=";
              });
          in
          {
            haskell-language-server = self.haskell.lib.disableCabalFlag hlsPackages.haskell-language-server "hlint";
          };
      };
    };
  };
}
