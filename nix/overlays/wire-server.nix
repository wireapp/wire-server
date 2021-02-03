self: super: {
  # TODO: Do not use buildRustPackage. Ces't horrible
  cryptobox = self.callPackage (
    { fetchFromGitHub, rustPlatform, pkg-config, libsodium }:
      rustPlatform.buildRustPackage rec {
        name = "cryptobox-c-${version}";
        version = "2019-06-17";
        nativeBuildInputs = [ pkg-config ];
        buildInputs = [ libsodium ];
        src = fetchFromGitHub {
          owner = "wireapp";
          repo = "cryptobox-c";
          rev = "4067ad96b125942545dbdec8c1a89f1e1b65d013";
          sha256 = "1i9dlhw0xk1viglyhail9fb36v1awrypps8jmhrkz8k1bhx98ci3";
        };
        cargoSha256 = "0zs8ibv7rinrrzp9naxd7yak7kn1gp3pjb3g8i4wf7xw2hkkq81z";

        patchLibs = super.lib.optionalString super.stdenv.isDarwin ''
            install_name_tool -id $out/lib/libcryptobox.dylib $out/lib/libcryptobox.dylib
          '';

        postInstall = ''
          ${patchLibs}
          mkdir -p $out/include
          cp src/cbox.h $out/include
        '';
      }
  ) {};

  zauth = self.callPackage (
    { fetchFromGitHub, rustPlatform, pkg-config, libsodium }:
      rustPlatform.buildRustPackage rec {
        name = "libzauth-${version}";
        version = "3.0.0";
        nativeBuildInputs = [ pkg-config ];
        buildInputs = [ libsodium ];
        src = self.nix-gitignore.gitignoreSourcePure [ ../../.gitignore ] ../../libs/libzauth;
        sourceRoot = "libzauth/libzauth-c";

        cargoSha256 = "10ijvi3rnnqpy589hhhp8s4p7xfpsbb1c3mzqnf65ra96q4nd6bf"; # self.lib.fakeSha256;

        patchLibs = super.lib.optionalString super.stdenv.isDarwin ''
            install_name_tool -id $out/lib/libzauth.dylib $out/lib/libzauth.dylib
          '';

        postInstall = ''
          mkdir -p $out/lib/pkg-config
          mkdir -p $out/include
          cp src/zauth.h $out/include
          sed -e "s~<<VERSION>>~${version}~" \
            -e "s~<<PREFIX>>~$out~" \
            src/libzauth.pc > $out/lib/pkg-config/libzauth.pc
          cp target/release-tmp/libzauth.* $out/lib/
          ${patchLibs}
        '';
      }
  ) {};

  nginxModules = super.nginxModules // {
    zauth = {
      src = ../../services/nginz/third_party/nginx-zauth-module;
      inputs = [ self.pkg-config self.zauth ];
    };
  };

  nginz = super.nginx.override {
    modules = [
      self.nginxModules.vts
      self.nginxModules.moreheaders
      self.nginxModules.zauth
    ];
  };
}
