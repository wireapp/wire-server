self: super: {
  # TODO: Do not use buildRustPackage. Ces't horrible
  cryptobox = self.callPackage (
    { fetchFromGitHub, rustPlatform, pkgconfig, libsodium }:
      rustPlatform.buildRustPackage rec {
        name = "cryptobox-c-${version}";
        version = "2019-06-17";
        nativeBuildInputs = [ pkgconfig ];
        buildInputs = [ libsodium ];
        src = fetchFromGitHub {
          owner = "wireapp";
          repo = "cryptobox-c";
          rev = "4067ad96b125942545dbdec8c1a89f1e1b65d013";
          sha256 = "1i9dlhw0xk1viglyhail9fb36v1awrypps8jmhrkz8k1bhx98ci3";
        };
        cargoSha256 = "0zs8ibv7rinrrzp9naxd7yak7kn1gp3pjb3g8i4wf7xw2hkkq81z";
        postInstall = ''
          mkdir -p $out/include
          cp src/cbox.h $out/include
        '';
      }
  ) {};

  zauth = self.callPackage (
    { fetchFromGitHub, rustPlatform, pkgconfig, libsodium }:
      rustPlatform.buildRustPackage rec {
        name = "libzauth-${version}";
        version = "3.0.0";
        nativeBuildInputs = [ pkgconfig ];
        buildInputs = [ libsodium ];
        src = self.nix-gitignore.gitignoreSourcePure [ ../../.gitignore ] ../../libs/libzauth;
        sourceRoot = "libzauth/libzauth-c";

        cargoSha256 = "10ijvi3rnnqpy589hhhp8s4p7xfpsbb1c3mzqnf65ra96q4nd6bf"; # self.lib.fakeSha256;
        postInstall = ''
          mkdir -p $out/lib/pkgconfig
          mkdir -p $out/include
          cp src/zauth.h $out/include
          sed -e "s~<<VERSION>>~${version}~" \
            -e "s~<<PREFIX>>~$out~" \
            src/libzauth.pc > $out/lib/pkgconfig/libzauth.pc
          cp target/release-tmp/libzauth.so $out/lib/
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
