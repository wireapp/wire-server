self: super: {
  # text-icu-translit.cabal refers to this by this name
  icuuc = self.icu;
  icui18n = self.icu;
  icudata = self.icu;
  GeoIP = self.geoip;

  # TODO: Do not use buildRustPackage. Ces't horrible
  cryptobox = self.callPackage (
    { fetchFromGitHub, rustPlatform, pkgconfig, libsodium }:
      rustPlatform.buildRustPackage rec {
        name = "cryptobox-c-${version}";
        version = "2019-06-17";
        buildInputs = [ pkgconfig libsodium ];
        src = fetchFromGitHub {
          owner = "wireapp";
          repo = "cryptobox-c";
          rev = "4067ad96b125942545dbdec8c1a89f1e1b65d013";
          sha256 = "1i9dlhw0xk1viglyhail9fb36v1awrypps8jmhrkz8k1bhx98ci3";
        };
        cargoSha256 = "0m85c49hvvxxv7jdipfcaydy4n8iw4h6myzv63v7qc0fxnp1vfm8";
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
        buildInputs = [ libsodium pkgconfig ];
        src = ../../libs/libzauth;

        sourceRoot = "libzauth/libzauth-c";

        cargoSha256 = "01yj1rchqmjnpj5cb9wl7vdzrycjwjhm60xh1jghw02n8jhl51p2"; # 2self.lib.fakeSha256;
        postInstall = ''
          mkdir -p $out/lib/pkgconfig
          mkdir -p $out/include
          cp src/zauth.h $out/include
          sed -e "s~<<VERSION>>~${version}~" \
            -e "s~<<PREFIX>>~$out~" \
            src/libzauth.pc > $out/lib/pkgconfig/libzauth.pc
          cp target/release/libzauth.so $out/lib/
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
