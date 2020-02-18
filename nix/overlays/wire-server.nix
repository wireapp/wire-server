self: super: {
  # text-icu-translit.cabal refers to this by this name
  icuuc = self.icu;
  icui18n = self.icu;
  icudata = self.icu;
  cryptobox = self.callPackage ({fetchFromGitHub, rustPlatform,  pkgconfig, libsodium}:
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
    }) {};
}
