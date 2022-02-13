{ fetchFromGitHub
, lib
, libsodium
, pkg-config
, rustPlatform
, stdenv
}:

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
  cargoSha256 = "sha256-Afr3ShCXDCwTQNdeCZbA5/aosRt+KFpGfT1mrob6cog=";

  patchLibs = lib.optionalString stdenv.isDarwin ''
    install_name_tool -id $out/lib/libcryptobox.dylib $out/lib/libcryptobox.dylib
  '';

  postInstall = ''
    ${patchLibs}
    mkdir -p $out/include
    cp src/cbox.h $out/include
  '';
}
