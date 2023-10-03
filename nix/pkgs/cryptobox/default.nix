{ fetchFromGitHub
, lib
, libsodium
, pkg-config
, rustPlatform
, stdenv
}:

rustPlatform.buildRustPackage rec {
  pname = "cryptobox-c";
  version = "2019-06-17";
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ libsodium ];
  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "cryptobox-c";
    rev = "4067ad96b125942545dbdec8c1a89f1e1b65d013";
    sha256 = "1i9dlhw0xk1viglyhail9fb36v1awrypps8jmhrkz8k1bhx98ci3";
  };
  patchLibs = lib.optionalString stdenv.isDarwin ''
    install_name_tool -id $out/lib/libcryptobox.dylib $out/lib/libcryptobox.dylib
  '';

  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "cryptobox-1.0.0" = "sha256-Ewo+FtEGTZ4/U7Ow6mGTQkxS4IQYcEthr5/xG9BRTWk=";
      "hkdf-0.2.0" = "sha256-cdgR94c40JFIjBf8NfZPXPGLU60BlAZX/SQnRHAXGOg=";
      "proteus-1.0.0" = "sha256-ppMt56RY5K3rOwO7MEdY6d3t96sbHZzDB/nPNNp35DY=";
    };
  };

  postInstall = ''
    ${patchLibs}
    mkdir -p $out/include
    cp src/cbox.h $out/include
  '';
}
