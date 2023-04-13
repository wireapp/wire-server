{ fetchFromGitHub
, lib
, libsodium
, nix-gitignore
, pkg-config
, rustPlatform
, stdenv
, gitMinimal
}:

rustPlatform.buildRustPackage rec {
  name = "libzauth-${version}";
  version = "3.0.0";
  nativeBuildInputs = [ pkg-config gitMinimal ];
  buildInputs = [ libsodium ];
  src = nix-gitignore.gitignoreSourcePure [ ../../../.gitignore ] ../../../libs/libzauth;
  sourceRoot = "libzauth/libzauth-c";

  cargoSha256 = "sha256-f/MNUrEQaPzSUHtnZ0jARMwBswS+Sh0Swe+2D+hpHF4=";

  patchLibs = lib.optionalString stdenv.isDarwin ''
    install_name_tool -id $out/lib/libzauth.dylib $out/lib/libzauth.dylib
  '';

  postInstall = ''
    mkdir -p $out/lib/pkgconfig
    mkdir -p $out/include
    cp src/zauth.h $out/include
    sed -e "s~<<VERSION>>~${version}~" \
      -e "s~<<PREFIX>>~$out~" \
      src/libzauth.pc > $out/lib/pkgconfig/libzauth.pc
    cp target/release-tmp/libzauth.* $out/lib/
    ${patchLibs}
  '';
}
