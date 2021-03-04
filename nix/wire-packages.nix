# Nix package definitions for all things wire-server
final: prev: {

  # TODO: Do not use buildRustPackage. Ces't horrible
  cryptobox = final.callPackage
    (
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

        patchLibs = prev.lib.optionalString prev.stdenv.isDarwin ''
          install_name_tool -id $out/lib/libcryptobox.dylib $out/lib/libcryptobox.dylib
        '';

        postInstall = ''
          ${patchLibs}
          mkdir -p $out/include
          cp src/cbox.h $out/include
        '';
      }
    )
    { };

  zauth = final.callPackage
    (
      { fetchFromGitHub, rustPlatform, pkg-config, libsodium }:
      rustPlatform.buildRustPackage rec {
        name = "libzauth-${version}";
        version = "3.0.0";
        nativeBuildInputs = [ pkg-config ];
        buildInputs = [ libsodium ];
        src = final.nix-gitignore.gitignoreSourcePure [ ../../.gitignore ] ../../libs/libzauth;
        sourceRoot = "libzauth/libzauth-c";

        cargoSha256 = "10ijvi3rnnqpy589hhhp8s4p7xfpsbb1c3mzqnf65ra96q4nd6bf"; # final.lib.fakeSha256;

        patchLibs = prev.lib.optionalString prev.stdenv.isDarwin ''
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
    )
    { };

  nginxModules = prev.nginxModules // {
    zauth = {
      src = ../../services/nginz/third_party/nginx-zauth-module;
      inputs = [ final.pkg-config final.zauth ];
    };
  };

  nginz = prev.nginx.override {
    modules = [
      final.nginxModules.vts
      final.nginxModules.moreheaders
      final.nginxModules.zauth
    ];
  };

  wireHaskellPkgs = final.haskell-nix.stackProject {
    src = final.haskell-nix.haskellLib.cleanGit {
      name = "wire-server";
      src = ../.;
    };

    # Lets avoid hpack as we have cabal files comitted
    ignorePackageYaml = true;

    # in stack.yaml only the commit to fetch is given; but this is ambigious as
    # apparently nix also needs the branch on which this commit is reachable.
    # (By default it uses HEAD; and if your commit isn't reachable from HEAD it
    # isn't fetchable!)t
    branchMap = {
      "https://github.com/wireapp/hspec-wai"."0a5142cd3ba48116ff059c041348b817fb7bdb25" = "body-contains";
      "https://github.com/wireapp/bloodhound"."92de9aa632d590f288a353d03591c38ba72b3cb3" = "wire-fork-ghc-8.8";
      "https://github.com/wireapp/hsaml2"."2ff7b0c11a9d510f1ec411f436bc134b216ebd4a" = "akshaymankar/pull-upstream-2020-08-18";
      "https://github.com/wireapp/servant-swagger.git"."23e9afafadaade29d21181b935286087457171e3" = "akshaymankar/fix-ghc-version-check";
    };

    modules = [
      # Needed because `build-tools` in cabal can refer to either a hackage
      # package or a system package and if there is a name-clash it is
      # ambigious. haskell.nix in that case chooses the hackage package instead
      # which is exactly what we do not want. We want the protobuf c library
      { packages.types-common-journal.components.library.build-tools = [ final.protobuf ]; }
    ];
  };

  # Given a haskell package name; builds a container with all the executables
  # that that haskell package exposes
  buildHaskellContainer = name: prev.dockerTools.buildLayeredImage {
    inherit name;
    contents = prev.lib.attrValues final.wireHaskellPkgs.${name}.components.exes;
  };
  wireContainers = prev.lib.genAttrs [ "brig" "galley" "cannon" "gundeck" "cargohold" ] final.buildHaskellContainer;
}
