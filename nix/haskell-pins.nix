# How to add a git pin:
#
# 1. If your target git repository has only package with the cabal file at the
# root, add it like this under 'gitPins':
#     <name-of-the-package> = {
#       src = fetchgit {
#          url = "<https-url-to-git>";
#          rev = "<commit-id/sha>";
#          sha256 = "";
#       };
#     };
#
# 2. If your target git repsitory has many packages, add it like this under 'gitPins':
#
#    <name-of-git-repo> = {
#      src = fetchgit {
#        url = "<https-url-to-git>";
#        rev = "<commit-id/sha>";
#        sha256 = "";
#      };
#      packages =  {
#        <name-of-package1> = "<relative-path-to-package1>";
#        <name-of-package2> = "<relative-path-to-package2>";
#        <name-of-package3> = "<relative-path-to-package3>";
#      };
#    };
#
# 3. Run 'nix build -f ./nix wireServer.haskellPackagesUnoptimizedNoDocs.<your-packge-name>'.
# This should produce an error saying expected sha <something with a lot of
# 'A's> and the actual sha. Replace the empty string in 'sha256' with the actual
# sha.
#
# How to update a git pin:
#
# 1. Determine the new commit ID/SHA of the git repository that you want to pin
# and update the 'rev' field of the pin under 'gitPins'.
#
# 2. Update 'sha256' field under `fetchgit` to be an empty string.  (This step is optional:
# since the hash has changed, the error will be the same if you remove it or if you leave the
# old value in place.)
#
# 3. Run step 3. from how to add a git pin.
#
# How to add a hackage pin:
#
# 1. Add your package like this, under 'hackagePins':
#    <package-name> = {
#      version = "<version>";
#      sha256 = "sha256-gD9b9AXpLkpPSAeg8oPBU7tsHtSNQjxIZKBo+7+r3+c=";
#    };
#
# 2. Run step 3. from how to add a git pin.
#
# How to update a hackage pin:
#
# 1. Update version number.
# 2. Make the 'sha256' blank string.
# 3. Run step 3. from how to add a git pin.
{ lib, fetchgit, pkgs }: hself: hsuper:
let
  gitPins = {
    transitive-anns = {
      src = fetchgit {
        url = "https://github.com/wireapp/transitive-anns";
        rev = "7caf82f8d1be0f994a557e0cdc87fde8e32d5420";
        sha256 = "sha256-rDIAbYpNGMBDOOE1hqLneRSVkCnj3cCQVYGKkhw8t7w=";
      };
    };
    amqp = {
      src = fetchgit {
        url = "https://github.com/hreinhardt/amqp";
        rev = "b5dfe4362b14b58d51ec306d6871d347751f3d47";
        sha256 = "sha256-ov85XFztGM0mEoj01lRZN9xYJttKa/crPnp0lh4A5DA=";
      };
    };
    amazonka = {
      src = fetchgit {
        url = "https://github.com/brendanhay/amazonka";
        rev = "cfe2584aef0b03c86650372d362c74f237925d8c";
        sha256 = "sha256-ss8IuIN0BbS6LMjlaFmUdxUqQu+IHsA8ucsjxXJwbyg=";
      };
      packages = {
        amazonka = "lib/amazonka";
        amazonka-core = "lib/amazonka-core";
        amazonka-cloudfront = "lib/services/amazonka-cloudfront";
        amazonka-dynamodb = "lib/services/amazonka-dynamodb";
        amazonka-s3 = "lib/services/amazonka-s3";
        amazonka-ses = "lib/services/amazonka-ses";
        amazonka-sns = "lib/services/amazonka-sns";
        amazonka-sqs = "lib/services/amazonka-sqs";
        amazonka-sso = "lib/services/amazonka-sso";
        amazonka-sts = "lib/services/amazonka-sts";
      };
    };
    bloodhound = {
      src = fetchgit {
        url = "https://github.com/wireapp/bloodhound";
        rev = "abf819a4a6ec7601f1e58cb8da13b2fdad377d9e";
        sha256 = "sha256-m1O+F/mOJN5z5WNChmeyHP4dtmLRkl2YnLlTuwzRelk=";
      };
    };
    cryptobox-haskell = {
      src = fetchgit {
        url = "https://github.com/wireapp/cryptobox-haskell";
        rev = "7546a1a25635ef65183e3d44c1052285e8401608";
        sha256 = "0dgizj1kc135yzzqdf5l7f5ax0qpvrr8mxvg7s1dbm01cf11aqzn";
      };
    };
    HaskellNet-SSL = {
      src = fetchgit {
        url = "https://github.com/MangoIV/HaskellNet-SSL";
        rev = "c2844b63a39f458ffbfe62f2ac824017f1f84453";
        sha256 = "sha256-1mu/yEAWr3POY4MHRomum0DDvs5Qty1JvP3v5GS2u64=";
      };
    };
    hsaml2 = {
      src = fetchgit {
        url = "https://github.com/wireapp/hsaml2";
        rev = "51d1fcecebf2417e658b9a78943c84a76a0ed347";
        sha256 = "sha256-jYJBhXBQ1MTLPI8JsiF2XUtgDxK+eniavNB2B1zaSQg=";
      };
    };
    http-client = {
      src = fetchgit {
        url = "https://github.com/wireapp/http-client";
        rev = "eabf64b4a8ff4c0fe6a3b39cb0f396ba8c2fb236";
        sha256 = "sha256-8NPRVDlul9Xnj6IyUOUe6w7fDt/5WWZNjR07CaAp/Kk=";
      };
      packages = {
        http-client = "http-client";
        http-client-openssl = "http-client-openssl";
        http-client-tls = "http-client-tls";
        http-conduit = "http-conduit";
      };
    };
    hspec-wai = {
      src = fetchgit {
        url = "https://github.com/wireapp/hspec-wai";
        rev = "6984a06b0c6294677c49d59382d48f975a8733d4";
        sha256 = "sha256-6FLTMMqvL0xFa5zsMnjVAmdpghmdeBl813bWcOyQo5E=";
      };
    };
    saml2-web-sso = {
      src = fetchgit {
        url = "https://github.com/wireapp/saml2-web-sso";
        rev = "ac46ea888026711860cf784b5bda206873c87333";
        sha256 = "sha256-IKovI1h2Wkm3Y7Sz6XsxLOv654SgUasaWsDX6gi9hZw=";
      };
    };
    # MR: https://gitlab.com/twittner/cql-io/-/merge_requests/20
    cql-io = {
      src = fetchgit {
        url = "https://gitlab.com/wireapp/forks/cql-io";
        rev = "c2b6aa995b5817ed7c78c53f72d5aa586ef87c36";
        sha256 = "sha256-DMRWUq4yorG5QFw2ZyF/DWnRjfnzGupx0njTiOyLzPI=";
      };
    };
    wai-predicates = {
      src = fetchgit {
        url = "https://gitlab.com/wireapp/forks/wai-predicates.git";
        rev = "ff95282a982ab45cced70656475eaf2cefaa26ea";
        sha256 = "sha256-x2XSv2+/+DG9FXN8hfUWGNIO7V4iBhlzYz19WWKaLKQ=";
      };
    };
    wai-routing = {
      src = fetchgit {
        url = "https://gitlab.com/twittner/wai-routing";
        rev = "7e996a93fec5901767f845a50316b3c18e51a61d";
        sha256 = "18icwks9jc6sy42vcvj2ysaip2s0dsrpvm9sy608b6nq6kk1ahlk";
      };
    };
    # PR: https://github.com/UnkindPartition/tasty/pull/351
    tasty = {
      src = fetchgit {
        url = "https://github.com/wireapp/tasty";
        rev = "394943c7672e5ad269e5587528b7678caf3b0720";
        sha256 = "sha256-rKvWGCLJUyKPmzeYaTj5J0VAExXYIpwJ5J2lJTcuXJI=";
      };
      packages = {
        tasty-hunit = "hunit";
      };
    };
    jose = {
      src = fetchgit {
        url = "https://github.com/frasertweedale/hs-jose";
        rev = "a7f919b19f667dfbb4d5c989ce620d3e75af8247";
        sha256 = "sha256-SKEE9ZqhjBxHYUKQaoB4IpN4/Ui3tS4S98FgZqj7WlY=";
      };
    };
    servant-openapi3 = {
      src = fetchgit {
        # This is a patched version of the library that sets the required flag for HTTP request bodies.
        # A PR for these changes has been made for the upstream library. biocad/servant-openapi3#49
        url = "https://github.com/lepsa/servant-openapi3";
        rev = "5cdb2783f15058f753c41b800415d4ba1149a78b";
        sha256 = "sha256-8FM3IAA3ewCuv9Mar8aWmzbyfKK9eLXIJPMHzmYb1zE=";
      };
    };
    # This can be removed once postie with TLS 1.9 is on nixpkgs.
    # https://github.com/alexbiehl/postie/pull/4
    postie = {
      src = fetchgit {
        url = "https://github.com/wireapp/postie.git";
        rev = "43b6d1d21d56e567077c194d49efb92e777e7628";
        sha256 = "sha256-DKugy4EpRsSgaGvybdh2tLa7HCtoxId+7RAAAw43llA=";
      };
    };
    # Not tested/relased yet
    # https://github.com/dylex/invertible/commit/e203c6a729fde87b1f903c3f468f739a085fb446
    invertible = {
      src = fetchgit {
        url = "https://github.com/dylex/invertible.git";
        rev = "e203c6a729fde87b1f903c3f468f739a085fb446";
        sha256 = "sha256-G6PX5lpU18oWLkwIityN4Hs0HuwQrq9T51kxbsdpK3M=";
      };
    };
    tls = {
      src = fetchTarball {
        url = "https://hackage.haskell.org/package/tls-1.9.0/tls-1.9.0.tar.gz";
        sha256 = "sha256:1gyc6yfygswg4pjj9hxw3pashq56viivf8m321b4f0bsd2yf372s";
      };
    };
    tinylog = {
      src = fetchgit {
        url = "https://gitlab.com/wireapp/forks/tinylog.git";
        rev = "9609104263e8cd2a631417c1c3ef23e090de0d09";
        sha256 = "sha256-htEIJY+LmIMACVZrflU60+X42/g14NxUyFM7VJs4E6w=";
      };
    };
    # PR: https://github.com/ocharles/tasty-ant-xml/pull/32
    tasty-ant-xml = {
      src = fetchgit {
        url = "https://github.com/akshaymankar/tasty-ant-xml";
        rev = "34ff294d805e62e73678dccc0be9d3da13540fbe";
        sha256 = "sha256-+rHcS+BwEFsXqPAHX/KZDIgv9zfk1dZl0LlZJ57Com4=";
      };
    };

    text-icu-translit = {
      src = pkgs.fetchFromGitHub {
        owner = "wireapp";
        repo = "text-icu-translit";
        rev = "317bbd27ea5ae4e7f93836ee9ca664f9bde7c583";
        hash = "sha256-9uVqUTkLkE7U19FDjn5xt8JEHyJmosLPSnmW7kYbe5w=";
      };
    };

    # PR at https://github.com/google/ghc-source-gen/pull/102
    ghc-source-gen = {
      version = "0.4.4.0";
      src = pkgs.fetchFromGitHub {
        owner = "circuithub";
        repo = "ghc-source-gen";
        rev = "7a6aac047b706508e85ba2054b5bedbecfd7eb7a";
        hash = "sha256-DZu3XAOYLKcSpOYhjpb6IuXMvRHtGohTkL0nsCb/dT0=";
      };
    };
    hoogle = {
      src = fetchgit {
        url = "https://github.com/ndmitchell/hoogle";
        rev = "0be38ee5e078e31ef7eabeaba255aed12ce7055d";
        sha256 = "sha256-xcGZ11ocdlB8ks20QAhtPZ+4ggmV4Om4CPHH/M6NjXk=";
      };
    };
  };
  hackagePins = {
    # Major re-write upstream, we should get rid of this dependency rather than
    # adapt to upstream.
    wai-route = {
      version = "0.4.0";
      sha256 = "sha256-DSMckKIeVE/buSMg8Mq+mUm1bYPYB7veA11Ns7vTBbc=";
    };
    polysemy = {
      version = "1.8.0.0";
      sha256 = "sha256-AdxxKWXdUjZiHLDj6iswMWpycs7mFB8eKhBR4ljF6kk=";
    };
    hpack = {
      version = "0.36.0";
      sha256 = "sha256-a8jKkzO3CWIoBg+Uaw5TtpDwmeajWCTW1zJNrlpBKPU=";
    };
    HsOpenSSL = {
      version = "0.11.7.5";
      sha256 = "sha256-CfH1YJSGuF4O1aUfdJwUZKRrVzv5nSPhwoI7mf9ewEg=";
    };
    http2 = {
      version = "4.1.0";
      sha256 = "sha256-D6RWYBguoj+W1LwNeX04h4csXV69rrs0tZpeNr7ZBqE=";
    };
    network-conduit-tls = {
      version = "1.4.0";
      sha256 = "sha256-zPT/FMxAiR94NReqNIDa/RS7dtiNWCRe3SZi8P11GDk=";
    };
    warp = {
      version = "3.3.30";
      sha256 = "sha256-VrK27a2wFtezh9qabcXGe2tw9EwBmI8mKwmpCtXq9rc=";
    };
    warp-tls = {
      version = "3.4.3";
      sha256 = "sha256-6MjlCKGC8v+7OiSuMFGwO8sgcA3gp0OfOnneI2wSpWI=";
    };
    optparse-generic = {
      version = "1.5.1";
      sha256 = "sha256-TS3T6AtYfdzmPkG6SwvN/tr2Vdr4eTdGRRH2Xbd8fzM=";
    };
    crypton-connection = {
      version = "0.3.1";
      sha256 = "sha256-TrRdD56cNIXMlDrHBO0VxQYkJ30pRXe4yVkEILsbMro=";
    };
  };
  # Name -> Source -> Maybe Subpath -> Drv
  mkGitDrv = name: src: subpath:
    let
      subpathArg =
        if subpath == null
        then ""
        else "--subpath='${subpath}'";
    in
    hself.callCabal2nixWithOptions name src "${subpathArg}" { };
  # [[AtrrSet]]
  gitPackages = lib.attrsets.mapAttrsToList
    (name: pin:
      let
        packages =
          if pin?packages
          then pin.packages
          else { "${name}" = null; };
      in
      lib.attrsets.mapAttrsToList
        (name: subpath:
          { "${name}" = mkGitDrv name pin.src subpath; }
        )
        packages
    )
    gitPins;
  # AttrSet
  hackagePackages = lib.attrsets.mapAttrs
    (pkg: { version, sha256 }:
      hself.callHackageDirect
        {
          ver = version;
          inherit pkg sha256;
        }
        { }
    )
    hackagePins;
in
lib.lists.foldr (a: b: a // b) hackagePackages (lib.lists.flatten gitPackages)
