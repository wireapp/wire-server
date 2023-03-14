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
{ lib, fetchgit }: hself: hsuper:
let
  gitPins = {
    transitive-anns = {
      src = fetchgit {
        url = "https://github.com/wireapp/transitive-anns";
        rev = "c3bdc423f84bf15fe8b3618b5dddd5764fc8a470";
        sha256 = "sha256-mWBZ2uY0shlxNRceyC2Zu1f3Kr4IDtT/rOL7CKWgilA=";
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
    hsaml2 = {
      src = fetchgit {
        url = "https://github.com/wireapp/hsaml2";
        rev = "d43818aac56678f0be02d0101d224fe0f6cdf131";
        sha256 = "16hj3i4h5rwhr8kqrs7345wg7v10ahwjd3fdp2qx3c5z4qls6prr";
      };
    };
    http-client = {
      src = fetchgit {
        url = "https://github.com/wireapp/http-client";
        rev = "9100baeddbd15d93dc58a826ae812dafff29d5fd";
        sha256 = "16n340bg5vdb169f6d6421hx13wyqdsb5b314r823v34r8p0b19z";
      };
      packages = {
        http-client = "http-client";
        http-client-openssl = "http-client-openssl";
        http-client-tls = "http-client-tls";
        http-conduit = "http-conduit";
      };
    };
    http2 = {
      src = fetchgit {
        url = "https://github.com/wireapp/http2";
        rev = "aa3501ad58e1abbd196781fac25a84f41ec2a787";
        sha256 = "09h86fkk8p7szq08x0iszaq16mhbylxivfc0apvj58d98wl8l6lq";
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
        rev = "b79a45ac98b1f592ac18511fce48ed88d2e931c9";
        sha256 = "sha256-g2lbKt3+hToVFQvaHOa9dg4HqAL7YgReo8fy7wQavmY=";
      };
    };
    swagger2 = {
      src = fetchgit {
        url = "https://github.com/GetShopTV/swagger2";
        rev = "d79deca03b714cdd4531217831a8305068b2e8f9";
        sha256 = "sha256-R3p0L0TgM0Bspe5z6vauwdPq9TmEWpMC53DBkMtCEoE=";
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
    # This can be removed once postie 0.6.0.3 (or later) is in nixpkgs
    postie = {
      src = fetchgit {
        url = "https://github.com/alexbiehl/postie.git";
        rev = "c92702386f760fcaa65cd052dc8114889c001e3f";
        sha256 = "sha256-yiw6hg3guRWS6CVdrUY8wyIDxoqfGjIVMrEtP+Fys0Y=";
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
    tinylog = {
      src = fetchgit {
        url = "https://gitlab.com/wireapp/forks/tinylog.git";
        rev = "9609104263e8cd2a631417c1c3ef23e090de0d09";
        sha256 = "sha256-htEIJY+LmIMACVZrflU60+X42/g14NxUyFM7VJs4E6w=";
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
    HsOpenSSL = {
      version = "0.11.7.5";
      sha256 = "sha256-CfH1YJSGuF4O1aUfdJwUZKRrVzv5nSPhwoI7mf9ewEg=";
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
