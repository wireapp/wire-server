# How to add a git pin:
#
# 1. If your target git repository has only package with the cabal file at the
# root, add it like this under 'gitPins':
#     <name-of-the-package> = {
#       src = fetchgit = {
#          url = "<https-url-to-git>"
#          rev = "<commit-id/sha>";
#          sha256 = "";
#       }
#     }
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
# 2. Update 'sha256' field under `fetchgit` to be an empty string.
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
    HaskellNet-SSL = {
      src = fetchgit {
        url = "https://github.com/dpwright/HaskellNet-SSL";
        rev = "ca84ef29a93eaef7673fa58056cdd8dae1568d2d";
        sha256 = "1w23xgjdq22px90p12yw30psagc668n7l183bqvf8x075s77wckr";
      };
    };
    prometheus-haskell = {
      src = fetchgit {
        url = "https://github.com/fimad/prometheus-haskell";
        rev = "2e3282e5fb27ba8d989c271a0a989823fad7ec43";
        sha256 = "0vfzysn9sgpxymfvpahxrp74fczgjnw3kgknj6zk0473qk85488f";
      };
      packages = {
        wai-middleware-prometheus = "wai-middleware-prometheus";
      };
    };
    hs-collectd = {
      src = fetchgit {
        url = "https://github.com/kim/hs-collectd";
        rev = "885da222be2375f78c7be36127620ed772b677c9";
        sha256 = "1a3jwj0h2vzgjvzaa4jghmxkjwbzisq4qc7dldc42zi4jaq7lix7";
      };
    };
    hs-certificate = {
      src = fetchgit {
        url = "https://github.com/vincenthz/hs-certificate";
        rev = "a899bda3d7666d25143be7be8f3105fc076703d9";
        sha256 = "0ivc4l3c272i7w37rfgsbwnxa3fzfmghwddlqvzj5jj3zx5lyqlk";
      };
      packages = {
        x509-store = "x509-store";
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
    multihash = {
      src = fetchgit {
        url = "https://github.com/wireapp/haskell-multihash.git";
        rev = "300a6f46384bfca33e545c8bab52ef3717452d12";
        sha256 = "0lcm6argp49fin4va7c50l1lj84xcm3cqzijzssfdgplimpmllma";
      };
    };
    hsaml2 = {
      src = fetchgit {
        url = "https://github.com/wireapp/hsaml2";
        rev = "d43818aac56678f0be02d0101d224fe0f6cdf131";
        sha256 = "16hj3i4h5rwhr8kqrs7345wg7v10ahwjd3fdp2qx3c5z4qls6prr";
      };
    };
    hspec-wai = {
      src = fetchgit {
        url = "https://github.com/wireapp/hspec-wai";
        rev = "6984a06b0c6294677c49d59382d48f975a8733d4";
        sha256 = "sha256-6FLTMMqvL0xFa5zsMnjVAmdpghmdeBl813bWcOyQo5E=";
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
    saml2-web-sso = {
      src = fetchgit {
        url = "https://github.com/wireapp/saml2-web-sso";
        rev = "b79a45ac98b1f592ac18511fce48ed88d2e931c9";
        sha256 = "sha256-g2lbKt3+hToVFQvaHOa9dg4HqAL7YgReo8fy7wQavmY=";
      };
    };
    swagger2 = {
      src = fetchgit {
        url = "https://github.com/wireapp/swagger2";
        rev = "ba916df2775bb38ec603b726bbebfb65a908317a";
        sha256 = "sha256-IcsrJ5ur8Zm7Xp1PQBOb+2N7T8WMI8jJ6YuDv8ypsPQ=";
      };
    };
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
    # PR: https://github.com/haskell-cryptography/HsOpenSSL/pull/76
    HsOpenSSL = {
      src = fetchgit {
        url = "https://github.com/wireapp/HsOpenSSL";
        rev = "b36c063d44b8b8732f0e6334ad58d2235fedd92e";
        sha256 = "sha256-NdWzTqWDQvCwyp3la3RGN9D1lGaLHHrxQuFxpk9R+lQ=";
      };
    };
  };
  hackagePins = {
    wai-route = {
      version = "0.4.0";
      sha256 = "sha256-DSMckKIeVE/buSMg8Mq+mUm1bYPYB7veA11Ns7vTBbc=";
    };
    kind-generics = {
      version = "0.4.1.4";
      sha256 = "sha256-vH4YO/2NtaLDn1pyAQYK9vC5dD0bpdk26aH5sA9+UT8=";
    };
    kind-generics-th = {
      version = "0.2.2.3";
      sha256 = "sha256-LH2Wpo0v+RQSsvVbosuB99ekSzqsqMNjrd+w/B0SOUs=";
    };
    polysemy = {
      version = "1.8.0.0";
      sha256 = "sha256-AdxxKWXdUjZiHLDj6iswMWpycs7mFB8eKhBR4ljF6kk=";
    };
    polysemy-check = {
      version = "0.9.0.1";
      sha256 = "sha256-CsL2vMxAmpvVVR/iUnZAkbcRLiy/a8ulJQ6QwtCYmRM=";
    };
    polysemy-plugin = {
      version = "0.4.3.1";
      sha256 = "sha256-0vkLYNZISr3fmmQvD8qdLkn2GHc80l1GzJuOmqjqXE4=";
    };
    one-liner = {
      version = "1.0";
      sha256 = "sha256-dv/W8hIPoHVevxiiCb6OfeP53O/9HPgUiqOHGSNb/pk=";
    };
    singletons = {
      version = "3.0.1";
      sha256 = "sha256-ixHWZae6AxjRUldMgpYolXBGsOUT5ZVIw9HZkxrhHQ0=";
    };
    singletons-base = {
      version = "3.1";
      sha256 = "sha256-SjpkQofdDMrUMi9cHNF5eyqic7WMAhWNqrKr4ip1RNs=";
    };
    singletons-th = {
      version = "3.1";
      sha256 = "sha256-34nyestxt8KNTSlmr1Y8nElNXa/wZ1+fuLEUVjZX8dk=";
    };
    th-desugar = {
      version = "1.13";
      sha256 = "sha256-xiAeSM2umcfsz5+mcW+oGKf/EmzvH0ch0lHoKBGzW9I=";
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
