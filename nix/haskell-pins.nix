{lib, fetchgit}: hself: hsuper:
let
  gitPins = {
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
      packages =  {
        wai-middleware-prometheus = "wai-middleware-prometheus";
      };
    };
    servant = {
      src = fetchgit {
        url = "https://github.com/haskell-servant/servant.git";
        rev = "75db4a5327d6d04ae2460bd5ffd008fe16197ba8";
        sha256 = "0khgk0iqvamph57qp86ilravaw76qnjmg4kpliwfdzfyj9h44w0l";
      };
      packages = {
        servant = "servant";
        servant-client = "servant-client";
        servant-client-core = "servant-client-core";
        servant-server = "servant-server";
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
      };};
    amazonka = {
      src = fetchgit {
        url = "https://github.com/wireapp/amazonka";
        rev = "7ced54b0396296307b9871d293cc0ac161e5743d";
        sha256 = "0md658m32zrvzc8nljn58r8iw4rqxpihgdnqrhl8vnmkq6i9np51";
      };
      packages = {
        amazonka = "lib/amazonka";
        amazonka-core = "lib/amazonka-core";
        amazonka-test = "lib/amazonka-test";
        amazonka-cloudfront = "lib/services/amazonka-cloudfront";
        amazonka-dynamodb = "lib/services/amazonka-dynamodb";
        amazonka-s3 = "lib/services/amazonka-s3";
        amazonka-ses = "lib/services/amazonka-ses";
        amazonka-sns = "lib/services/amazonka-sns";
        amazonka-sqs = "lib/services/amazonka-sqs";
        amazonka-sso = "lib/services/amazonka-sso";
        amazonka-sts = "lib/services/amazonka-sts";
      };};
    bloodhound = {
      src = fetchgit {
        url = "https://github.com/wireapp/bloodhound";
        rev = "c68e6d877d87988331bf391ed16780383a58eefa";
        sha256 = "0fr5xgq8f1nmcbk8srrhyf4vad4xm5iqr974jgqfg1mg31y85h0x";
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
        rev = "0a5142cd3ba48116ff059c041348b817fb7bdb25";
        sha256 = "1yqkla7467fgb23yw689xh15zjn38rkc7ckf18cfalpc2ff5wfq1";
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
      };};
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
        rev = "74371cd775cb98d6cf85f6e182244a3c4fd48702";
        sha256 = "1w23yz2iiayniymk7k4g8gww7268187cayw0c8m3bz2hbnvbyfbc";
      };
    };
    cql-io = {
      src = fetchgit {
        url = "https://gitlab.com/axeman/cql-io";
        rev = "c2b6aa995b5817ed7c78c53f72d5aa586ef87c36";
        sha256 = "1wncign8ilvqs9qyl6pkz66x2s8dgwhnfdjw82wv38ijmr95di0c";
      };
    };
    swagger = {
      src = fetchgit {
        url = "https://gitlab.com/axeman/swagger";
        rev = "e2d3f5b5274b8d8d301b5377b0af4319cea73f9e";
        sha256 = "1zj3fqlvcvp9s0myb98b6s9mpmiqamyxn2d3jan55irdgm53prmv";
      };
    };
    wai-routing = {
      src = fetchgit {
        url = "https://gitlab.com/twittner/wai-routing";
        rev = "7e996a93fec5901767f845a50316b3c18e51a61d";
        sha256 = "18icwks9jc6sy42vcvj2ysaip2s0dsrpvm9sy608b6nq6kk1ahlk";
      };
    };
    snappy = {
      src = fetchgit {
        url = "https://github.com/wireapp/snappy";
        rev = "b0e5c08af48911caecffa4fa6a3e74872018b258";
        sha256 = "1d1x0kkh2p4mb29wi31wpffgr64i27jd10ci70i81y5fwn44c542";
      };
    };
    snappy-framing = {
      src = fetchgit {
        url = "https://github.com/kim/snappy-framing";
        rev = "d99f702c0086729efd6848dea8a01e5266c3a61c";
        sha256 = "04z8qw5jaw58c09bf29w5k8hp7xa5w69c14ad672crw8zgsw7860";
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
  };
  # TODO: This depends on <some-hardcoded-list-of-sha56sum-for-each-package-version-combo>, make it not depend on it.
  hackagePins = {
    kind-generics = "0.4.1.0";
    wai-route = "0.4.0";
    partial-isomorphisms = "0.2.2.1";
    singletons = "2.7";
    th-desugar = "1.11";
    one-liner = "1.0";
  };
  # Name -> Source -> Maybe Subpath -> Drv
  mkGitDrv = name: src: subpath:
    let subpathArg = if subpath == null
                     then ""
                     else "--subpath='${subpath}'";
    in hself.callCabal2nixWithOptions name src "${subpathArg}" {};
  # [[AtrrSet]]
  gitPackages = lib.attrsets.mapAttrsToList (name: pin:
    let packages = if pin?packages
                   then pin.packages
                   else { "${name}" = null;};
    in lib.attrsets.mapAttrsToList (name: subpath:
      {"${name}" = mkGitDrv name pin.src subpath;}
    ) packages
  ) gitPins;
  # AttrSet
  hackagePackages = lib.attrsets.mapAttrs (p: v: hself.callHackage p v {}) hackagePins;
in lib.lists.foldr (a: b: a // b) hackagePackages (lib.lists.flatten gitPackages)
