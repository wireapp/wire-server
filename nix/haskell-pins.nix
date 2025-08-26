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
# since the sha256 has changed, the error will be the same if you remove it or if you leave the
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
    # ----------------
    # maintained by us
    # ----------------

    cryptobox-haskell = {
      src = fetchgit {
        url = "https://github.com/wireapp/cryptobox-haskell";
        rev = "7546a1a25635ef65183e3d44c1052285e8401608";
        hash = "sha256-9mMVgmMB1NWCPm/3inLeF4Ouiju0uIb/92UENoP88TU=";
      };
    };

    # --------------------
    # END maintained by us
    # --------------------

    bloodhound = {
      src = fetchgit {
        url = "https://github.com/wireapp/bloodhound";
        rev = "dac0f1384b335ce35dc026bf8154e574b1a15d62";
        hash = "sha256-E3co9FGZP135T3RocX4vbUELbbgGbYddD8CcVNUzHu8=";
      };
    };

    # Merged PR https://github.com/dylex/hsaml2/pull/20
    hsaml2 = {
      src = fetchgit {
        url = "https://github.com/dylex/hsaml2";
        rev = "874627ad22e69afe4d9a797e39633ffb30697c78";
        hash = "sha256-gufEAC7fFqafG8dXkGIOSfAcVv+ZWkawmBgUV+Ics2s=";
      };
    };

    # PR: https://github.com/informatikr/hedis/pull/224
    # PR: https://github.com/informatikr/hedis/pull/226
    # PR: https://github.com/informatikr/hedis/pull/227
    hedis = {
      src = fetchgit {
        url = "https://github.com/wireapp/hedis";
        rev = "00d7fbf5f19b812b9e64e12be8860c4741be8558";
        sha256 = "sha256-BwcqQZf2GaEn2i6o9bVl+jiu/CjShYlHCmO81bYfc8Y=";
      };
    };

    # Our fork because we need to a few special things
    http-client = {
      src = fetchgit {
        url = "https://github.com/wireapp/http-client";
        rev = "37494bb9a89dd52f97a8dc582746c6ff52943934";
        hash = "sha256-z47GlT+tHsSlRX4ApSGQIpOpaZiBeqr72/tWuvzw8tc=";
      };
      packages = {
        "http-client" = "http-client";
        "http-client-tls" = "http-client-tls";
        "http-client-openssl" = "http-client-openssl";
        "http-conduit" = "http-conduit";
      };
    };

    # PR: https://github.com/hspec/hspec-wai/pull/49
    hspec-wai = {
      src = fetchgit {
        url = "https://github.com/wireapp/hspec-wai";
        rev = "08176f07fa893922e2e78dcaf996c33d79d23ce2";
        hash = "sha256-Nc5POjA+mJt7Vi3drczEivGsv9PXeVOCSwp21lLmz58=";
      };
    };

    # PR: https://gitlab.com/twittner/cql/-/merge_requests/11
    cql = {
      src = fetchgit {
        url = "https://github.com/wireapp/cql";
        rev = "abbd2739969d17a909800f282d10d42a254c4e3b";
        hash = "sha256-2MYwZKiTdwgjJdLNvECi7gtcIo+3H4z1nYzen5x0lgU=";
      };
    };

    # PR: https://gitlab.com/twittner/cql-io/-/merge_requests/20
    cql-io = {
      src = fetchgit {
        url = "https://github.com/wireapp/cql-io";
        rev = "c2b6aa995b5817ed7c78c53f72d5aa586ef87c36";
        hash = "sha256-DMRWUq4yorG5QFw2ZyF/DWnRjfnzGupx0njTiOyLzPI=";
      };
    };

    # missing upstream PR, this will get removed when completing
    # servantification
    wai-predicates = {
      src = fetchgit {
        url = "https://github.com/wireapp/wai-predicates";
        rev = "ff95282a982ab45cced70656475eaf2cefaa26ea";
        hash = "sha256-x2XSv2+/+DG9FXN8hfUWGNIO7V4iBhlzYz19WWKaLKQ=";
      };
    };

    # PR: https://github.com/UnkindPartition/tasty/pull/351
    tasty = {
      src = fetchgit {
        url = "https://github.com/wireapp/tasty";
        rev = "97df5c1db305b626ffa0b80055361b7b28e69cec";
        hash = "sha256-oACehxazeKgRr993gASRbQMf74heh5g0B+70ceAg17I=";
      };
      packages = {
        tasty-hunit = "hunit";
      };
    };

    # sets the required flag for HTTP request bodies.
    # PR: https://github.com/biocad/servant-openapi3/pull/49
    servant-openapi3 = {
      src = fetchgit {
        url = "https://github.com/wireapp/servant-openapi3";
        rev = "0db0095040df2c469a48f5b8724595f82afbad0c";
        hash = "sha256-iKMWd+qm8hHhKepa13VWXDPCpTMXxoOwWyoCk4lLlIY=";
      };
    };

    # we need HEAD, the latest release is too old
    postie = {
      src = fetchgit {
        url = "https://github.com/alexbiehl/postie";
        rev = "13404b8cb7164cd9010c9be6cda5423194dd0c06";
        hash = "sha256-nNivtyBpr4DFsbaXxlCznX+MYtzNshU7vfVpnhMh52c=";
      };
    };

    tinylog = {
      src = fetchgit {
        url = "https://github.com/wireapp/tinylog.git";
        rev = "9609104263e8cd2a631417c1c3ef23e090de0d09";
        hash = "sha256-htEIJY+LmIMACVZrflU60+X42/g14NxUyFM7VJs4E6w=";
      };
    };

    # PR: https://github.com/ocharles/tasty-ant-xml/pull/32
    tasty-ant-xml = {
      src = fetchgit {
        url = "https://github.com/wireapp/tasty-ant-xml";
        rev = "11c53e976e2e941f25a33e8768669eb576d19ea8";
        hash = "sha256-Aj/iTVECsCGq4f+32FXWyYj/iLH5e4Gm4hYRmewnJJM=";
      };
    };

    text-icu-translit = {
      src = pkgs.fetchFromGitHub {
        owner = "wireapp";
        repo = "text-icu-translit";
        rev = "317bbd27ea5ae4e7f93836ee9ca664f9bde7c583";
        hash = "sha256-E35PVxi/4iJFfWts3td52KKZKQt4dj9KFP3SvWG77Cc=";
      };
    };

    # open PR https://github.com/yesodweb/wai/pull/958 for sending connection: close when closing connection
    warp = {
      packages.warp = "warp";
      src = pkgs.fetchFromGitHub {
        owner = "yesodweb";
        repo = "wai";
        rev = "8b20c9db265a202a2c7ba2a9ec8786a1ee59957b";
        hash = "sha256-fKUSiRl38FKY1gFSmbksktoqoLfQrDxRRWEh4k+RRW4=";
      };
    };

    # this contains an important fix to the initialization of the window size
    # and should be switched to upstream as soon as we can
    # version = "5.2.5";
    # This patch also includes suppressing ConnectionIsClosed
    http2 = {
      src = fetchgit {
        url = "https://github.com/wireapp/http2";
        rev = "45653e3caab0642e539fab2681cb09402aae29ca";
        hash = "sha256-L90PQtDw/JFwyltSVFvmfjTAb0ZLhFt9Hl0jbzn+cFQ=";
      };
    };

    # hs-opentelemetry-* has not been released for a while on hackage. Thus,
    # we're following main.
    hs-opentelemetry = {
      src = fetchgit {
        url = "https://github.com/iand675/hs-opentelemetry";
        rev = "ee8a6dad7db306eb67748ddcd77df4974ad8259e";
        hash = "sha256-UirBRxY9gAv5x/t87RZcWCy6GtsigzFMABKqrhS9b7s=";
      };
      packages = {
        hs-opentelemetry-sdk = "sdk";
        hs-opentelemetry-api = "api";
        hs-opentelemetry-propagator-datadog = "propagators/datadog";
        hs-opentelemetry-instrumentation-http-client = "instrumentation/http-client";
        hs-opentelemetry-instrumentation-wai = "instrumentation/wai";
        hs-opentelemetry-exporter-otlp = "exporters/otlp";
        hs-opentelemetry-utils-exceptions = "utils/exceptions";
      };
    };

    HaskellNet = {
      src = fetchgit {
        url = "https://github.com/wireapp/HaskellNet";
        rev = "74cde03b4beb09794a6120ea5321a09430bcd2c7";
        hash = "sha256-VIM60sXCVC25ULf/2yPvqANK/h9BY6dEYY3o3/xiEEQ=";
      };
    };

    # Our fork of 2.0.0. This release hasn't been updated for a while and Nix
    # is bad in coping with Hackage patched revisions and overriding
    # ghc-options. So, we have our fork to gain GHC 9.8 compatibility.
    #
    # N.B. only the listed packages work. If you want to use another:
    # - list it here
    # - patch it on the fork (if required)
    amazonka = {
      src = fetchgit {
        url = "https://github.com/wireapp/amazonka";
        rev = "d98cefc04bcc7076a915076a322ab5905c6a4945";
        hash = "sha256-8HNHoTUaLi5lyOrKYybacZsDSHrju9/oo+Lf/YulbIo=";
      };
      packages = {
        amazonka = "lib/amazonka";
        amazonka-core = "lib/amazonka-core";
        amazonka-dynamodb = "lib/services/amazonka-dynamodb";
        amazonka-s3 = "lib/services/amazonka-s3";
        amazonka-sts = "lib/services/amazonka-sts";
        amazonka-sqs = "lib/services/amazonka-sqs";
        amazonka-ses = "lib/services/amazonka-ses";
        amazonka-sns = "lib/services/amazonka-sns";
        amazonka-sso = "lib/services/amazonka-sso";
        amazonka-gen = "gen/";
        amazonka-test = "lib/amazonka-test";
      };
    };
  };

  hackagePins = {
    # start pinned dependencies for http2
    http-semantics = {
      version = "0.1.2";
      sha256 = "sha256-S4rGBCIKVPpLPumLcVzrPONrbWm8VBizqxI3dXNIfr0=";
    };

    tasty-ant-xml = {
      version = "1.1.9";
      sha256 = "sha256-aB7B61XSAZ5V+uW+QBe/PKBmhdFfX3OoOjDE9jB7Mek=";
    };

    network-run = {
      version = "0.3.0";
      sha256 = "sha256-FP2GZKwacC+TLLwEIVgKBtnKplYPf5xOIjDfvlbQV0o=";
    };
    time-manager = {
      version = "0.1.0";
      sha256 = "sha256-WRe9LZrOIPJVBFk0vMN2IMoxgP0a0psQCiCiOFWJc74=";
    };
    hasql = {
      version = "1.9.1.2";
      sha256 = "sha256-W2pAC3wLIizmbspWHeWDQqb5AROtwA8Ok+lfZtzTlQg=";
    };

    hasql-pool = {
      version = "1.3.0.1";
      sha256 = "sha256-TtNrs1z8L39WnX8277V97g9Ot1DwutKLrAB1JOjQQoQ=";
    };

    postgresql-syntax = {
      version = "0.4.1.3";
      sha256 = "sha256-afC4lQUPUL5cHe+7vTG1lFZ4wWyQzdh9MEhMT/TtP5c=";
    };

    text-builder-core = {
      version = "0.1.1.1";
      sha256 = "sha256-BX6JRG+K1PnS3GLvpakG7rTsARfVmGLl1gTW0a4UyDo=";
    };

    text-builder = {
      version = "1.0.0.3";
      sha256 = "sha256-9VCOzwebs89KosOouJjFUcAgY6PF97yJnnIp4HZOK20=";
    };

    network-control = {
      version = "0.1.0";
      sha256 = "sha256-D6pKb6+0Pr08FnObGbXBVMv04ys3N731p7U+GYH1oEg=";
    };
    # end pinned dependencies for http2

    # This pin should not be necessary. However, without it, Nix tries to fetch
    # the sources from the `amazonka` package and fails.
    # Fix: https://github.com/NixOS/nixpkgs/pull/409098
    amazonka-s3-streaming = {
      version = "2.0.0.0";
      sha256 = "sha256-SQyFjl1Zf4vnntjZHJpf46gMR3LXWCQAMsR56NdsvRA=";
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
  # [[AttrSet]]
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
    (pkg: args:
      hself.callHackageDirect
        {
          ver = args.version;
          sha256 = args.sha256 or "";
          inherit pkg;
        }
        { }
    )
    hackagePins;
in
lib.lists.foldr (a: b: a // b) hackagePackages (lib.lists.flatten gitPackages)
