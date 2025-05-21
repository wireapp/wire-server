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

    # PR: https://github.com/kazu-yamamoto/crypton-certificate/pull/8
    crypton-certificates = {
      src = fetchgit {
        url = "https://github.com/akshaymankar/hs-certificate";
        rev = "9e293695d8ca5efc513ee0082ae955ff9b32eb6b";
        hash = "sha256-mD5Dvuzol3K9CNNSfa2L9ir9AbrQ8HJc0QNmkK3qBWk=";
      };
      packages = {
        "crypton-x509-validation" = "x509-validation";
      };
    };

    # PR: https://github.com/dpwright/HaskellNet-SSL/pull/33
    HaskellNet-SSL = {
      src = fetchgit {
        url = "https://github.com/wireapp/HaskellNet-SSL";
        rev = "c2844b63a39f458ffbfe62f2ac824017f1f84453";
        hash = "sha256-1mu/yEAWr3POY4MHRomum0DDvs5Qty1JvP3v5GS2u64=";
      };
    };

    # PR https://github.com/dylex/hsaml2/pull/20
    hsaml2 = {
      src = fetchgit {
        url = "https://github.com/mangoiv/hsaml2";
        rev = "d35f92a3253d146c92caf371b90eb4889841918a";
        hash = "sha256-gufEAC7fFqafG8dXkGIOSfAcVv+ZWkawmBgUV+Ics2s=";
      };
    };

    # PR: https://github.com/informatikr/hedis/pull/224
    # PR: https://github.com/informatikr/hedis/pull/226
    # PR: https://github.com/informatikr/hedis/pull/227
    hedis = {
      src = fetchgit {
        url = "https://github.com/wireapp/hedis";
        rev = "87f4a5ecfa572dfdc9ebe905485d0012ad2d1833";
        sha256 = "sha256-3evlUj/n39SYncJDUjN6hk12tn/DyCFy2TFvP0/6xdU=";
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

    # Replace this with upstream once > 0.20.2 has been released.
    servant = {
      src = fetchgit {
        url = "https://github.com/wireapp/servant";
        rev = "fa8271564ebd9dff22de84aa77a687c89398a612";
        hash = "sha256-9g3tEfHCtGyA+w4HAy6H36IyIUnDPmfJHAxCswJEVSQ=";
      };
      packages = {
        servant = "servant";
        servant-server = "servant-server";
      };
    };

    # we need HEAD, the latest release is too old
    postie = {
      src = fetchgit {
        url = "https://github.com/alexbiehl/postie";
        rev = "7321b977a2b427e0be782b7239901e4edfbb027f";
        hash = "sha256-DKugy4EpRsSgaGvybdh2tLa7HCtoxId+7RAAAw43llA=";
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
        rev = "34ff294d805e62e73678dccc0be9d3da13540fbe";
        hash = "sha256-+rHcS+BwEFsXqPAHX/KZDIgv9zfk1dZl0LlZJ57Com4=";
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

    # hs-opentelemetry-* has not been released for a while on hackage
    hs-opentelemetry = {
      src = fetchgit {
        url = "https://github.com/iand675/hs-opentelemetry";
        rev = "0b3c854a88113fc18df8561202a76357e593a294";
        hash = "sha256-N5FzKz6T1sE9xffGCeWa+iTW8a1GCLsy2TlAjzIed34=";
      };
      packages = {
        hs-opentelemetry-sdk = "sdk";
        hs-opentelemetry-api = "api";
        hs-opentelemetry-propagator-datadog = "propagators/datadog";
        hs-opentelemetry-instrumentation-http-client = "instrumentation/http-client";
        hs-opentelemetry-instrumentation-wai = "instrumentation/wai";
        hs-opentelemetry-exporter-otlp = "exporters/otlp";
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
    auto-update = {
      version = "0.2.0";
      sha256 = "sha256-d/0IDjaaCLz8tlx88z8Ew8ol9PrSRPVWaUwTbim70yE=";
    };

    hasql = {
      version = "1.9.1.2";
      sha256 = "sha256-W2pAC3wLIizmbspWHeWDQqb5AROtwA8Ok+lfZtzTlQg=";
    };

    hasql-pool = {
      version = "1.3.0.1";
      sha256 = "sha256-TtNrs1z8L39WnX8277V97g9Ot1DwutKLrAB1JOjQQoQ=";
    };

    hasql-transaction = {
      version = "1.2.0.1";
      sha256 = "sha256-gXLDMlD6E3degEUJOtFCiZf9EAsWEBJqsOfZK54iBSA=";
    };

    hasql-th = {
      version = "0.4.0.23";
      sha256 = "sha256-t9WgRQ60zXYpBz7qZHjz7S5ksEUfg/PHBvD0gOLad0Y=";
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

    # PR: https://github.com/wireapp/wire-server/pull/4027
    HsOpenSSL = {
      version = "0.11.7.7";
      sha256 = "sha256-45qWTqfY4fwCjTQsQg/f0EPkC5KZ8CFZYH4cwcw3Y18=";
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
