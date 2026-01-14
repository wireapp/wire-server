# How to add a git pin:
#
# 1. Add the target git repo to the inputs section of flake.nix like this:
#   <name-of-the-repo> = {
#     url = "github:<owner>/<repo>?rev=<sha>";
#     flake = false;
#   };
# 2. If your target git repository has only package with the cabal file at the
# root, add it like this under 'gitPins':
#     <name-of-the-package> = {
#       src = inputs.<name-of-the-repo>;
#     };
#
# 3. If your target git repsitory has many packages, add it like this under 'gitPins':
#
#    <name-of-git-repo> = {
#      src = inputs.<name-of-the-repo>;
#      packages =  {
#        <name-of-package1> = "<relative-path-to-package1>";
#        <name-of-package2> = "<relative-path-to-package2>";
#        <name-of-package3> = "<relative-path-to-package3>";
#      };
#    };
#
# How to update a git pin:
#
# 1. Determine the new commit ID/SHA of the git repository that you want to pin
# and update the 'rev' param in the URL in the inputs section of the flake.nix.
#
# How to add a hackage pin:
#
# 1. Add your package like this, under 'hackagePins':
#    <package-name> = {
#      version = "<version>";
#      sha256 = "";
#    };
#
# 2. Run 'nix build '.#wireServer.haskellPackagesUnoptimizedNoDocs.<your-packge-name>'.
# This should produce an error saying expected sha <something with a lot of
# 'A's> and the actual sha. Replace the empty string in 'sha256' with the actual
# sha.
#
# How to update a hackage pin:
#
# 1. Update version number.
# 2. Make the 'sha256' blank string.
# 3. Run step 2. from how to add a hackage pin.
{ lib, inputs }: hself: hsuper:
let
  gitPins = {
    # ----------------
    # maintained by us
    # ----------------

    cryptobox-haskell = {
      src = inputs.cryptobox-haskell;
    };

    # --------------------
    # END maintained by us
    # --------------------

    bloodhound = {
      src = inputs.bloodhound;
    };

    # Merged PR https://github.com/dylex/hsaml2/pull/20
    hsaml2 = {
      src = inputs.hsaml2;
    };

    # PR: https://github.com/informatikr/hedis/pull/224
    # PR: https://github.com/informatikr/hedis/pull/226
    # PR: https://github.com/informatikr/hedis/pull/227
    hedis = {
      src = inputs.hedis;
    };

    # Our fork because we need to a few special things
    http-client = {
      src = inputs.http-client;
      packages = {
        "http-client" = "http-client";
        "http-client-tls" = "http-client-tls";
        "http-client-openssl" = "http-client-openssl";
        "http-conduit" = "http-conduit";
      };
    };

    # PR: https://github.com/hspec/hspec-wai/pull/49
    hspec-wai = {
      src = inputs.hspec-wai;
    };

    # PR: https://gitlab.com/twittner/cql/-/merge_requests/11
    cql = {
      src = inputs.cql;
    };

    # PR: https://gitlab.com/twittner/cql-io/-/merge_requests/20
    cql-io = {
      src = inputs.cql-io;
    };

    # missing upstream PR, this will get removed when completing
    # servantification
    #
    # this is currently still used/needed in the proxy service
    wai-predicates = {
      src = inputs.wai-predicates;
    };

    # PR: https://github.com/UnkindPartition/tasty/pull/351
    tasty = {
      src = inputs.tasty;
      packages = {
        tasty-hunit = "hunit";
      };
    };

    # sets the required flag for HTTP request bodies.
    # PR: https://github.com/biocad/servant-openapi3/pull/49
    servant-openapi3 = {
      src = inputs.servant-openapi3;
    };

    # we need HEAD, the latest release is too old
    postie = {
      src = inputs.postie;
    };

    tinylog = {
      src = inputs.tinylog;
    };

    # PR: https://github.com/ocharles/tasty-ant-xml/pull/32
    tasty-ant-xml = {
      src = inputs.tasty-ant-xml;
    };

    text-icu-translit = {
      src = inputs.text-icu-translit;
    };

    # Our fork of 2.0.0. This release hasn't been updated for a while and Nix
    # is bad in coping with Hackage patched revisions and overriding
    # ghc-options. So, we have our fork to gain GHC 9.8 compatibility.
    #
    # N.B. only the listed packages work. If you want to use another:
    # - list it here
    # - patch it on the fork (if required)
    # 
    # Can't currently be removed because amazonka-dynamodb-attributevalue
    # does not exist on hackage
    amazonka = {
      src = inputs.amazonka;
      packages = {
        amazonka = "lib/amazonka";
        amazonka-core = "lib/amazonka-core";
        amazonka-dynamodb = "lib/services/amazonka-dynamodb";
        amazonka-dynamodb-attributevalue = "lib/amazonka-dynamodb-attributevalue";
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
    # Cross-platform file system watching for federator certificate monitoring
    fsnotify = {
      version = "0.4.4.0";
      sha256 = "sha256-oBY1EN5YeFvFaSdeD9vUdooW1V6h7uip24BCft6G6xQ=";
    };

    # start pinned dependencies for http2
    http-semantics = {
      version = "0.4.0";
      sha256 = "sha256-rh0z51EKvsu5rQd5n2z3fSRjjEObouNZSBPO9NFYOF0=";
    };

    network-run = {
      version = "0.5.0";
      sha256 = "sha256-vbXh+CzxDsGApjqHxCYf/ijpZtUCApFbkcF5gyN0THU=";
    };

    time-manager = {
      version = "0.2.4";
      sha256 = "sha256-sAt/331YLQ2IU3z90aKYSq1nxoazv87irsuJp7ZG3pw=";
    };
    # end pinned dependencies for http2

    # This pin should not be necessary. However, without it, Nix tries to fetch
    # the sources from the `amazonka` package and fails.
    # Fix: https://github.com/NixOS/nixpkgs/pull/409098
    amazonka-s3-streaming = {
      version = "2.0.0.0";
      sha256 = "sha256-SQyFjl1Zf4vnntjZHJpf46gMR3LXWCQAMsR56NdsvRA=";
    };

    # Pin uri-bytestring: newer parser rejects unescaped Set-Cookie in SSO mobile redirect query, breaking Spar’s URI substitution; stick to 0.3.3.1 for now
    uri-bytestring = {
      version = "0.3.3.1";
      sha256 = "sha256-jgSTBBDcxRQ0tjs0wTyvEpEAkGA7npJKjdXDT81VpT4=";
    };

    warp = {
      version = "3.4.12";
      sha256 = "sha256-Y9xQ1wBbBtSZ4qw3yTGSYX27qi2uFRDJVtAdmQqRnFQ=";
    };
    http2 = {
      version = "5.4.0";
      sha256 = "sha256-PeEWVd61bQ8G7LvfLeXklzXqNJFaAjE2ecRMWJZESPE=";
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
