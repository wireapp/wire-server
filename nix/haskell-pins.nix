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
    # ----------------
    # maintained by us
    # ----------------

    transitive-anns = {
      src = fetchgit {
        url = "https://github.com/wireapp/transitive-anns";
        rev = "95ee8b5f9c47fe04f8f0d1321f0ade261ab9af54";
        sha256 = "sha256-8NEAHkCBlGO6xnG2K3Lllb2xiCHSYf/dSV1YrmBkOW8=";
      };
    };

    cryptobox-haskell = {
      src = fetchgit {
        url = "https://github.com/wireapp/cryptobox-haskell";
        rev = "7546a1a25635ef65183e3d44c1052285e8401608";
        sha256 = "0dgizj1kc135yzzqdf5l7f5ax0qpvrr8mxvg7s1dbm01cf11aqzn";
      };
    };

    saml2-web-sso = {
      src = fetchgit {
        url = "https://github.com/wireapp/saml2-web-sso";
        rev = "d50bddadf9bd9a96dd6036dad0e2dda27567ec1a";
        sha256 = "sha256-IKovI1h2Wkm3Y7Sz6XsxLOv654SgUasaWsDX6gi9hZw=";
      };
    };

    # --------------------
    # END maintained by us
    # --------------------

    bloodhound = {
      src = fetchgit {
        url = "https://github.com/wireapp/bloodhound";
        rev = "abf819a4a6ec7601f1e58cb8da13b2fdad377d9e";
        sha256 = "sha256-m1O+F/mOJN5z5WNChmeyHP4dtmLRkl2YnLlTuwzRelk=";
      };
    };

    # PR: https://github.com/dpwright/HaskellNet-SSL/pull/33
    HaskellNet-SSL = {
      src = fetchgit {
        url = "https://github.com/wireapp/HaskellNet-SSL";
        rev = "c2844b63a39f458ffbfe62f2ac824017f1f84453";
        sha256 = "sha256-1mu/yEAWr3POY4MHRomum0DDvs5Qty1JvP3v5GS2u64=";
      };
    };

    hsaml2 = {
      src = fetchgit {
        url = "https://github.com/wireapp/hsaml2";
        rev = "723b377fcd759c8be9ad4b2e159a6a06df0d17c9";
        sha256 = "sha256-rPfztTu+NR/5FuoYWGMCfJFhrMn4o09bMcEKoerNX4A=";
      };
    };

    # PR: https://github.com/hspec/hspec-wai/pull/49
    hspec-wai = {
      src = fetchgit {
        url = "https://github.com/wireapp/hspec-wai";
        rev = "08176f07fa893922e2e78dcaf996c33d79d23ce2";
        sha256 = "sha256-Nc5POjA+mJt7Vi3drczEivGsv9PXeVOCSwp21lLmz58=";
      };
    };

    # PR: https://gitlab.com/twittner/cql/-/merge_requests/11
    cql = {
      src = fetchgit {
        url = "https://github.com/wireapp/cql";
        rev = "abbd2739969d17a909800f282d10d42a254c4e3b";
        sha256 = "sha256-2MYwZKiTdwgjJdLNvECi7gtcIo+3H4z1nYzen5x0lgU=";
      };
    };

    # PR: https://gitlab.com/twittner/cql-io/-/merge_requests/20
    cql-io = {
      src = fetchgit {
        url = "https://github.com/wireapp/cql-io";
        rev = "c2b6aa995b5817ed7c78c53f72d5aa586ef87c36";
        sha256 = "sha256-DMRWUq4yorG5QFw2ZyF/DWnRjfnzGupx0njTiOyLzPI=";
      };
    };

    # missing upstream PR, this will get removed when completing
    # servantification
    wai-predicates = {
      src = fetchgit {
        url = "https://github.com/wireapp/wai-predicates";
        rev = "ff95282a982ab45cced70656475eaf2cefaa26ea";
        sha256 = "sha256-x2XSv2+/+DG9FXN8hfUWGNIO7V4iBhlzYz19WWKaLKQ=";
      };
    };

    # we use upstream, but has not been uploaded to hackage since 2016
    wai-routing = {
      src = fetchgit {
        url = "https://github.com/wireapp/wai-routing";
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

    # sets the required flag for HTTP request bodies.
    # PR: https://github.com/biocad/servant-openapi3/pull/49
    servant-openapi3 = {
      src = fetchgit {
        url = "https://github.com/wireapp/servant-openapi3";
        rev = "5cdb2783f15058f753c41b800415d4ba1149a78b";
        sha256 = "sha256-8FM3IAA3ewCuv9Mar8aWmzbyfKK9eLXIJPMHzmYb1zE=";
      };
    };

    postie = {
      src = fetchgit {
        url = "https://github.com/wireapp/postie.git";
        rev = "7321b977a2b427e0be782b7239901e4edfbb027f";
        sha256 = "sha256-DKugy4EpRsSgaGvybdh2tLa7HCtoxId+7RAAAw43llA=";
      };
    };

    tinylog = {
      src = fetchgit {
        url = "https://github.com/wireapp/tinylog.git";
        rev = "9609104263e8cd2a631417c1c3ef23e090de0d09";
        sha256 = "sha256-htEIJY+LmIMACVZrflU60+X42/g14NxUyFM7VJs4E6w=";
      };
    };

    # PR: https://github.com/ocharles/tasty-ant-xml/pull/32
    tasty-ant-xml = {
      src = fetchgit {
        url = "https://github.com/wireapp/tasty-ant-xml";
        rev = "34ff294d805e62e73678dccc0be9d3da13540fbe";
        sha256 = "sha256-+rHcS+BwEFsXqPAHX/KZDIgv9zfk1dZl0LlZJ57Com4=";
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
    # PR: https://github.com/yesodweb/wai/pull/958
    warp = {
      src = fetchgit {
        url = "https://github.com/wireapp/wai";
        rev = "bedd6a835f6d98128880465c30e8115fa986e3f6";
        sha256 = "sha256-0r/d9YwcKZIZd10EhL2TP+W14Wjk0/S8Q4pVvZuZLaY=";
      };
      packages = {
        "warp" = "warp";
      };
    };
  };
  hackagePins = {
    # Major re-write upstream, we should get rid of this dependency rather than
    # adapt to upstream, this will go away when completing servantification.
    wai-route = {
      version = "0.4.0";
      sha256 = "sha256-DSMckKIeVE/buSMg8Mq+mUm1bYPYB7veA11Ns7vTBbc=";
    };

    # these are not yet in nixpkgs
    ghc-source-gen = {
      version = "0.4.4.0";
      sha256 = "sha256-ZSJGF4sdr7tOCv6IUCjIiTrFYL+5gF4W3U6adjBODrE=";
    };
    hoogle = {
      version = "5.0.18.4";
      sha256 = "sha256-gIc4hpdUfTS33rZPfzwLfVcXkQaglmsljqViyYdihdk=";
    };
    # dependency of hoogle 
    safe = {
      version = "0.3.20";
      sha256 = "sha256-PGwjhrRnkH8cLhd7fHTZFd6ts9abp0w5sLlV8ke1yXU=";
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
