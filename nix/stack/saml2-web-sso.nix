let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  ({
    flags = {};
    package = {
      specVersion = "0";
      identifier = { name = "saml2-web-sso"; version = "0.18"; };
      license = "AGPL-3.0-only";
      copyright = "(c) 2017 Wire Swiss GmbH";
      maintainer = "Wire Swiss GmbH <backend@wire.com>";
      author = "Wire Swiss GmbH";
      homepage = "";
      url = "";
      synopsis = "Library and example web app for the SAML Web-based SSO profile.";
      description = "See README.md";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."asn1-encoding" or (buildDepError "asn1-encoding"))
          (hsPkgs."asn1-parse" or (buildDepError "asn1-parse"))
          (hsPkgs."asn1-types" or (buildDepError "asn1-types"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cookie" or (buildDepError "cookie"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."dns" or (buildDepError "dns"))
          (hsPkgs."email-validate" or (buildDepError "email-validate"))
          (hsPkgs."errors" or (buildDepError "errors"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."extra" or (buildDepError "extra"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."foundation" or (buildDepError "foundation"))
          (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
          (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
          (hsPkgs."hedgehog-quickcheck" or (buildDepError "hedgehog-quickcheck"))
          (hsPkgs."hourglass" or (buildDepError "hourglass"))
          (hsPkgs."hsaml2" or (buildDepError "hsaml2"))
          (hsPkgs."http-media" or (buildDepError "http-media"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."hxt" or (buildDepError "hxt"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."lens-datetime" or (buildDepError "lens-datetime"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."network-uri" or (buildDepError "network-uri"))
          (hsPkgs."pretty-show" or (buildDepError "pretty-show"))
          (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
          (hsPkgs."random" or (buildDepError "random"))
          (hsPkgs."servant" or (buildDepError "servant"))
          (hsPkgs."servant-multipart" or (buildDepError "servant-multipart"))
          (hsPkgs."servant-server" or (buildDepError "servant-server"))
          (hsPkgs."silently" or (buildDepError "silently"))
          (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."uniplate" or (buildDepError "uniplate"))
          (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
          (hsPkgs."uuid" or (buildDepError "uuid"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."x509" or (buildDepError "x509"))
          (hsPkgs."xml-conduit" or (buildDepError "xml-conduit"))
          (hsPkgs."xml-conduit-writer" or (buildDepError "xml-conduit-writer"))
          (hsPkgs."xml-hamlet" or (buildDepError "xml-hamlet"))
          (hsPkgs."xml-types" or (buildDepError "xml-types"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ];
        buildable = true;
        };
      exes = {
        "toy-sp" = {
          depends = [
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."asn1-encoding" or (buildDepError "asn1-encoding"))
            (hsPkgs."asn1-parse" or (buildDepError "asn1-parse"))
            (hsPkgs."asn1-types" or (buildDepError "asn1-types"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cookie" or (buildDepError "cookie"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."dns" or (buildDepError "dns"))
            (hsPkgs."email-validate" or (buildDepError "email-validate"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."foundation" or (buildDepError "foundation"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
            (hsPkgs."hedgehog-quickcheck" or (buildDepError "hedgehog-quickcheck"))
            (hsPkgs."hourglass" or (buildDepError "hourglass"))
            (hsPkgs."hsaml2" or (buildDepError "hsaml2"))
            (hsPkgs."http-media" or (buildDepError "http-media"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."hxt" or (buildDepError "hxt"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lens-datetime" or (buildDepError "lens-datetime"))
            (hsPkgs."memory" or (buildDepError "memory"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network-uri" or (buildDepError "network-uri"))
            (hsPkgs."pretty-show" or (buildDepError "pretty-show"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."saml2-web-sso" or (buildDepError "saml2-web-sso"))
            (hsPkgs."servant" or (buildDepError "servant"))
            (hsPkgs."servant-multipart" or (buildDepError "servant-multipart"))
            (hsPkgs."servant-server" or (buildDepError "servant-server"))
            (hsPkgs."silently" or (buildDepError "silently"))
            (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."uniplate" or (buildDepError "uniplate"))
            (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."x509" or (buildDepError "x509"))
            (hsPkgs."xml-conduit" or (buildDepError "xml-conduit"))
            (hsPkgs."xml-conduit-writer" or (buildDepError "xml-conduit-writer"))
            (hsPkgs."xml-hamlet" or (buildDepError "xml-hamlet"))
            (hsPkgs."xml-types" or (buildDepError "xml-types"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."asn1-encoding" or (buildDepError "asn1-encoding"))
            (hsPkgs."asn1-parse" or (buildDepError "asn1-parse"))
            (hsPkgs."asn1-types" or (buildDepError "asn1-types"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cookie" or (buildDepError "cookie"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."dns" or (buildDepError "dns"))
            (hsPkgs."email-validate" or (buildDepError "email-validate"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."foundation" or (buildDepError "foundation"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
            (hsPkgs."hedgehog-quickcheck" or (buildDepError "hedgehog-quickcheck"))
            (hsPkgs."hourglass" or (buildDepError "hourglass"))
            (hsPkgs."hsaml2" or (buildDepError "hsaml2"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-core" or (buildDepError "hspec-core"))
            (hsPkgs."hspec-discover" or (buildDepError "hspec-discover"))
            (hsPkgs."hspec-wai" or (buildDepError "hspec-wai"))
            (hsPkgs."http-media" or (buildDepError "http-media"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."hxt" or (buildDepError "hxt"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lens-datetime" or (buildDepError "lens-datetime"))
            (hsPkgs."memory" or (buildDepError "memory"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network-uri" or (buildDepError "network-uri"))
            (hsPkgs."pretty-show" or (buildDepError "pretty-show"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."saml2-web-sso" or (buildDepError "saml2-web-sso"))
            (hsPkgs."servant" or (buildDepError "servant"))
            (hsPkgs."servant-multipart" or (buildDepError "servant-multipart"))
            (hsPkgs."servant-server" or (buildDepError "servant-server"))
            (hsPkgs."shelly" or (buildDepError "shelly"))
            (hsPkgs."silently" or (buildDepError "silently"))
            (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
            (hsPkgs."temporary" or (buildDepError "temporary"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."uniplate" or (buildDepError "uniplate"))
            (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."x509" or (buildDepError "x509"))
            (hsPkgs."xml-conduit" or (buildDepError "xml-conduit"))
            (hsPkgs."xml-conduit-writer" or (buildDepError "xml-conduit-writer"))
            (hsPkgs."xml-hamlet" or (buildDepError "xml-hamlet"))
            (hsPkgs."xml-types" or (buildDepError "xml-types"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/wireapp/saml2-web-sso";
      rev = "1a1b313092beb685a9bb15685c83a3162c1e220f";
      sha256 = "15711vshj860d7x42aph7r4icynawh1wy5m8h2b8bdxbxklzrq1l";
      });
    }) // { cabal-generator = "hpack"; }