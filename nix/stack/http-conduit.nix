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
  {
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "http-conduit"; version = "2.3.7.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Michael Snoyman <michael@snoyman.com>";
      homepage = "http://www.yesodweb.com/book/http-conduit";
      url = "";
      synopsis = "HTTP client package with conduit interface and HTTPS support.";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/http-conduit>.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."resourcet" or (buildDepError "resourcet"))
          (hsPkgs."conduit" or (buildDepError "conduit"))
          (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."unliftio-core" or (buildDepError "unliftio-core"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.9")) (hsPkgs."void" or (buildDepError "void"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."HUnit" or (buildDepError "HUnit"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."data-default-class" or (buildDepError "data-default-class"))
            (hsPkgs."connection" or (buildDepError "connection"))
            (hsPkgs."warp-tls" or (buildDepError "warp-tls"))
            (hsPkgs."tls" or (buildDepError "tls"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."blaze-builder" or (buildDepError "blaze-builder"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."utf8-string" or (buildDepError "utf8-string"))
            (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
            (hsPkgs."unliftio" or (buildDepError "unliftio"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."wai-conduit" or (buildDepError "wai-conduit"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."cookie" or (buildDepError "cookie"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
            (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
            (hsPkgs."streaming-commons" or (buildDepError "streaming-commons"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."temporary" or (buildDepError "temporary"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            (hsPkgs."network" or (buildDepError "network"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/wireapp/http-client";
      rev = "a160cef95d9daaff7d9cfe616d95754c2f8202bf";
      sha256 = "1x7ynm0knqiy5fwvypsrr8zg421j78kdr1y3yia4sxg0yv00hj24";
      });
    postUnpack = "sourceRoot+=/http-conduit; echo source root reset to \$sourceRoot";
    }