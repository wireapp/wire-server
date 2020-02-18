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
    flags = { arbitrary = true; cql = false; };
    package = {
      specVersion = "0";
      identifier = { name = "brig-types"; version = "1.35.0"; };
      license = "AGPL-3.0-only";
      copyright = "(c) 2017 Wire Swiss GmbH";
      maintainer = "Wire Swiss GmbH <backend@wire.com>";
      author = "Wire Swiss GmbH";
      homepage = "";
      url = "";
      synopsis = "User Service";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
          (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."currency-codes" or (buildDepError "currency-codes"))
          (hsPkgs."errors" or (buildDepError "errors"))
          (hsPkgs."galley-types" or (buildDepError "galley-types"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."hostname-validate" or (buildDepError "hostname-validate"))
          (hsPkgs."imports" or (buildDepError "imports"))
          (hsPkgs."iproute" or (buildDepError "iproute"))
          (hsPkgs."iso3166-country-codes" or (buildDepError "iso3166-country-codes"))
          (hsPkgs."iso639" or (buildDepError "iso639"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
          (hsPkgs."network-uri" or (buildDepError "network-uri"))
          (hsPkgs."pem" or (buildDepError "pem"))
          (hsPkgs."safe" or (buildDepError "safe"))
          (hsPkgs."scientific" or (buildDepError "scientific"))
          (hsPkgs."singletons" or (buildDepError "singletons"))
          (hsPkgs."swagger" or (buildDepError "swagger"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."types-common" or (buildDepError "types-common"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."uuid" or (buildDepError "uuid"))
          ] ++ (pkgs.lib).optional (flags.cql) (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))) ++ (pkgs.lib).optionals (flags.arbitrary) [
          (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
          (hsPkgs."extra" or (buildDepError "extra"))
          (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
          (hsPkgs."random" or (buildDepError "random"))
          (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
          ];
        buildable = true;
        };
      tests = {
        "brig-types-tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-qq" or (buildDepError "aeson-qq"))
            (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."brig-types" or (buildDepError "brig-types"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."currency-codes" or (buildDepError "currency-codes"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."galley-types" or (buildDepError "galley-types"))
            (hsPkgs."hostname-validate" or (buildDepError "hostname-validate"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."iproute" or (buildDepError "iproute"))
            (hsPkgs."iso639" or (buildDepError "iso639"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."pem" or (buildDepError "pem"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."vector" or (buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../libs/brig-types; }) // {
    cabal-generator = "hpack";
    }