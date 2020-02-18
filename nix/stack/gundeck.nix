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
    flags = { static = false; };
    package = {
      specVersion = "0";
      identifier = { name = "gundeck"; version = "1.45.0"; };
      license = "AGPL-3.0-only";
      copyright = "(c) 2017 Wire Swiss GmbH";
      maintainer = "Wire Swiss GmbH <backend@wire.com>";
      author = "Wire Swiss GmbH";
      homepage = "";
      url = "";
      synopsis = "Push Notification Hub";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."amazonka" or (buildDepError "amazonka"))
          (hsPkgs."amazonka-sns" or (buildDepError "amazonka-sns"))
          (hsPkgs."amazonka-sqs" or (buildDepError "amazonka-sqs"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."auto-update" or (buildDepError "auto-update"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
          (hsPkgs."bilge" or (buildDepError "bilge"))
          (hsPkgs."blaze-builder" or (buildDepError "blaze-builder"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
          (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
          (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
          (hsPkgs."conduit" or (buildDepError "conduit"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."enclosed-exceptions" or (buildDepError "enclosed-exceptions"))
          (hsPkgs."errors" or (buildDepError "errors"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."extended" or (buildDepError "extended"))
          (hsPkgs."extra" or (buildDepError "extra"))
          (hsPkgs."gundeck-types" or (buildDepError "gundeck-types"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."imports" or (buildDepError "imports"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
          (hsPkgs."lifted-base" or (buildDepError "lifted-base"))
          (hsPkgs."metrics-core" or (buildDepError "metrics-core"))
          (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
          (hsPkgs."monad-control" or (buildDepError "monad-control"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."network-uri" or (buildDepError "network-uri"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
          (hsPkgs."psqueues" or (buildDepError "psqueues"))
          (hsPkgs."redis-io" or (buildDepError "redis-io"))
          (hsPkgs."resourcet" or (buildDepError "resourcet"))
          (hsPkgs."retry" or (buildDepError "retry"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."semigroups" or (buildDepError "semigroups"))
          (hsPkgs."singletons" or (buildDepError "singletons"))
          (hsPkgs."split" or (buildDepError "split"))
          (hsPkgs."swagger" or (buildDepError "swagger"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."text-format" or (buildDepError "text-format"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."tinylog" or (buildDepError "tinylog"))
          (hsPkgs."tls" or (buildDepError "tls"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
          (hsPkgs."types-common" or (buildDepError "types-common"))
          (hsPkgs."unliftio" or (buildDepError "unliftio"))
          (hsPkgs."unliftio-core" or (buildDepError "unliftio-core"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."uuid" or (buildDepError "uuid"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
          (hsPkgs."wai-middleware-gunzip" or (buildDepError "wai-middleware-gunzip"))
          (hsPkgs."wai-predicates" or (buildDepError "wai-predicates"))
          (hsPkgs."wai-routing" or (buildDepError "wai-routing"))
          (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ];
        buildable = true;
        };
      exes = {
        "gundeck" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."gundeck" or (buildDepError "gundeck"))
            (hsPkgs."gundeck-types" or (buildDepError "gundeck-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            ];
          buildable = true;
          };
        "gundeck-integration" = {
          depends = [
            (hsPkgs."HUnit" or (buildDepError "HUnit"))
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
            (hsPkgs."bilge" or (buildDepError "bilge"))
            (hsPkgs."brig-types" or (buildDepError "brig-types"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."gundeck" or (buildDepError "gundeck"))
            (hsPkgs."gundeck-types" or (buildDepError "gundeck-types"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."network-uri" or (buildDepError "network-uri"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."retry" or (buildDepError "retry"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."tagged" or (buildDepError "tagged"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."vector" or (buildDepError "vector"))
            (hsPkgs."websockets" or (buildDepError "websockets"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        "gundeck-schema" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            ];
          buildable = true;
          };
        };
      tests = {
        "gundeck-tests" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."MonadRandom" or (buildDepError "MonadRandom"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
            (hsPkgs."amazonka" or (buildDepError "amazonka"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."gundeck" or (buildDepError "gundeck"))
            (hsPkgs."gundeck-types" or (buildDepError "gundeck-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."multiset" or (buildDepError "multiset"))
            (hsPkgs."network-uri" or (buildDepError "network-uri"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."quickcheck-state-machine" or (buildDepError "quickcheck-state-machine"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."scientific" or (buildDepError "scientific"))
            (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."tree-diff" or (buildDepError "tree-diff"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."vector" or (buildDepError "vector"))
            (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "gundeck-bench" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."amazonka" or (buildDepError "amazonka"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."criterion" or (buildDepError "criterion"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."gundeck" or (buildDepError "gundeck"))
            (hsPkgs."gundeck-types" or (buildDepError "gundeck-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../services/gundeck; }) // {
    cabal-generator = "hpack";
    }