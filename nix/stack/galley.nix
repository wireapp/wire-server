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
      identifier = { name = "galley"; version = "0.83.0"; };
      license = "AGPL-3.0-only";
      copyright = "(c) 2017 Wire Swiss GmbH";
      maintainer = "Wire Swiss GmbH <backend@wire.com>";
      author = "Wire Swiss GmbH";
      homepage = "";
      url = "";
      synopsis = "Conversations";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
          (hsPkgs."HsOpenSSL-x509-system" or (buildDepError "HsOpenSSL-x509-system"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."amazonka" or (buildDepError "amazonka"))
          (hsPkgs."amazonka-sqs" or (buildDepError "amazonka-sqs"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
          (hsPkgs."bilge" or (buildDepError "bilge"))
          (hsPkgs."blaze-builder" or (buildDepError "blaze-builder"))
          (hsPkgs."brig-types" or (buildDepError "brig-types"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
          (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
          (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
          (hsPkgs."cereal" or (buildDepError "cereal"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."currency-codes" or (buildDepError "currency-codes"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."enclosed-exceptions" or (buildDepError "enclosed-exceptions"))
          (hsPkgs."errors" or (buildDepError "errors"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."extended" or (buildDepError "extended"))
          (hsPkgs."extra" or (buildDepError "extra"))
          (hsPkgs."galley-types" or (buildDepError "galley-types"))
          (hsPkgs."gundeck-types" or (buildDepError "gundeck-types"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-client-openssl" or (buildDepError "http-client-openssl"))
          (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."imports" or (buildDepError "imports"))
          (hsPkgs."insert-ordered-containers" or (buildDepError "insert-ordered-containers"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."lifted-base" or (buildDepError "lifted-base"))
          (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
          (hsPkgs."monad-control" or (buildDepError "monad-control"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."pem" or (buildDepError "pem"))
          (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
          (hsPkgs."proto-lens" or (buildDepError "proto-lens"))
          (hsPkgs."protobuf" or (buildDepError "protobuf"))
          (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
          (hsPkgs."resourcet" or (buildDepError "resourcet"))
          (hsPkgs."retry" or (buildDepError "retry"))
          (hsPkgs."safe" or (buildDepError "safe"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."semigroups" or (buildDepError "semigroups"))
          (hsPkgs."servant" or (buildDepError "servant"))
          (hsPkgs."servant-swagger" or (buildDepError "servant-swagger"))
          (hsPkgs."singletons" or (buildDepError "singletons"))
          (hsPkgs."split" or (buildDepError "split"))
          (hsPkgs."ssl-util" or (buildDepError "ssl-util"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
          (hsPkgs."swagger" or (buildDepError "swagger"))
          (hsPkgs."swagger2" or (buildDepError "swagger2"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."text-format" or (buildDepError "text-format"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."tinylog" or (buildDepError "tinylog"))
          (hsPkgs."tls" or (buildDepError "tls"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
          (hsPkgs."types-common" or (buildDepError "types-common"))
          (hsPkgs."types-common-journal" or (buildDepError "types-common-journal"))
          (hsPkgs."unliftio" or (buildDepError "unliftio"))
          (hsPkgs."unliftio-core" or (buildDepError "unliftio-core"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
          (hsPkgs."uuid" or (buildDepError "uuid"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
          (hsPkgs."wai-middleware-gunzip" or (buildDepError "wai-middleware-gunzip"))
          (hsPkgs."wai-predicates" or (buildDepError "wai-predicates"))
          (hsPkgs."wai-routing" or (buildDepError "wai-routing"))
          (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."zauth" or (buildDepError "zauth"))
          ];
        buildable = true;
        };
      exes = {
        "galley" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."galley" or (buildDepError "galley"))
            (hsPkgs."galley-types" or (buildDepError "galley-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."ssl-util" or (buildDepError "ssl-util"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            ];
          buildable = true;
          };
        "galley-integration" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."HsOpenSSL-x509-system" or (buildDepError "HsOpenSSL-x509-system"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-qq" or (buildDepError "aeson-qq"))
            (hsPkgs."amazonka" or (buildDepError "amazonka"))
            (hsPkgs."amazonka-sqs" or (buildDepError "amazonka-sqs"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."bilge" or (buildDepError "bilge"))
            (hsPkgs."brig-types" or (buildDepError "brig-types"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."cereal" or (buildDepError "cereal"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."currency-codes" or (buildDepError "currency-codes"))
            (hsPkgs."data-default-class" or (buildDepError "data-default-class"))
            (hsPkgs."data-timeout" or (buildDepError "data-timeout"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."galley" or (buildDepError "galley"))
            (hsPkgs."galley-types" or (buildDepError "galley-types"))
            (hsPkgs."gundeck-types" or (buildDepError "gundeck-types"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-openssl" or (buildDepError "http-client-openssl"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
            (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."pem" or (buildDepError "pem"))
            (hsPkgs."proto-lens" or (buildDepError "proto-lens"))
            (hsPkgs."protobuf" or (buildDepError "protobuf"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
            (hsPkgs."retry" or (buildDepError "retry"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."servant-swagger" or (buildDepError "servant-swagger"))
            (hsPkgs."ssl-util" or (buildDepError "ssl-util"))
            (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
            (hsPkgs."tagged" or (buildDepError "tagged"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-cannon" or (buildDepError "tasty-cannon"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."tls" or (buildDepError "tls"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."types-common-journal" or (buildDepError "types-common-journal"))
            (hsPkgs."unix" or (buildDepError "unix"))
            (hsPkgs."unliftio" or (buildDepError "unliftio"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-route" or (buildDepError "wai-route"))
            (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."warp-tls" or (buildDepError "warp-tls"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        "galley-journaler" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."HsOpenSSL-x509-system" or (buildDepError "HsOpenSSL-x509-system"))
            (hsPkgs."amazonka" or (buildDepError "amazonka"))
            (hsPkgs."amazonka-sqs" or (buildDepError "amazonka-sqs"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."galley" or (buildDepError "galley"))
            (hsPkgs."galley-types" or (buildDepError "galley-types"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-openssl" or (buildDepError "http-client-openssl"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."proto-lens" or (buildDepError "proto-lens"))
            (hsPkgs."proto-lens-protoc" or (buildDepError "proto-lens-protoc"))
            (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."ssl-util" or (buildDepError "ssl-util"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."types-common-journal" or (buildDepError "types-common-journal"))
            (hsPkgs."unliftio" or (buildDepError "unliftio"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            ];
          buildable = true;
          };
        "galley-schema" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."ssl-util" or (buildDepError "ssl-util"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../services/galley; }) // {
    cabal-generator = "hpack";
    }