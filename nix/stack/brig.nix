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
      identifier = { name = "brig"; version = "1.35.0"; };
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
        depends = [
          (hsPkgs."HaskellNet" or (buildDepError "HaskellNet"))
          (hsPkgs."HaskellNet-SSL" or (buildDepError "HaskellNet-SSL"))
          (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
          (hsPkgs."HsOpenSSL-x509-system" or (buildDepError "HsOpenSSL-x509-system"))
          (hsPkgs."MonadRandom" or (buildDepError "MonadRandom"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."amazonka" or (buildDepError "amazonka"))
          (hsPkgs."amazonka-dynamodb" or (buildDepError "amazonka-dynamodb"))
          (hsPkgs."amazonka-ses" or (buildDepError "amazonka-ses"))
          (hsPkgs."amazonka-sns" or (buildDepError "amazonka-sns"))
          (hsPkgs."amazonka-sqs" or (buildDepError "amazonka-sqs"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."auto-update" or (buildDepError "auto-update"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."base-prelude" or (buildDepError "base-prelude"))
          (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
          (hsPkgs."bilge" or (buildDepError "bilge"))
          (hsPkgs."blaze-builder" or (buildDepError "blaze-builder"))
          (hsPkgs."bloodhound" or (buildDepError "bloodhound"))
          (hsPkgs."brig-types" or (buildDepError "brig-types"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
          (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
          (hsPkgs."conduit" or (buildDepError "conduit"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cookie" or (buildDepError "cookie"))
          (hsPkgs."cryptobox-haskell" or (buildDepError "cryptobox-haskell"))
          (hsPkgs."currency-codes" or (buildDepError "currency-codes"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."data-timeout" or (buildDepError "data-timeout"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."either" or (buildDepError "either"))
          (hsPkgs."email-validate" or (buildDepError "email-validate"))
          (hsPkgs."enclosed-exceptions" or (buildDepError "enclosed-exceptions"))
          (hsPkgs."errors" or (buildDepError "errors"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."extended" or (buildDepError "extended"))
          (hsPkgs."extra" or (buildDepError "extra"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."fsnotify" or (buildDepError "fsnotify"))
          (hsPkgs."galley-types" or (buildDepError "galley-types"))
          (hsPkgs."geoip2" or (buildDepError "geoip2"))
          (hsPkgs."gundeck-types" or (buildDepError "gundeck-types"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."html-entities" or (buildDepError "html-entities"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-client-openssl" or (buildDepError "http-client-openssl"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."imports" or (buildDepError "imports"))
          (hsPkgs."iproute" or (buildDepError "iproute"))
          (hsPkgs."iso639" or (buildDepError "iso639"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
          (hsPkgs."lifted-base" or (buildDepError "lifted-base"))
          (hsPkgs."metrics-core" or (buildDepError "metrics-core"))
          (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
          (hsPkgs."mime" or (buildDepError "mime"))
          (hsPkgs."mime-mail" or (buildDepError "mime-mail"))
          (hsPkgs."monad-control" or (buildDepError "monad-control"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."multihash" or (buildDepError "multihash"))
          (hsPkgs."mwc-random" or (buildDepError "mwc-random"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."network-conduit-tls" or (buildDepError "network-conduit-tls"))
          (hsPkgs."network-uri" or (buildDepError "network-uri"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."pem" or (buildDepError "pem"))
          (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
          (hsPkgs."proto-lens" or (buildDepError "proto-lens"))
          (hsPkgs."random-shuffle" or (buildDepError "random-shuffle"))
          (hsPkgs."resource-pool" or (buildDepError "resource-pool"))
          (hsPkgs."resourcet" or (buildDepError "resourcet"))
          (hsPkgs."retry" or (buildDepError "retry"))
          (hsPkgs."ropes" or (buildDepError "ropes"))
          (hsPkgs."safe" or (buildDepError "safe"))
          (hsPkgs."scientific" or (buildDepError "scientific"))
          (hsPkgs."scrypt" or (buildDepError "scrypt"))
          (hsPkgs."semigroups" or (buildDepError "semigroups"))
          (hsPkgs."singletons" or (buildDepError "singletons"))
          (hsPkgs."smtp-mail" or (buildDepError "smtp-mail"))
          (hsPkgs."sodium-crypto-sign" or (buildDepError "sodium-crypto-sign"))
          (hsPkgs."split" or (buildDepError "split"))
          (hsPkgs."ssl-util" or (buildDepError "ssl-util"))
          (hsPkgs."statistics" or (buildDepError "statistics"))
          (hsPkgs."stomp-queue" or (buildDepError "stomp-queue"))
          (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
          (hsPkgs."swagger" or (buildDepError "swagger"))
          (hsPkgs."tagged" or (buildDepError "tagged"))
          (hsPkgs."template" or (buildDepError "template"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."text-icu-translit" or (buildDepError "text-icu-translit"))
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
          (hsPkgs."vault" or (buildDepError "vault"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
          (hsPkgs."wai-middleware-gunzip" or (buildDepError "wai-middleware-gunzip"))
          (hsPkgs."wai-predicates" or (buildDepError "wai-predicates"))
          (hsPkgs."wai-routing" or (buildDepError "wai-routing"))
          (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          (hsPkgs."zauth" or (buildDepError "zauth"))
          ];
        buildable = true;
        };
      exes = {
        "brig" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."bloodhound" or (buildDepError "bloodhound"))
            (hsPkgs."brig" or (buildDepError "brig"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cookie" or (buildDepError "cookie"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."types-common-journal" or (buildDepError "types-common-journal"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        "brig-index" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."bloodhound" or (buildDepError "bloodhound"))
            (hsPkgs."brig" or (buildDepError "brig"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cookie" or (buildDepError "cookie"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
            (hsPkgs."metrics-core" or (buildDepError "metrics-core"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."types-common-journal" or (buildDepError "types-common-journal"))
            (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        "brig-integration" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."bilge" or (buildDepError "bilge"))
            (hsPkgs."bloodhound" or (buildDepError "bloodhound"))
            (hsPkgs."brig" or (buildDepError "brig"))
            (hsPkgs."brig-types" or (buildDepError "brig-types"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."cargohold-types" or (buildDepError "cargohold-types"))
            (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cookie" or (buildDepError "cookie"))
            (hsPkgs."data-timeout" or (buildDepError "data-timeout"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."galley-types" or (buildDepError "galley-types"))
            (hsPkgs."gundeck-types" or (buildDepError "gundeck-types"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
            (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
            (hsPkgs."mime" or (buildDepError "mime"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."options" or (buildDepError "options"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."pem" or (buildDepError "pem"))
            (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
            (hsPkgs."proto-lens" or (buildDepError "proto-lens"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."retry" or (buildDepError "retry"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."semigroups" or (buildDepError "semigroups"))
            (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-cannon" or (buildDepError "tasty-cannon"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."temporary" or (buildDepError "temporary"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."types-common-aws" or (buildDepError "types-common-aws"))
            (hsPkgs."types-common-journal" or (buildDepError "types-common-journal"))
            (hsPkgs."unix" or (buildDepError "unix"))
            (hsPkgs."unliftio" or (buildDepError "unliftio"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."vector" or (buildDepError "vector"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
            (hsPkgs."wai-route" or (buildDepError "wai-route"))
            (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."warp-tls" or (buildDepError "warp-tls"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            (hsPkgs."zauth" or (buildDepError "zauth"))
            ];
          buildable = true;
          };
        "brig-schema" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."bloodhound" or (buildDepError "bloodhound"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cookie" or (buildDepError "cookie"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
            (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."types-common-journal" or (buildDepError "types-common-journal"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../services/brig; }) // {
    cabal-generator = "hpack";
    }