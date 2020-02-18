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
      identifier = { name = "cargohold"; version = "1.5.0"; };
      license = "AGPL-3.0-only";
      copyright = "(c) 2017 Wire Swiss GmbH";
      maintainer = "Wire Swiss GmbH <backend@wire.com>";
      author = "Wire Swiss GmbH";
      homepage = "";
      url = "";
      synopsis = "Asset Storage API";
      description = "API for asset storage.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
          (hsPkgs."HsOpenSSL-x509-system" or (buildDepError "HsOpenSSL-x509-system"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."auto-update" or (buildDepError "auto-update"))
          (hsPkgs."aws" or (buildDepError "aws"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
          (hsPkgs."bilge" or (buildDepError "bilge"))
          (hsPkgs."byteable" or (buildDepError "byteable"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
          (hsPkgs."cargohold-types" or (buildDepError "cargohold-types"))
          (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
          (hsPkgs."cereal" or (buildDepError "cereal"))
          (hsPkgs."conduit" or (buildDepError "conduit"))
          (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."either" or (buildDepError "either"))
          (hsPkgs."errors" or (buildDepError "errors"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."extended" or (buildDepError "extended"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-client-openssl" or (buildDepError "http-client-openssl"))
          (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."imports" or (buildDepError "imports"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
          (hsPkgs."mime" or (buildDepError "mime"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
          (hsPkgs."random" or (buildDepError "random"))
          (hsPkgs."resourcet" or (buildDepError "resourcet"))
          (hsPkgs."retry" or (buildDepError "retry"))
          (hsPkgs."ropes" or (buildDepError "ropes"))
          (hsPkgs."safe" or (buildDepError "safe"))
          (hsPkgs."split" or (buildDepError "split"))
          (hsPkgs."swagger" or (buildDepError "swagger"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."tinylog" or (buildDepError "tinylog"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."types-common" or (buildDepError "types-common"))
          (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
          (hsPkgs."uuid" or (buildDepError "uuid"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-conduit" or (buildDepError "wai-conduit"))
          (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
          (hsPkgs."wai-predicates" or (buildDepError "wai-predicates"))
          (hsPkgs."wai-routing" or (buildDepError "wai-routing"))
          (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."xml-conduit" or (buildDepError "xml-conduit"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ];
        buildable = true;
        };
      exes = {
        "cargohold" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."bilge" or (buildDepError "bilge"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."cargohold" or (buildDepError "cargohold"))
            (hsPkgs."cereal" or (buildDepError "cereal"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."mime" or (buildDepError "mime"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        "cargohold-integration" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."bilge" or (buildDepError "bilge"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."cargohold" or (buildDepError "cargohold"))
            (hsPkgs."cargohold-types" or (buildDepError "cargohold-types"))
            (hsPkgs."cereal" or (buildDepError "cereal"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cryptohash-md5" or (buildDepError "cryptohash-md5"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
            (hsPkgs."mime" or (buildDepError "mime"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."tagged" or (buildDepError "tagged"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../services/cargohold; }) // {
    cabal-generator = "hpack";
    }