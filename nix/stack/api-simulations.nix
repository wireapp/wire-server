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
      identifier = { name = "api-simulations"; version = "0.4.2"; };
      license = "AGPL-3.0-only";
      copyright = "";
      maintainer = "Wire Swiss GmbH <backend@wire.com>";
      author = "Wire Swiss GmbH";
      homepage = "";
      url = "";
      synopsis = "(Internal) Wire API simulations";
      description = "(Internal) Wire API simulations using bots (automated users and clients).";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."api-bot" or (buildDepError "api-bot"))
          (hsPkgs."api-client" or (buildDepError "api-client"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
          (hsPkgs."cereal" or (buildDepError "cereal"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."errors" or (buildDepError "errors"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."imports" or (buildDepError "imports"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."split" or (buildDepError "split"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."types-common" or (buildDepError "types-common"))
          ];
        buildable = true;
        };
      exes = {
        "api-loadtest" = {
          depends = [
            (hsPkgs."api-bot" or (buildDepError "api-bot"))
            (hsPkgs."api-client" or (buildDepError "api-client"))
            (hsPkgs."api-simulations" or (buildDepError "api-simulations"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."data-default-class" or (buildDepError "data-default-class"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."metrics-core" or (buildDepError "metrics-core"))
            (hsPkgs."mime" or (buildDepError "mime"))
            (hsPkgs."monad-control" or (buildDepError "monad-control"))
            (hsPkgs."mwc-random" or (buildDepError "mwc-random"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."retry" or (buildDepError "retry"))
            (hsPkgs."split" or (buildDepError "split"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."unliftio" or (buildDepError "unliftio"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            ];
          buildable = true;
          };
        "api-smoketest" = {
          depends = [
            (hsPkgs."api-bot" or (buildDepError "api-bot"))
            (hsPkgs."api-client" or (buildDepError "api-client"))
            (hsPkgs."api-simulations" or (buildDepError "api-simulations"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."data-default-class" or (buildDepError "data-default-class"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mime" or (buildDepError "mime"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."retry" or (buildDepError "retry"))
            (hsPkgs."split" or (buildDepError "split"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."unliftio" or (buildDepError "unliftio"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../tools/api-simulations; }) // {
    cabal-generator = "hpack";
    }