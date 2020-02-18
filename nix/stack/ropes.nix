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
      identifier = { name = "ropes"; version = "0.4.20"; };
      license = "AGPL-3.0-only";
      copyright = "(c) 2017 Wire Swiss GmbH";
      maintainer = "Wire Swiss GmbH <backend@wire.com>";
      author = "Wire Swiss GmbH";
      homepage = "";
      url = "";
      synopsis = "Various ropes to tie together with external web services.";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."aws" or (buildDepError "aws"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."errors" or (buildDepError "errors"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."imports" or (buildDepError "imports"))
          (hsPkgs."iso3166-country-codes" or (buildDepError "iso3166-country-codes"))
          (hsPkgs."mime-mail" or (buildDepError "mime-mail"))
          (hsPkgs."resourcet" or (buildDepError "resourcet"))
          (hsPkgs."retry" or (buildDepError "retry"))
          (hsPkgs."semigroups" or (buildDepError "semigroups"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."tinylog" or (buildDepError "tinylog"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ];
        buildable = true;
        };
      exes = {
        "ropes-aws-auth-test" = {
          depends = [
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."mime-mail" or (buildDepError "mime-mail"))
            (hsPkgs."ropes" or (buildDepError "ropes"))
            (hsPkgs."semigroups" or (buildDepError "semigroups"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            ];
          buildable = true;
          };
        "ropes-aws-test" = {
          depends = [
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."mime-mail" or (buildDepError "mime-mail"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            (hsPkgs."ropes" or (buildDepError "ropes"))
            (hsPkgs."semigroups" or (buildDepError "semigroups"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../libs/ropes; }) // {
    cabal-generator = "hpack";
    }