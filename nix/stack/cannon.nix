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
      identifier = { name = "cannon"; version = "0.31.0"; };
      license = "AGPL-3.0-only";
      copyright = "(c) 2017 Wire Swiss GmbH";
      maintainer = "Wire Swiss GmbH <backend@wire.com>";
      author = "Wire Swiss GmbH";
      homepage = "";
      url = "";
      synopsis = "Push Notification API";
      description = "Push Notification API Service.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."api-field-json-th" or (buildDepError "api-field-json-th"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bilge" or (buildDepError "bilge"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
          (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."data-timeout" or (buildDepError "data-timeout"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."extended" or (buildDepError "extended"))
          (hsPkgs."gundeck-types" or (buildDepError "gundeck-types"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."imports" or (buildDepError "imports"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."lens-family-core" or (buildDepError "lens-family-core"))
          (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."mwc-random" or (buildDepError "mwc-random"))
          (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
          (hsPkgs."retry" or (buildDepError "retry"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."singletons" or (buildDepError "singletons"))
          (hsPkgs."strict" or (buildDepError "strict"))
          (hsPkgs."swagger" or (buildDepError "swagger"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."tinylog" or (buildDepError "tinylog"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."types-common" or (buildDepError "types-common"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."uuid" or (buildDepError "uuid"))
          (hsPkgs."vault" or (buildDepError "vault"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
          (hsPkgs."wai-predicates" or (buildDepError "wai-predicates"))
          (hsPkgs."wai-routing" or (buildDepError "wai-routing"))
          (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
          (hsPkgs."wai-websockets" or (buildDepError "wai-websockets"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."websockets" or (buildDepError "websockets"))
          ];
        buildable = true;
        };
      exes = {
        "cannon" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cannon" or (buildDepError "cannon"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            ];
          buildable = true;
          };
        };
      tests = {
        "cannon-tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cannon" or (buildDepError "cannon"))
            (hsPkgs."criterion" or (buildDepError "criterion"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../services/cannon; }) // {
    cabal-generator = "hpack";
    }