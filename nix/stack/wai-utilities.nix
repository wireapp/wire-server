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
      identifier = { name = "wai-utilities"; version = "0.16.1"; };
      license = "AGPL-3.0-only";
      copyright = "(c) 2017 Wire Swiss GmbH";
      maintainer = "Wire Swiss GmbH <backend@wire.com>";
      author = "Wire Swiss GmbH";
      homepage = "";
      url = "";
      synopsis = "Various helpers for WAI";
      description = "Small helper functions useful when working with WAI.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."errors" or (buildDepError "errors"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."imports" or (buildDepError "imports"))
          (hsPkgs."metrics-core" or (buildDepError "metrics-core"))
          (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
          (hsPkgs."pipes" or (buildDepError "pipes"))
          (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."streaming-commons" or (buildDepError "streaming-commons"))
          (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
          (hsPkgs."swagger" or (buildDepError "swagger"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."tinylog" or (buildDepError "tinylog"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."types-common" or (buildDepError "types-common"))
          (hsPkgs."unix" or (buildDepError "unix"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-predicates" or (buildDepError "wai-predicates"))
          (hsPkgs."wai-routing" or (buildDepError "wai-routing"))
          (hsPkgs."warp" or (buildDepError "warp"))
          ];
        buildable = true;
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../libs/wai-utilities; }) // {
    cabal-generator = "hpack";
    }