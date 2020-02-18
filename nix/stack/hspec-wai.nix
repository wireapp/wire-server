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
      identifier = { name = "hspec-wai"; version = "0.9.2"; };
      license = "MIT";
      copyright = "(c) 2012-2014 Fujimura Daisuke,\n(c) 2014-2018 Simon Hengel";
      maintainer = "Fujimura Daisuke <me@fujimuradaisuke.com>,\nSimon Hengel <sol@typeful.net>";
      author = "Fujimura Daisuke <me@fujimuradaisuke.com>,\nSimon Hengel <sol@typeful.net>";
      homepage = "https://github.com/hspec/hspec-wai#readme";
      url = "";
      synopsis = "Experimental Hspec support for testing WAI applications";
      description = "Experimental Hspec support for testing WAI applications";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."base-compat" or (buildDepError "base-compat"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
          (hsPkgs."hspec-core" or (buildDepError "hspec-core"))
          (hsPkgs."hspec-expectations" or (buildDepError "hspec-expectations"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base-compat" or (buildDepError "base-compat"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-core" or (buildDepError "hspec-core"))
            (hsPkgs."hspec-expectations" or (buildDepError "hspec-expectations"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/wireapp/hspec-wai";
      rev = "0a5142cd3ba48116ff059c041348b817fb7bdb25";
      sha256 = "1yqkla7467fgb23yw689xh15zjn38rkc7ckf18cfalpc2ff5wfq1";
      });
    }) // { cabal-generator = "hpack"; }