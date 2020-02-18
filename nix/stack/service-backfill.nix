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
      identifier = { name = "service-backfill"; version = "1.0.0"; };
      license = "LicenseRef-UnspecifiedLicense";
      copyright = "(c) 2018 Wire Swiss GmbH";
      maintainer = "Wire Swiss GmbH <backend@wire.com>";
      author = "Wire Swiss GmbH";
      homepage = "";
      url = "";
      synopsis = "Backfill service tables";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      exes = {
        "service-backfill" = {
          depends = [
            (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."brig-types" or (buildDepError "brig-types"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."galley-types" or (buildDepError "galley-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."unliftio" or (buildDepError "unliftio"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            ];
          buildable = true;
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ../../tools/db/service-backfill;
    }) // { cabal-generator = "hpack"; }