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
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "amazonka"; version = "1.6.1"; };
      license = "MPL-2.0";
      copyright = "Copyright (c) 2013-2017 Brendan Hay";
      maintainer = "Brendan Hay <brendan.g.hay+amazonka@gmail.com>";
      author = "Brendan Hay";
      homepage = "https://github.com/brendanhay/amazonka";
      url = "";
      synopsis = "Comprehensive Amazon Web Services SDK.";
      description = "This client library contains request and response logic to communicate\nwith Amazon Web Service compatible APIs using the types supplied by the\nvarious @amazonka-*@ service libraries. See the <http://hackage.haskell.org/packages/#cat:AWS AWS>\ncategory on Hackage for supported services.\n\nTo get started, import the desired @amazonka-*@ library (such as\n<http://hackage.haskell.org/package/amazonka-ml/docs/Network-AWS-MachineLearning.html Network.AWS.MachineLearning>)\nand one of the following:\n\n* \"Control.Monad.Trans.AWS\": The 'AWST' transformer and generalised operations.\n\n* \"Network.AWS\": The 'AWS' monad and 'MonadAWS' type class for automatically\nlifting operations when embedded as a layer in a transformer stack.\n\nGHC 7.8.4 and higher is officially supported.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."amazonka-core" or (buildDepError "amazonka-core"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."conduit" or (buildDepError "conduit"))
          (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."ini" or (buildDepError "ini"))
          (hsPkgs."mmorph" or (buildDepError "mmorph"))
          (hsPkgs."monad-control" or (buildDepError "monad-control"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."resourcet" or (buildDepError "resourcet"))
          (hsPkgs."retry" or (buildDepError "retry"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
          (hsPkgs."transformers-compat" or (buildDepError "transformers-compat"))
          (hsPkgs."unliftio-core" or (buildDepError "unliftio-core"))
          (hsPkgs."void" or (buildDepError "void"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."amazonka" or (buildDepError "amazonka"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/brendanhay/amazonka";
      rev = "9cf5b5777b69ac494d23d43a692294882927df34";
      sha256 = "16h9ygvidacnn7fkg6klixhcnyrr3c6bdbc1hfws5nwq0s86y6sl";
      });
    postUnpack = "sourceRoot+=/amazonka; echo source root reset to \$sourceRoot";
    }