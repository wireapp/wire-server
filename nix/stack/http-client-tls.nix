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
      identifier = { name = "http-client-tls"; version = "0.3.5.3"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "https://github.com/snoyberg/http-client";
      url = "";
      synopsis = "http-client backend using the connection package and tls library";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <https://www.stackage.org/package/http-client-tls>.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."data-default-class" or (buildDepError "data-default-class"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."connection" or (buildDepError "connection"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."tls" or (buildDepError "tls"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."network-uri" or (buildDepError "network-uri"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."connection" or (buildDepError "connection"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmark" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."gauge" or (buildDepError "gauge"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/wireapp/http-client";
      rev = "a160cef95d9daaff7d9cfe616d95754c2f8202bf";
      sha256 = "1x7ynm0knqiy5fwvypsrr8zg421j78kdr1y3yia4sxg0yv00hj24";
      });
    postUnpack = "sourceRoot+=/http-client-tls; echo source root reset to \$sourceRoot";
    }