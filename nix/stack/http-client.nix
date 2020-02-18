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
    flags = { network-uri = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "http-client"; version = "0.6.4"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "https://github.com/snoyberg/http-client";
      url = "";
      synopsis = "An HTTP client engine";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/http-client>.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."blaze-builder" or (buildDepError "blaze-builder"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."streaming-commons" or (buildDepError "streaming-commons"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."cookie" or (buildDepError "cookie"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."array" or (buildDepError "array"))
          (hsPkgs."random" or (buildDepError "random"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."mime-types" or (buildDepError "mime-types"))
          (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
          (hsPkgs."stm" or (buildDepError "stm"))
          ] ++ (if flags.network-uri
          then [
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."network-uri" or (buildDepError "network-uri"))
            ]
          else [
            (hsPkgs."network" or (buildDepError "network"))
            ])) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (buildDepError "semigroups"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.10") (hsPkgs."unsupported-ghc-version" or (buildDepError "unsupported-ghc-version"))) ++ (pkgs.lib).optionals (system.isWindows) [
          (hsPkgs."Win32" or (buildDepError "Win32"))
          (hsPkgs."safe" or (buildDepError "safe"))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."monad-control" or (buildDepError "monad-control"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."blaze-builder" or (buildDepError "blaze-builder"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
            (hsPkgs."zlib" or (buildDepError "zlib"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."streaming-commons" or (buildDepError "streaming-commons"))
            ];
          buildable = true;
          };
        "spec-nonet" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."monad-control" or (buildDepError "monad-control"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."blaze-builder" or (buildDepError "blaze-builder"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."network-uri" or (buildDepError "network-uri"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
            (hsPkgs."zlib" or (buildDepError "zlib"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."streaming-commons" or (buildDepError "streaming-commons"))
            (hsPkgs."directory" or (buildDepError "directory"))
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
    postUnpack = "sourceRoot+=/http-client; echo source root reset to \$sourceRoot";
    }