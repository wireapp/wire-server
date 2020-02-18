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
      identifier = { name = "wai-routing"; version = "0.13.0"; };
      license = "MPL-2.0";
      copyright = "(C) 2014-2016 Toralf Wittner";
      maintainer = "Toralf Wittner <tw@dtex.org>";
      author = "Toralf Wittner";
      homepage = "https://gitlab.com/twittner/wai-routing/";
      url = "";
      synopsis = "Declarative routing for WAI.";
      description = "Enables the declaration of \\\"routes\\\" which handle requests to a\nspecific URL.\n\nThe set of possible handlers can be restricted by \\\"predicates\\\",\nwhich operate on WAI requests and have to be true or else the\nhandler will not be called.\n\nExample:\n\n>import Data.ByteString (ByteString)\n>import Data.Text (Text)\n>import Network.Wai\n>import Network.Wai.Predicate\n>import Network.Wai.Routing\n>import Network.Wai.Handler.Warp\n>\n>main :: IO ()\n>main = run 8080 (route (prepare start))\n>\n>start :: Monad m => Routes a m ()\n>start = do\n>    get \"/user/:name\" (continue fetchUser) \$\n>        capture \"name\"\n>\n>    get \"/user/find\" (continue findUser) \$\n>        query \"byName\" ||| query \"byId\"\n>\n>    delete \"/user/:name\" rmUser \$\n>        capture \"name\" .&. opt (cookie \"foo\")\n>\n>fetchUser :: Monad m => Text -> m Response\n>fetchUser name = ...\n>\n>findUser :: Monad m => Either ByteString Word64 -> m Response\n>findUser (Left  name)  = ...\n>findUser (Right ident) = ...\n>\n>rmUser :: Monad m => Text ::: Maybe Int -> Continue m -> m ResponseReceived\n>rmUser (name ::: foo) k = k \$ ...";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
          (hsPkgs."cookie" or (buildDepError "cookie"))
          (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-predicates" or (buildDepError "wai-predicates"))
          (hsPkgs."wai-route" or (buildDepError "wai-route"))
          ];
        buildable = true;
        };
      tests = {
        "wai-routing-tests" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."blaze-builder" or (buildDepError "blaze-builder"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-predicates" or (buildDepError "wai-predicates"))
            (hsPkgs."wai-routing" or (buildDepError "wai-routing"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "wai-routing-bench" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."criterion" or (buildDepError "criterion"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-predicates" or (buildDepError "wai-predicates"))
            (hsPkgs."wai-routing" or (buildDepError "wai-routing"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://gitlab.com/twittner/wai-routing";
      rev = "7e996a93fec5901767f845a50316b3c18e51a61d";
      sha256 = "18icwks9jc6sy42vcvj2ysaip2s0dsrpvm9sy608b6nq6kk1ahlk";
      });
    }