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
    flags = { examples = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "aws"; version = "0.20"; };
      license = "BSD-3-Clause";
      copyright = "See contributors list in README and LICENSE file";
      maintainer = "aristidb@gmail.com";
      author = "Aristid Breitkreuz, contributors see README";
      homepage = "http://github.com/aristidb/aws";
      url = "";
      synopsis = "Amazon Web Services (AWS) for Haskell";
      description = "Bindings for Amazon Web Services (AWS), with the aim of supporting all AWS services. To see a high level overview of the library, see the README at <https://github.com/aristidb/aws/blob/master/README.md>.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."base16-bytestring" or (buildDepError "base16-bytestring"))
          (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
          (hsPkgs."blaze-builder" or (buildDepError "blaze-builder"))
          (hsPkgs."byteable" or (buildDepError "byteable"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
          (hsPkgs."cereal" or (buildDepError "cereal"))
          (hsPkgs."conduit" or (buildDepError "conduit"))
          (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."lifted-base" or (buildDepError "lifted-base"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."monad-control" or (buildDepError "monad-control"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."old-locale" or (buildDepError "old-locale"))
          (hsPkgs."resourcet" or (buildDepError "resourcet"))
          (hsPkgs."safe" or (buildDepError "safe"))
          (hsPkgs."scientific" or (buildDepError "scientific"))
          (hsPkgs."tagged" or (buildDepError "tagged"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."utf8-string" or (buildDepError "utf8-string"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."xml-conduit" or (buildDepError "xml-conduit"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.6")) (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (buildDepError "semigroups"));
        buildable = true;
        };
      exes = {
        "GetObjectV4" = {
          depends = (pkgs.lib).optionals (!(!flags.examples)) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            ];
          buildable = if !flags.examples then false else true;
          };
        "GetObject" = {
          depends = (pkgs.lib).optionals (!(!flags.examples)) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            ];
          buildable = if !flags.examples then false else true;
          };
        "GetObjectGoogle" = {
          depends = (pkgs.lib).optionals (!(!flags.examples)) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            ];
          buildable = if !flags.examples then false else true;
          };
        "MultipartUpload" = {
          depends = (pkgs.lib).optionals (!(!flags.examples)) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            ];
          buildable = if !flags.examples then false else true;
          };
        "MultipartTransfer" = {
          depends = (pkgs.lib).optionals (!(!flags.examples)) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            ];
          buildable = if !flags.examples then false else true;
          };
        "NukeBucket" = {
          depends = (pkgs.lib).optionals (!(!flags.examples)) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            ];
          buildable = if !flags.examples then false else true;
          };
        "PutBucketNearLine" = {
          depends = (pkgs.lib).optionals (!(!flags.examples)) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."conduit-extra" or (buildDepError "conduit-extra"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            ];
          buildable = if !flags.examples then false else true;
          };
        "SimpleDb" = {
          depends = (pkgs.lib).optionals (!(!flags.examples)) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."text" or (buildDepError "text"))
            ];
          buildable = if !flags.examples then false else true;
          };
        "DynamoDb" = {
          depends = (pkgs.lib).optionals (!(!flags.examples)) [
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            ];
          buildable = if !flags.examples then false else true;
          };
        "Sqs" = {
          depends = (pkgs.lib).optionals (!(!flags.examples)) [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            ];
          buildable = if !flags.examples then false else true;
          };
        };
      tests = {
        "sqs-tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."lifted-base" or (buildDepError "lifted-base"))
            (hsPkgs."monad-control" or (buildDepError "monad-control"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            (hsPkgs."tagged" or (buildDepError "tagged"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
            ];
          buildable = true;
          };
        "dynamodb-tests" = {
          depends = [
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."lifted-base" or (buildDepError "lifted-base"))
            (hsPkgs."monad-control" or (buildDepError "monad-control"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            (hsPkgs."tagged" or (buildDepError "tagged"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
            ];
          buildable = true;
          };
        "s3-tests" = {
          depends = [
            (hsPkgs."aws" or (buildDepError "aws"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."conduit" or (buildDepError "conduit"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."lifted-base" or (buildDepError "lifted-base"))
            (hsPkgs."monad-control" or (buildDepError "monad-control"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tagged" or (buildDepError "tagged"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."transformers-base" or (buildDepError "transformers-base"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/wireapp/aws";
      rev = "42695688fc20f80bf89cec845c57403954aab0a2";
      sha256 = "1484ksx44lbdgc1lhbgq77w6w2dz4syzrhgv83z6nm0ifg2iwfkf";
      });
    }