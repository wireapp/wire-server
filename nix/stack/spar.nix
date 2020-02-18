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
      identifier = { name = "spar"; version = "0.1"; };
      license = "AGPL-3.0-only";
      copyright = "(c) 2018 Wire Swiss GmbH";
      maintainer = "Wire Swiss GmbH <backend@wire.com>";
      author = "Wire Swiss GmbH";
      homepage = "";
      url = "";
      synopsis = "User Service for SSO (Single Sign-On) provisioning and authentication.";
      description = "See README.md";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
          (hsPkgs."aeson-qq" or (buildDepError "aeson-qq"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
          (hsPkgs."bilge" or (buildDepError "bilge"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."brig-types" or (buildDepError "brig-types"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
          (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
          (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cookie" or (buildDepError "cookie"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."email-validate" or (buildDepError "email-validate"))
          (hsPkgs."errors" or (buildDepError "errors"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."extended" or (buildDepError "extended"))
          (hsPkgs."extra" or (buildDepError "extra"))
          (hsPkgs."galley-types" or (buildDepError "galley-types"))
          (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
          (hsPkgs."hscim" or (buildDepError "hscim"))
          (hsPkgs."http-api-data" or (buildDepError "http-api-data"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
          (hsPkgs."http-media" or (buildDepError "http-media"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."imports" or (buildDepError "imports"))
          (hsPkgs."insert-ordered-containers" or (buildDepError "insert-ordered-containers"))
          (hsPkgs."interpolate" or (buildDepError "interpolate"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."metrics-core" or (buildDepError "metrics-core"))
          (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."network-uri" or (buildDepError "network-uri"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
          (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
          (hsPkgs."retry" or (buildDepError "retry"))
          (hsPkgs."saml2-web-sso" or (buildDepError "saml2-web-sso"))
          (hsPkgs."scientific" or (buildDepError "scientific"))
          (hsPkgs."servant" or (buildDepError "servant"))
          (hsPkgs."servant-multipart" or (buildDepError "servant-multipart"))
          (hsPkgs."servant-server" or (buildDepError "servant-server"))
          (hsPkgs."servant-swagger" or (buildDepError "servant-swagger"))
          (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
          (hsPkgs."swagger2" or (buildDepError "swagger2"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."text-latin1" or (buildDepError "text-latin1"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."tinylog" or (buildDepError "tinylog"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."types-common" or (buildDepError "types-common"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
          (hsPkgs."uuid" or (buildDepError "uuid"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-middleware-prometheus" or (buildDepError "wai-middleware-prometheus"))
          (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."x509" or (buildDepError "x509"))
          (hsPkgs."xml-conduit" or (buildDepError "xml-conduit"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ];
        buildable = true;
        };
      exes = {
        "spar" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
            (hsPkgs."aeson-qq" or (buildDepError "aeson-qq"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."bilge" or (buildDepError "bilge"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."brig-types" or (buildDepError "brig-types"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cookie" or (buildDepError "cookie"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."email-validate" or (buildDepError "email-validate"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."galley-types" or (buildDepError "galley-types"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."hscim" or (buildDepError "hscim"))
            (hsPkgs."http-api-data" or (buildDepError "http-api-data"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."http-media" or (buildDepError "http-media"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."insert-ordered-containers" or (buildDepError "insert-ordered-containers"))
            (hsPkgs."interpolate" or (buildDepError "interpolate"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."metrics-core" or (buildDepError "metrics-core"))
            (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network-uri" or (buildDepError "network-uri"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
            (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
            (hsPkgs."retry" or (buildDepError "retry"))
            (hsPkgs."saml2-web-sso" or (buildDepError "saml2-web-sso"))
            (hsPkgs."scientific" or (buildDepError "scientific"))
            (hsPkgs."servant" or (buildDepError "servant"))
            (hsPkgs."servant-multipart" or (buildDepError "servant-multipart"))
            (hsPkgs."servant-server" or (buildDepError "servant-server"))
            (hsPkgs."servant-swagger" or (buildDepError "servant-swagger"))
            (hsPkgs."spar" or (buildDepError "spar"))
            (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
            (hsPkgs."swagger2" or (buildDepError "swagger2"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."text-latin1" or (buildDepError "text-latin1"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-middleware-prometheus" or (buildDepError "wai-middleware-prometheus"))
            (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."x509" or (buildDepError "x509"))
            (hsPkgs."xml-conduit" or (buildDepError "xml-conduit"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        "spar-integration" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."MonadRandom" or (buildDepError "MonadRandom"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
            (hsPkgs."aeson-qq" or (buildDepError "aeson-qq"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."bilge" or (buildDepError "bilge"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."brig-types" or (buildDepError "brig-types"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cookie" or (buildDepError "cookie"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."email-validate" or (buildDepError "email-validate"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."galley-types" or (buildDepError "galley-types"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."hscim" or (buildDepError "hscim"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-discover" or (buildDepError "hspec-discover"))
            (hsPkgs."hspec-wai" or (buildDepError "hspec-wai"))
            (hsPkgs."http-api-data" or (buildDepError "http-api-data"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."http-media" or (buildDepError "http-media"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."insert-ordered-containers" or (buildDepError "insert-ordered-containers"))
            (hsPkgs."interpolate" or (buildDepError "interpolate"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
            (hsPkgs."metrics-core" or (buildDepError "metrics-core"))
            (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network-uri" or (buildDepError "network-uri"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
            (hsPkgs."random" or (buildDepError "random"))
            (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
            (hsPkgs."retry" or (buildDepError "retry"))
            (hsPkgs."saml2-web-sso" or (buildDepError "saml2-web-sso"))
            (hsPkgs."scientific" or (buildDepError "scientific"))
            (hsPkgs."servant" or (buildDepError "servant"))
            (hsPkgs."servant-client" or (buildDepError "servant-client"))
            (hsPkgs."servant-multipart" or (buildDepError "servant-multipart"))
            (hsPkgs."servant-server" or (buildDepError "servant-server"))
            (hsPkgs."servant-swagger" or (buildDepError "servant-swagger"))
            (hsPkgs."silently" or (buildDepError "silently"))
            (hsPkgs."spar" or (buildDepError "spar"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
            (hsPkgs."swagger2" or (buildDepError "swagger2"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."text-latin1" or (buildDepError "text-latin1"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
            (hsPkgs."wai-middleware-prometheus" or (buildDepError "wai-middleware-prometheus"))
            (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."warp-tls" or (buildDepError "warp-tls"))
            (hsPkgs."x509" or (buildDepError "x509"))
            (hsPkgs."xml-conduit" or (buildDepError "xml-conduit"))
            (hsPkgs."xml-hamlet" or (buildDepError "xml-hamlet"))
            (hsPkgs."xml-lens" or (buildDepError "xml-lens"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            (hsPkgs."zauth" or (buildDepError "zauth"))
            ];
          buildable = true;
          };
        "spar-schema" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
            (hsPkgs."aeson-qq" or (buildDepError "aeson-qq"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."bilge" or (buildDepError "bilge"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."brig-types" or (buildDepError "brig-types"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cookie" or (buildDepError "cookie"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."email-validate" or (buildDepError "email-validate"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."galley-types" or (buildDepError "galley-types"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."hscim" or (buildDepError "hscim"))
            (hsPkgs."http-api-data" or (buildDepError "http-api-data"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."http-media" or (buildDepError "http-media"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."insert-ordered-containers" or (buildDepError "insert-ordered-containers"))
            (hsPkgs."interpolate" or (buildDepError "interpolate"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."metrics-core" or (buildDepError "metrics-core"))
            (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network-uri" or (buildDepError "network-uri"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
            (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
            (hsPkgs."retry" or (buildDepError "retry"))
            (hsPkgs."saml2-web-sso" or (buildDepError "saml2-web-sso"))
            (hsPkgs."scientific" or (buildDepError "scientific"))
            (hsPkgs."servant" or (buildDepError "servant"))
            (hsPkgs."servant-multipart" or (buildDepError "servant-multipart"))
            (hsPkgs."servant-server" or (buildDepError "servant-server"))
            (hsPkgs."servant-swagger" or (buildDepError "servant-swagger"))
            (hsPkgs."spar" or (buildDepError "spar"))
            (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
            (hsPkgs."swagger2" or (buildDepError "swagger2"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."text-latin1" or (buildDepError "text-latin1"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-middleware-prometheus" or (buildDepError "wai-middleware-prometheus"))
            (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."x509" or (buildDepError "x509"))
            (hsPkgs."xml-conduit" or (buildDepError "xml-conduit"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (buildDepError "HsOpenSSL"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
            (hsPkgs."aeson-qq" or (buildDepError "aeson-qq"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."base64-bytestring" or (buildDepError "base64-bytestring"))
            (hsPkgs."bilge" or (buildDepError "bilge"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."brig-types" or (buildDepError "brig-types"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."bytestring-conversion" or (buildDepError "bytestring-conversion"))
            (hsPkgs."case-insensitive" or (buildDepError "case-insensitive"))
            (hsPkgs."cassandra-util" or (buildDepError "cassandra-util"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cookie" or (buildDepError "cookie"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."data-default" or (buildDepError "data-default"))
            (hsPkgs."email-validate" or (buildDepError "email-validate"))
            (hsPkgs."errors" or (buildDepError "errors"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extended" or (buildDepError "extended"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."galley-types" or (buildDepError "galley-types"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."hscim" or (buildDepError "hscim"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-discover" or (buildDepError "hspec-discover"))
            (hsPkgs."http-api-data" or (buildDepError "http-api-data"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
            (hsPkgs."http-media" or (buildDepError "http-media"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."imports" or (buildDepError "imports"))
            (hsPkgs."insert-ordered-containers" or (buildDepError "insert-ordered-containers"))
            (hsPkgs."interpolate" or (buildDepError "interpolate"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
            (hsPkgs."metrics-core" or (buildDepError "metrics-core"))
            (hsPkgs."metrics-wai" or (buildDepError "metrics-wai"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."network-uri" or (buildDepError "network-uri"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
            (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
            (hsPkgs."retry" or (buildDepError "retry"))
            (hsPkgs."saml2-web-sso" or (buildDepError "saml2-web-sso"))
            (hsPkgs."scientific" or (buildDepError "scientific"))
            (hsPkgs."servant" or (buildDepError "servant"))
            (hsPkgs."servant-multipart" or (buildDepError "servant-multipart"))
            (hsPkgs."servant-server" or (buildDepError "servant-server"))
            (hsPkgs."servant-swagger" or (buildDepError "servant-swagger"))
            (hsPkgs."spar" or (buildDepError "spar"))
            (hsPkgs."string-conversions" or (buildDepError "string-conversions"))
            (hsPkgs."swagger2" or (buildDepError "swagger2"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."text-latin1" or (buildDepError "text-latin1"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."tinylog" or (buildDepError "tinylog"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."types-common" or (buildDepError "types-common"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."uri-bytestring" or (buildDepError "uri-bytestring"))
            (hsPkgs."uuid" or (buildDepError "uuid"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-middleware-prometheus" or (buildDepError "wai-middleware-prometheus"))
            (hsPkgs."wai-utilities" or (buildDepError "wai-utilities"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."x509" or (buildDepError "x509"))
            (hsPkgs."xml-conduit" or (buildDepError "xml-conduit"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../../services/spar; }) // {
    cabal-generator = "hpack";
    }