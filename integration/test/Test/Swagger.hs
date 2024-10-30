module Test.Swagger where

import qualified API.Brig as BrigP
import qualified Data.ByteString as B
import qualified Data.Set as Set
import Data.String.Conversions
import GHC.Stack
import System.Exit
import System.FilePath
import System.Process
import Testlib.Assertions
import Testlib.Prelude
import UnliftIO.Temporary

existingVersions :: Set Int
existingVersions = Set.fromList [0, 1, 2, 3, 4, 5, 6, 7]

internalApis :: Set String
internalApis = Set.fromList ["brig", "cannon", "cargohold", "cannon", "spar"]

-- | See https://docs.wire.com/understand/api-client-perspective/swagger.html
testSwagger :: (HasCallStack) => App ()
testSwagger = do
  bindResponse BrigP.getApiVersions $ \resp -> do
    resp.status `shouldMatchInt` 200
    actualVersions :: Set Int <- do
      sup <- resp.json %. "supported" & asSetOf asIntegral
      dev <- resp.json %. "development" & asSetOf asIntegral
      pure $ sup <> dev
    assertBool ("unexpected actually existing versions: " <> show actualVersions)
      $
      -- make sure nobody has added a new version without adding it to `existingVersions`.
      -- ("subset" because blocked versions like v3 are not actually existing, but still
      -- documented.)
      actualVersions
      `Set.isSubsetOf` existingVersions

  bindResponse BrigP.getSwaggerPublicTOC $ \resp -> do
    resp.status `shouldMatchInt` 200
    cs resp.body `shouldContainString` "<html>"

  forM_ existingVersions $ \v -> do
    bindResponse (BrigP.getSwaggerPublicAllUI v) $ \resp -> do
      resp.status `shouldMatchInt` 200
      cs resp.body `shouldContainString` "<!DOCTYPE html>"
    bindResponse (BrigP.getSwaggerPublicAllJson v) $ \resp -> do
      resp.status `shouldMatchInt` 200
      void resp.json

  -- !
  -- FUTUREWORK: Implement BrigP.getSwaggerInternalTOC (including the end-point); make sure
  -- newly added internal APIs make this test fail if not added to `internalApis`.

  forM_ internalApis $ \api -> do
    bindResponse (BrigP.getSwaggerInternalUI api) $ \resp -> do
      resp.status `shouldMatchInt` 200
      cs resp.body `shouldContainString` "<!DOCTYPE html>"
    bindResponse (BrigP.getSwaggerInternalJson api) $ \resp -> do
      resp.status `shouldMatchInt` 200
      void resp.json

testSwaggerInternalVersionedNotFound :: (HasCallStack) => App ()
testSwaggerInternalVersionedNotFound = do
  forM_ internalApis $ \api -> do
    bindResponse (getSwaggerInternalUI api) $ \resp -> do
      resp.status `shouldMatchInt` 404
  where
    getSwaggerInternalUI :: String -> App Response
    getSwaggerInternalUI srv =
      rawBaseRequest OwnDomain Brig (ExplicitVersion 2) (joinHttpPath ["api-internal", "swagger-ui", srv])
        >>= submit "GET"

testSwaggerToc :: (HasCallStack) => App ()
testSwaggerToc = do
  forM_ ["/api/swagger-ui", "/api/swagger-ui/index.html", "/api/swagger.json"] $ \path ->
    bindResponse (get path) $ \resp -> do
      resp.status `shouldMatchInt` 200
      let body = cs @_ @String resp.body
      body `shouldMatch` html
      forM_ existingVersions $ \v ->
        body `shouldContainString` ("v" <> show v)
  where
    get :: String -> App Response
    get path = rawBaseRequest OwnDomain Brig Unversioned path >>= submit "GET"

    html :: String
    html = "<html><head></head><body><h1>OpenAPI 3.0 docs for all Wire APIs</h1>\n<p>This wire-server system provides <a href=\"https://swagger.io/resources/open-api/\">OpenAPI 3.0</a> documentation of our HTTP REST API.</p> <p>The openapi docs are correct by construction (compiled from the server code), and more or less complete.</p> <p>Some endpoints are version-controlled. </a href=\"/api-version\">Show all supported versions.</a> <a href=\"https://docs.wire.com/developer/developer/api-versioning.html\">find out more.</a>\n<h2>Public (all available versions)</h2>\nv0: \n<a href=\"/v0/api/swagger-ui\">swagger-ui</a>; \n<a href=\"/v0/api/swagger.json\">swagger.json</a>\n<br>\nv1: \n<a href=\"/v1/api/swagger-ui\">swagger-ui</a>; \n<a href=\"/v1/api/swagger.json\">swagger.json</a>\n<br>\nv2: \n<a href=\"/v2/api/swagger-ui\">swagger-ui</a>; \n<a href=\"/v2/api/swagger.json\">swagger.json</a>\n<br>\nv3: \n<a href=\"/v3/api/swagger-ui\">swagger-ui</a>; \n<a href=\"/v3/api/swagger.json\">swagger.json</a>\n<br>\nv4: \n<a href=\"/v4/api/swagger-ui\">swagger-ui</a>; \n<a href=\"/v4/api/swagger.json\">swagger.json</a>\n<br>\nv5: \n<a href=\"/v5/api/swagger-ui\">swagger-ui</a>; \n<a href=\"/v5/api/swagger.json\">swagger.json</a>\n<br>\nv6: \n<a href=\"/v6/api/swagger-ui\">swagger-ui</a>; \n<a href=\"/v6/api/swagger.json\">swagger.json</a>\n<br>\nv7: \n<a href=\"/v7/api/swagger-ui\">swagger-ui</a>; \n<a href=\"/v7/api/swagger.json\">swagger.json</a>\n<br>\n\n<h2>Internal (not version-controlled)</h2>\n<p>Openapi docs for internal endpoints are served per service. I.e. there's one for `brig`, one for `cannon`, etc..  This is because Openapi doesn't play well with multiple actions having the same combination of HTTP method and URL path.</p>\nbrig:<br>\n<a href=\"/api-internal/swagger-ui/brig\">swagger-ui</a>; \n<a href=\"/api-internal/swagger-ui/brig-swagger.json\">swagger.json</a>\n<br>\ngalley:<br>\n<a href=\"/api-internal/swagger-ui/galley\">swagger-ui</a>; \n<a href=\"/api-internal/swagger-ui/galley-swagger.json\">swagger.json</a>\n<br>\nspar:<br>\n<a href=\"/api-internal/swagger-ui/spar\">swagger-ui</a>; \n<a href=\"/api-internal/swagger-ui/spar-swagger.json\">swagger.json</a>\n<br>\ncargohold:<br>\n<a href=\"/api-internal/swagger-ui/cargohold\">swagger-ui</a>; \n<a href=\"/api-internal/swagger-ui/cargohold-swagger.json\">swagger.json</a>\n<br>\ngundeck:<br>\n<a href=\"/api-internal/swagger-ui/gundeck\">swagger-ui</a>; \n<a href=\"/api-internal/swagger-ui/gundeck-swagger.json\">swagger.json</a>\n<br>\ncannon:<br>\n<a href=\"/api-internal/swagger-ui/cannon\">swagger-ui</a>; \n<a href=\"/api-internal/swagger-ui/cannon-swagger.json\">swagger.json</a>\n<br>\nproxy:<br>\n<a href=\"/api-internal/swagger-ui/proxy\">swagger-ui</a>; \n<a href=\"/api-internal/swagger-ui/proxy-swagger.json\">swagger.json</a>\n<br>\n\n<h2>Federated API (backend-to-backend)</h2>\nbrig (v0):<br><a href=\"/v0/api-federation/swagger-ui/brig\">swagger-ui</a>; <a href=\"/v0/api-federation/swagger-ui/brig-swagger.json\">swagger.json</a><br>brig (v1):<br><a href=\"/v1/api-federation/swagger-ui/brig\">swagger-ui</a>; <a href=\"/v1/api-federation/swagger-ui/brig-swagger.json\">swagger.json</a><br>brig (v2):<br><a href=\"/v2/api-federation/swagger-ui/brig\">swagger-ui</a>; <a href=\"/v2/api-federation/swagger-ui/brig-swagger.json\">swagger.json</a><br><br>\ngalley (v0):<br><a href=\"/v0/api-federation/swagger-ui/galley\">swagger-ui</a>; <a href=\"/v0/api-federation/swagger-ui/galley-swagger.json\">swagger.json</a><br>galley (v1):<br><a href=\"/v1/api-federation/swagger-ui/galley\">swagger-ui</a>; <a href=\"/v1/api-federation/swagger-ui/galley-swagger.json\">swagger.json</a><br>galley (v2):<br><a href=\"/v2/api-federation/swagger-ui/galley\">swagger-ui</a>; <a href=\"/v2/api-federation/swagger-ui/galley-swagger.json\">swagger.json</a><br><br>\ncargohold (v0):<br><a href=\"/v0/api-federation/swagger-ui/cargohold\">swagger-ui</a>; <a href=\"/v0/api-federation/swagger-ui/cargohold-swagger.json\">swagger.json</a><br>cargohold (v1):<br><a href=\"/v1/api-federation/swagger-ui/cargohold\">swagger-ui</a>; <a href=\"/v1/api-federation/swagger-ui/cargohold-swagger.json\">swagger.json</a><br>cargohold (v2):<br><a href=\"/v2/api-federation/swagger-ui/cargohold\">swagger-ui</a>; <a href=\"/v2/api-federation/swagger-ui/cargohold-swagger.json\">swagger.json</a><br><br>\n\n</body></html>\n"

data Swagger = SwaggerPublic | SwaggerInternal Service

instance TestCases Swagger where
  mkTestCases =
    pure
      [ MkTestCase "[swagger=ibrig]" (SwaggerInternal Brig),
        MkTestCase "[swagger=icannon]" (SwaggerInternal Cannon),
        MkTestCase "[swagger=icargohold]" (SwaggerInternal Cargohold),
        MkTestCase "[swagger=igalley]" (SwaggerInternal Galley),
        MkTestCase "[swagger=igundeck]" (SwaggerInternal Gundeck),
        MkTestCase "[swagger=ispar]" (SwaggerInternal Spar),
        MkTestCase "[swagger=public]" SwaggerPublic
      ]

-- | This runs the swagger linter [vacuum](https://quobix.com/vacuum/).
--
-- The reason for adding the linter in the integration tests, and not in the lint job, is that
-- it calls brig for the swagger docs it validates, but no running brig during linting.
--
-- There is also a make rule that does this, for convenience in your develop
-- flow. Make sure that brig is running before using the make rule.
testSwaggerLint :: (HasCallStack) => Swagger -> App ()
testSwaggerLint sw = do
  withSystemTempDirectory "swagger" $ \tmp -> do
    req <- case sw of
      SwaggerPublic ->
        baseRequest OwnDomain Brig Versioned
          $ joinHttpPath ["api", "swagger.json"]
      (SwaggerInternal service) ->
        baseRequest OwnDomain Brig Unversioned
          $ joinHttpPath
            [ "api-internal",
              "swagger-ui",
              serviceName service <> "-swagger.json"
            ]
    swagger <- submit "GET" req >>= getBody 200
    liftIO $ B.writeFile (tmp </> "swagger.json") swagger
    let cmd = shell $ "vacuum lint -a -d -e " <> (tmp </> "swagger.json")
    (exitCode, out, err) <- liftIO $ readCreateProcessWithExitCode cmd ""
    case exitCode of
      ExitSuccess -> pure ()
      _ -> do
        liftIO $ putStrLn out
        liftIO $ putStrLn err
        assertFailure "swagger validation errors"
