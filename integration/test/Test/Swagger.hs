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
existingVersions = Set.fromList [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

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
      forM_ existingVersions $ \v ->
        body `shouldContainString` ("\nv" <> show v <> ":")
  where
    get :: String -> App Response
    get path = rawBaseRequest OwnDomain Brig Unversioned path >>= submit "GET"

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
