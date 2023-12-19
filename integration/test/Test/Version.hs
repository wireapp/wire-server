module Test.Version where

import API.Brig
import Data.Set qualified as Set
import SetupHelpers
import Testlib.Prelude

_testVersion :: Versioned -> App ()
_testVersion v = bindResponse (getAPIVersionWithVersion OwnDomain v) $ \resp -> do
  resp.status `shouldMatchInt` 200
  dev <- resp.json %. "development" & asSet
  supported <- resp.json %. "supported" & asSet
  domain <- resp.json %. "domain" & asString
  federation <- resp.json %. "federation" & asBool

  length dev `shouldMatchInt` 1
  domain `shouldMatch` OwnDomain
  federation `shouldMatch` True

  when (not (null (Set.intersection supported dev))) $
    assertFailure "development and supported versions should not overlap"

testVersion :: App ()
testVersion = _testVersion Unversioned

testVersionV1 :: App ()
testVersionV1 = _testVersion (ExplicitVersion 1)

testVersionVersioned :: App ()
testVersionVersioned = _testVersion Versioned

testUnsupportedVersion :: App ()
testUnsupportedVersion = bindResponse (getAPIVersionWithVersion OwnDomain (ExplicitVersion 500)) $
  \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "unsupported-version"

testDisabledVersion :: App ()
testDisabledVersion = withModifiedBackend
  def {brigCfg = setField "optSettings.disabledAPIVersions" ["v2"]}
  $ \domain -> do
    do
      user <- randomUser OwnDomain def
      void $ getSelfWithVersion (ExplicitVersion 2) user >>= getJSON 200

    do
      getAPIVersion domain >>= getJSON 200 >>= printJSON

      user <- randomUser domain def
      bindResponse (getSelfWithVersion (ExplicitVersion 2) user) $ \resp -> do
        resp.status `shouldMatchInt` 404
        resp.json %. "label" `shouldMatch` "unsupported-version"

-- testDisabledVersionIsUnsupported :: Opts -> Brig -> Http ()
-- testDisabledVersionIsUnsupported opts brig = do
--   uid <- userId <$> randomUser brig
--
--   get (apiVersion "v2" . brig . path "/self" . zUser uid)
--     !!! const 200 === statusCode
--
--   withSettingsOverrides
--     ( opts
--         & Opt.optionSettings
--           . Opt.disabledAPIVersions
--           ?~ [VersionExpConst V2]
--     )
--     $ do
--       err <-
--         responseJsonError
--           =<< get (apiVersion "v2" . brig . path "/self" . zUser uid)
--             <!! const 404 === statusCode
--       liftIO $
--         Wai.label err @?= "unsupported-version"
--
--       get (apiVersion "v1" . brig . path "/self" . zUser uid)
--         !!! const 200 === statusCode
--
--       get (apiVersion "v3" . brig . path "/self" . zUser uid)
--         !!! const 200 === statusCode
--
--       get (unversioned . brig . path "/self" . zUser uid)
--         !!! const 200 === statusCode
