module Test.Version where

import API.Brig
import qualified Data.Set as Set
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

  unless (null (Set.intersection supported dev)) $
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
  def {brigCfg = setField "optSettings.setDisabledAPIVersions" ["v2"]}
  $ \domain -> do
    do
      user <- randomUser OwnDomain def
      void $ getSelfWithVersion (ExplicitVersion 2) user >>= getJSON 200

    do
      user <- randomUser domain def
      bindResponse (getSelfWithVersion (ExplicitVersion 2) user) $ \resp -> do
        resp.status `shouldMatchInt` 404
        resp.json %. "label" `shouldMatch` "unsupported-version"

      void $ getSelfWithVersion (ExplicitVersion 1) user >>= getJSON 200
      void $ getSelfWithVersion (ExplicitVersion 3) user >>= getJSON 200
      void $ getSelfWithVersion Unversioned user >>= getJSON 200
