module Test.Version where

import API.Brig
import qualified Data.Set as Set
import SetupHelpers
import Testlib.Prelude

_testVersion :: Versioned -> App ()
_testVersion v = withModifiedBackend
  def {brigCfg = setField "optSettings.setDisabledAPIVersions" ([] :: [String])}
  $ \dom ->
    bindResponse (baseRequest dom Brig v "/api-version" >>= submit "GET") $ \resp -> do
      resp.status `shouldMatchInt` 200
      dev <- resp.json %. "development" & asSet
      supported <- resp.json %. "supported" & asSet
      domain <- resp.json %. "domain" & asString
      federation <- resp.json %. "federation" & asBool

      length dev `shouldMatchInt` 1
      domain `shouldMatch` dom
      federation `shouldMatch` True

      unless (null (Set.intersection supported dev)) $
        assertFailure "development and supported versions should not overlap"

testVersion :: App ()
testVersion = _testVersion Unversioned

testVersionV1 :: App ()
testVersionV1 = _testVersion (ExplicitVersion 1)

testVersionVersioned :: App ()
testVersionVersioned = _testVersion Versioned

testVersionUnsupported :: App ()
testVersionUnsupported = bindResponse (baseRequest OwnDomain Brig (ExplicitVersion 500) "/api-version" >>= submit "GET") $
  \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "unsupported-version"

testVersionDisabled :: App ()
testVersionDisabled = withModifiedBackend
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
      void $ getSelfWithVersion (ExplicitVersion 4) user >>= getJSON 200
      void $ getSelfWithVersion (ExplicitVersion 5) user >>= getJSON 200
      void $ getSelfWithVersion (ExplicitVersion 6) user >>= getJSON 200
      void $ getSelfWithVersion Unversioned user >>= getJSON 200

testVersionDisabledNotAdvertised :: App ()
testVersionDisabledNotAdvertised = do
  allVersions <- bindResponse (baseRequest OwnDomain Brig Versioned "/api-version" >>= submit "GET") $ \resp ->
    (<>)
      <$> (resp.json %. "development" & asList >>= traverse asInt)
      <*> (resp.json %. "supported" & asList >>= traverse asInt)
  forM_ allVersions _testVersionDisabledNotAdvertised

_testVersionDisabledNotAdvertised :: Int -> App ()
_testVersionDisabledNotAdvertised v = withModifiedBackend
  def {brigCfg = setField "optSettings.setDisabledAPIVersions" ["v" <> show v]}
  $ \domain -> do
    bindResponse (getAPIVersion domain) $ \resp -> do
      resp.status `shouldMatchInt` 200
      dev <- resp.json %. "development" & asList >>= traverse asInt
      supported <- resp.json %. "supported" & asList >>= traverse asInt

      assertBool "supported versions should not be empty" $ not (null supported)
      assertBool "the disabled version should not be propagated as dev version" $ v `notElem` dev
      assertBool "the disabled version should not be propagated as supported version" $ v `notElem` supported

testVersionDisabledDevNotAdvertised :: App ()
testVersionDisabledDevNotAdvertised = withModifiedBackend
  def {brigCfg = setField "optSettings.setDisabledAPIVersions" ["development"]}
  $ \domain -> do
    bindResponse (getAPIVersion domain) $ \resp -> do
      resp.status `shouldMatchInt` 200
      dev <- resp.json %. "development" & asList
      supported <- resp.json %. "supported" & asList

      assertBool "supported versions should not be empty" $ not (null supported)
      assertBool "development versions should be empty" $ null dev

testVersionDevDisabledPerDefault :: App ()
testVersionDevDisabledPerDefault = withModifiedBackend
  def {brigCfg = removeField "optSettings.setDisabledAPIVersions"}
  $ \domain -> do
    bindResponse (getAPIVersion domain) $ \resp -> do
      resp.status `shouldMatchInt` 200
      dev <- resp.json %. "development" & asList
      supported <- resp.json %. "supported" & asList

      assertBool "supported versions should not be empty" $ not (null supported)
      assertBool "development versions should be empty" $ null dev
