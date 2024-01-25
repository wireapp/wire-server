module Test.Version where

import API.Brig
import qualified Data.Set as Set
import SetupHelpers
import Testlib.Prelude

newtype Versioned' = Versioned' Versioned

-- | This instance is used to generate tests for some of the versions. (Not checking all of them for time efficiency reasons)
instance HasTests x => HasTests (Versioned' -> x) where
  mkTests m n s f x =
    mkTests m (n <> "[version=unversioned]") s f (x (Versioned' Unversioned))
      <> mkTests m (n <> "[version=versioned]") s f (x (Versioned' Versioned))
      <> mkTests m (n <> "[version=v1]") s f (x (Versioned' (ExplicitVersion 1)))
      <> mkTests m (n <> "[version=v3]") s f (x (Versioned' (ExplicitVersion 3)))
      <> mkTests m (n <> "[version=v6]") s f (x (Versioned' (ExplicitVersion 6)))

testVersion :: Versioned' -> App ()
testVersion (Versioned' v) = withModifiedBackend
  def {brigCfg = setField "optSettings.setDisabledAPIVersions" ([] :: [String])}
  $ \dom ->
    bindResponse (baseRequest dom Brig v "/api-version" >>= submit "GET") $ \resp -> do
      resp.status `shouldMatchInt` 200
      dev <- resp.json %. "development" & asSet
      supported <- resp.json %. "supported" & asSet
      domain <- resp.json %. "domain" & asString
      federation <- resp.json %. "federation" & asBool

      -- currently there is only one development version
      -- it is however theoretically possible to have multiple development versions
      length dev `shouldMatchInt` 1
      domain `shouldMatch` dom
      federation `shouldMatch` True

      unless (null (Set.intersection supported dev)) $
        assertFailure "development and supported versions should not overlap"

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
  forM_ allVersions testWithVersion
  where
    testWithVersion :: Int -> App ()
    testWithVersion v = withModifiedBackend
      def {brigCfg = setField "optSettings.setDisabledAPIVersions" ["v" <> show v]}
      $ \domain -> do
        bindResponse (getAPIVersion domain) $ \resp -> do
          resp.status `shouldMatchInt` 200
          dev <- resp.json %. "development" & asList >>= traverse asInt
          supported <- resp.json %. "supported" & asList >>= traverse asInt

          assertBool "supported versions should not be empty" $ not (null supported)
          assertBool "the disabled version should not be propagated as dev version" $ v `notElem` dev
          assertBool "the disabled version should not be propagated as supported version" $ v `notElem` supported

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
