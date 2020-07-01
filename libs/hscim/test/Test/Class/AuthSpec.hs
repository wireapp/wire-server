{-# LANGUAGE QuasiQuotes #-}

module Test.Class.AuthSpec
  ( spec,
  )
where

import Data.Text (Text)
import Data.Text.Encoding
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method (methodGet)
import qualified StmContainers.Map as STMMap
import Test.Hspec
import Test.Hspec.Wai hiding (patch, post, put)
import Web.Scim.Capabilities.MetaSchema (empty)
import Web.Scim.Server (app)
import Web.Scim.Server.Mock

testStorage :: IO TestStorage
testStorage = TestStorage <$> STMMap.newIO <*> STMMap.newIO

spec :: Spec
spec = beforeAll ((\s -> app @Mock empty (nt s)) <$> testStorage) $ do
  describe "/ServiceProviderConfig" $ do
    it "is accessible without authentication" $ do
      get "/ServiceProviderConfig" `shouldRespondWith` 200
    it "doesn't check auth credentials" $ do
      request methodGet "/ServiceProviderConfig" [authHeader "blah"] ""
        `shouldRespondWith` 200
  describe "/Users" $ do
    it "succeeds with authentication" $ do
      request methodGet "/Users" [authHeader "authorized"] ""
        `shouldRespondWith` 200
    it "fails if the auth token is invalid" $ do
      request methodGet "/Users" [authHeader "blah"] ""
        `shouldRespondWith` 401
    it "fails if no authentication is provided" $ do
      get "/Users" `shouldRespondWith` 401

authHeader :: Text -> Header
authHeader token = (hAuthorization, encodeUtf8 token)
