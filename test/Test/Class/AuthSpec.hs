{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Class.AuthSpec (spec) where

import           Web.SCIM.Server (app)
import           Web.SCIM.Server.Mock
import           Web.SCIM.Class.Auth (Admin (..))
import           Web.SCIM.Capabilities.MetaSchema (empty)
import           Data.Text (Text)
import           Data.Text.Encoding
import           Data.Monoid
import           Test.Hspec
import           Test.Hspec.Wai      hiding (post, put, patch)
import qualified Data.ByteString.Base64 as Base64
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method (methodGet)
import qualified STMContainers.Map   as STMMap
import           Control.Monad.STM

storage :: IO TestStorage
storage = do
  auth <- STMMap.newIO
  atomically $ STMMap.insert (Admin "admin", "password") "admin" auth
  TestStorage <$> STMMap.newIO <*> STMMap.newIO <*> pure auth

spec :: Spec
spec = beforeAll ((\s -> app empty (nt s)) =<< storage) $ do
  describe "/ServiceProviderConfig" $ do
    it "is accessible without authentication" $ do
      get "/ServiceProviderConfig" `shouldRespondWith` 200

    it "doesn't check auth credentials" $ do
      request methodGet "/ServiceProviderConfig" [authHeader "foo" "bar"] ""
        `shouldRespondWith` 200

  describe "/Users" $ do
    it "succeeds with authentication" $ do
      request methodGet "/Users" [authHeader "admin" "password"] ""
        `shouldRespondWith` 200

    it "fails if the password is wrong" $ do
      request methodGet "/Users" [authHeader "admin" "wrongpassword"] ""
        `shouldRespondWith` 401

    it "fails if the admin is not found" $ do
      request methodGet "/Users" [authHeader "foo" "password"] ""
        `shouldRespondWith` 401

    it "fails if no authentication is provided" $ do
      get "/Users" `shouldRespondWith` 401

authHeader :: Text -> Text -> Header
authHeader user pass =
  (hAuthorization, "Basic " <> Base64.encode (encodeUtf8 user <> ":" <> encodeUtf8 pass))
