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
import           Data.UUID as UUID
import           Data.UUID.V4 as UUID

-- | A hardcoded UUID of an admin existing in the 'TestStorage'.
--
-- @00000500-0000-0000-0000-000000000001@
testAdminUUID :: UUID
testAdminUUID = UUID.fromWords 0x500 0 0 1

testStorage :: IO TestStorage
testStorage = do
  authMap <- STMMap.newIO
  atomically $ STMMap.insert (Admin testAdminUUID, "password") testAdminUUID authMap
  TestStorage <$> STMMap.newIO <*> STMMap.newIO <*> pure authMap

spec :: Spec
spec = beforeAll ((\s -> app empty (nt s)) <$> testStorage) $ do
  describe "/ServiceProviderConfig" $ do
    it "is accessible without authentication" $ do
      get "/ServiceProviderConfig" `shouldRespondWith` 200

    it "doesn't check auth credentials" $ do
      randomAdmin <- liftIO UUID.nextRandom
      request methodGet "/ServiceProviderConfig" [authHeader randomAdmin "wrongpassword"] ""
        `shouldRespondWith` 200

  describe "/Users" $ do
    it "succeeds with authentication" $ do
      request methodGet "/Users" [authHeader testAdminUUID "password"] ""
        `shouldRespondWith` 200

    it "fails if the password is wrong" $ do
      request methodGet "/Users" [authHeader testAdminUUID "wrongpassword"] ""
        `shouldRespondWith` 401

    it "fails if the admin is not found" $ do
      randomAdminUUID <- liftIO UUID.nextRandom
      request methodGet "/Users" [authHeader randomAdminUUID "password"] ""
        `shouldRespondWith` 401

    it "fails if no authentication is provided" $ do
      get "/Users" `shouldRespondWith` 401

authHeader :: UUID -> Text -> Header
authHeader user pass =
  (hAuthorization, "Basic " <> Base64.encode (toASCIIBytes user <> ":" <> encodeUtf8 pass))
