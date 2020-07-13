{-# LANGUAGE QuasiQuotes #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
