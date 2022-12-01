{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module API.OAuth where

import Bilge
import Bilge.Assert
import Brig.API.OAuth
import Brig.Options
import Data.ByteString.Conversion (toByteString')
import Data.Id (OAuthClientId, UserId)
import Data.Range (unsafeRange)
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import URI.ByteString (parseURI, strictURIParserOptions)
import Util
import Wire.API.User

tests :: Manager -> Brig -> Opts -> TestTree
tests m b _opts = do
  testGroup "oauth" $
    [test m "register new OAuth client" $ testRegisterNewOAuthClient b]

testRegisterNewOAuthClient :: Brig -> Http ()
testRegisterNewOAuthClient brig = do
  let Right redirectUrl = RedirectUrl <$> parseURI strictURIParserOptions "https://example.com"
  let applicationName = OAuthApplicationName (unsafeRange "E Corp")
  let newOAuthClient = NewOAuthClient applicationName redirectUrl
  cid <- occClientId <$> registerNewOAuthClient brig newOAuthClient
  uid <- userId <$> randomUser brig
  oauthClientInfo <- getOAuthClientInfo brig uid cid
  liftIO $ do
    applicationName @?= ocName oauthClientInfo
    redirectUrl @?= ocRedirectUrl oauthClientInfo

-------------------------------------------------------------------------------
-- Util

registerNewOAuthClient :: HasCallStack => Brig -> NewOAuthClient -> Http OAuthClientCredentials
registerNewOAuthClient brig reqBody =
  responseJsonError =<< post (brig . paths ["i", "oauth", "clients"] . json reqBody) <!! const 200 === statusCode

getOAuthClientInfo :: HasCallStack => Brig -> UserId -> OAuthClientId -> Http OAuthClient
getOAuthClientInfo brig uid cid =
  responseJsonError =<< get (brig . paths ["oauth", "clients", toByteString' cid] . zUser uid) <!! const 200 === statusCode
