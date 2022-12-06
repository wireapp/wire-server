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
import Control.Lens
import Data.ByteString.Conversion (fromByteString, fromByteString', toByteString')
import Data.Id (OAuthClientId, UserId, randomId)
import Data.Range (unsafeRange)
import Data.Set as Set
import Data.String.Conversions (cs)
import Imports
import qualified Network.Wai.Utilities as Error
import Test.Tasty
import Test.Tasty.HUnit
import URI.ByteString
import Util
import Wire.API.User

tests :: Manager -> Brig -> Opts -> TestTree
tests m b _opts = do
  testGroup "oauth" $
    [ test m "register new OAuth client" $ testRegisterNewOAuthClient b,
      test m "create oauth code - success" $ testCreateOAuthCodeSuccess b,
      test m "create oauth code - oauth client not found" $ testCreateOAuthCodeClientNotFound b,
      test m "create oauth code - redirect url mismatch" $ testCreateOAuthCodeRedirectUrlMismatch b
    ]

testRegisterNewOAuthClient :: Brig -> Http ()
testRegisterNewOAuthClient brig = do
  let newOAuthClient@(NewOAuthClient expectedAppName expectedUrl) = newOAuthClientRequestBody "E Corp" "https://example.com"
  cid <- occClientId <$> registerNewOAuthClient brig newOAuthClient
  uid <- userId <$> randomUser brig
  oauthClientInfo <- getOAuthClientInfo brig uid cid
  liftIO $ do
    expectedAppName @?= ocName oauthClientInfo
    expectedUrl @?= ocRedirectUrl oauthClientInfo

testCreateOAuthCodeSuccess :: Brig -> Http ()
testCreateOAuthCodeSuccess brig = do
  let newOAuthClient@(NewOAuthClient _ redirectUrl) = newOAuthClientRequestBody "E Corp" "https://example.com"
  cid <- occClientId <$> registerNewOAuthClient brig newOAuthClient
  uid <- userId <$> randomUser brig
  let scope = OAuthScopes $ Set.fromList [ConversationCreate, ConversationCodeCreate]
  let state = "foobar"
  createOAuthCode brig uid (NewOAuthAuthCode cid scope OAuthResponseTypeCode redirectUrl state) !!! do
    const 302 === statusCode
    const (Just $ unRedirectUrl redirectUrl ^. pathL) === (fmap getPath . getLocation)
    const (Just $ ["code", "state"]) === (fmap (fmap fst . getQueryParams) . getLocation)
    const (Just $ cs state) === (getLocation >=> getQueryParamValue "state")
  where
    getLocation :: ResponseLBS -> Maybe RedirectUrl
    getLocation = getHeader "Location" >=> fromByteString

    getPath :: RedirectUrl -> ByteString
    getPath (RedirectUrl uri) = uri ^. pathL

    getQueryParams :: RedirectUrl -> [(ByteString, ByteString)]
    getQueryParams (RedirectUrl uri) = uri ^. (queryL . queryPairsL)

    getQueryParamValue :: ByteString -> RedirectUrl -> Maybe ByteString
    getQueryParamValue key uri = snd <$> find ((== key) . fst) (getQueryParams uri)

testCreateOAuthCodeRedirectUrlMismatch :: Brig -> Http ()
testCreateOAuthCodeRedirectUrlMismatch brig = do
  cid <- occClientId <$> registerNewOAuthClient brig (newOAuthClientRequestBody "E Corp" "https://example.com")
  uid <- userId <$> randomUser brig
  let differentUrl = fromMaybe (error "invalid url") $ fromByteString' "https://wire.com"
  createOAuthCode brig uid (NewOAuthAuthCode cid (OAuthScopes Set.empty) OAuthResponseTypeCode differentUrl "") !!! do
    const 400 === statusCode
    const (Just "redirect-url-miss-match") === fmap Error.label . responseJsonMaybe

testCreateOAuthCodeClientNotFound :: Brig -> Http ()
testCreateOAuthCodeClientNotFound brig = do
  cid <- randomId
  uid <- userId <$> randomUser brig
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' "https://example.com"
  createOAuthCode brig uid (NewOAuthAuthCode cid (OAuthScopes Set.empty) OAuthResponseTypeCode redirectUrl "") !!! do
    const 404 === statusCode
    const (Just "not-found") === fmap Error.label . responseJsonMaybe

-------------------------------------------------------------------------------
-- Util

newOAuthClientRequestBody :: Text -> Text -> NewOAuthClient
newOAuthClientRequestBody name url =
  let redirectUrl = fromMaybe (error "invalid url") $ fromByteString' (cs url)
      applicationName = OAuthApplicationName (unsafeRange name)
   in NewOAuthClient applicationName redirectUrl

registerNewOAuthClient :: HasCallStack => Brig -> NewOAuthClient -> Http OAuthClientCredentials
registerNewOAuthClient brig reqBody =
  responseJsonError =<< post (brig . paths ["i", "oauth", "clients"] . json reqBody) <!! const 200 === statusCode

getOAuthClientInfo :: HasCallStack => Brig -> UserId -> OAuthClientId -> Http OAuthClient
getOAuthClientInfo brig uid cid =
  responseJsonError =<< get (brig . paths ["oauth", "clients", toByteString' cid] . zUser uid) <!! const 200 === statusCode

createOAuthCode :: HasCallStack => Brig -> UserId -> NewOAuthAuthCode -> Http ResponseLBS
createOAuthCode brig uid reqBody = post (brig . paths ["oauth", "authorization", "codes"] . zUser uid . json reqBody . noRedirect)
