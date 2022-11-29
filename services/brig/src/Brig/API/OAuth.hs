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

module Brig.API.OAuth where

import Brig.API.Error (throwStd)
import Brig.API.Handler (Handler)
import Control.Monad.Except
import Crypto.Random.DRBG
import qualified Data.Aeson as A
import Data.ByteString.Conversion
import Data.Id (OAuthClientId, randomId)
import Data.Range
import Data.Schema
import qualified Data.Swagger as S
import Data.Text.Ascii
import qualified Data.Text.Encoding as TE
import Imports
import Network.HTTP.Types
import qualified Network.Wai.Utilities.Error as Wai
import Servant hiding (Handler)
import URI.ByteString
import Wire.API.Routes.Named (Named (..))

--------------------------------------------------------------------------------
-- Types

newtype RedirectUrl = RedirectUrl {unRedirectUrl :: URIRef Absolute}
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema RedirectUrl)

instance ToSchema RedirectUrl where
  schema =
    (TE.decodeUtf8 . serializeURIRef' . unRedirectUrl)
      .= (RedirectUrl <$> parsedText "RedirectUrl" (runParser (uriParser strictURIParserOptions) . TE.encodeUtf8))

data NewOAuthClient = NewOAuthClient
  { nocApplicationName :: Range 1 256 Text,
    nocRedirectURI :: RedirectUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema NewOAuthClient)

instance ToSchema NewOAuthClient where
  schema =
    object "NewOAuthClient" $
      NewOAuthClient
        <$> nocApplicationName .= field "applicationName" schema
        <*> nocRedirectURI .= field "redirectUri" schema

newtype OAuthClientSecret = OAuthClientSecret {unOAuthClientSecret :: AsciiBase16}
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthClientSecret)

instance ToSchema OAuthClientSecret where
  schema = (toText . unOAuthClientSecret) .= parsedText "OAuthClientSecret" (fmap OAuthClientSecret . validateBase16)

data OAuthClientCredentials = OAuthClientCredentials
  { occClientId :: OAuthClientId,
    occClientSecret :: OAuthClientSecret
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthClientCredentials)

instance ToSchema OAuthClientCredentials where
  schema =
    object "OAuthClientCredentials" $
      OAuthClientCredentials
        <$> occClientId .= field "clientId" schema
        <*> occClientSecret .= field "clientSecret" schema

--------------------------------------------------------------------------------
-- API

type OAuthAPI =
  Named
    "create-oauth-client"
    ( Summary "Register an OAuth client"
        :> "i"
        :> "oauth"
        :> "clients"
        :> ReqBody '[JSON] NewOAuthClient
        :> Post '[JSON] OAuthClientCredentials
    )

oauthAPI :: ServerT OAuthAPI (Handler r)
oauthAPI =
  Named @"create-oauth-client" createNewOAuthClient

--------------------------------------------------------------------------------
-- Handlers

createNewOAuthClient :: NewOAuthClient -> (Handler r) OAuthClientCredentials
createNewOAuthClient _ = do
  OAuthClientCredentials
    <$> randomId
    <*> (createSecret >>= either (const $ throwStd $ oauthClientSecretGenError) pure)
  where
    createSecret :: MonadIO m => m (Either GenError OAuthClientSecret)
    createSecret = do
      g :: CtrDRBG <- liftIO newGenIO
      pure $ genBytes 32 g <&> OAuthClientSecret . encodeBase16 . fst

-------------------------------------------------------------------------------
-- Errors

oauthClientSecretGenError :: Wai.Error
oauthClientSecretGenError = Wai.mkError status500 "oauth-client-secret-gen-error" "OAuth client secret generation failed."
