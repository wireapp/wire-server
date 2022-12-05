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
import Brig.App (Env, wrapClient)
import Brig.Password (Password, mkSafePassword)
import Cassandra hiding (Set)
import qualified Cassandra as C
import Control.Lens ((.~))
import Control.Monad.Except
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.Id (OAuthClientId, UserId, randomId)
import Data.Misc (PlainTextPassword (PlainTextPassword))
import Data.Range
import Data.Schema
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import qualified Data.Text as T
import Data.Text.Ascii
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error as TErr
import Imports
import OpenSSL.Random (randBytes)
import Servant hiding (Handler, Tagged)
import URI.ByteString
import Wire.API.Error
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named (Named (..))
import Wire.API.Routes.Public (ZUser)

--------------------------------------------------------------------------------
-- Types
-- TODO(leif): add golden and roundtrip tests for these types

newtype RedirectUrl = RedirectUrl {unRedirectUrl :: URIRef Absolute}
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema RedirectUrl)

instance ToByteString RedirectUrl where
  builder = serializeURIRef . unRedirectUrl

instance FromByteString RedirectUrl where
  parser = RedirectUrl <$> uriParser strictURIParserOptions

instance ToSchema RedirectUrl where
  schema =
    (TE.decodeUtf8 . serializeURIRef' . unRedirectUrl)
      .= (RedirectUrl <$> parsedText "RedirectUrl" (runParser (uriParser strictURIParserOptions) . TE.encodeUtf8))

instance ToHttpApiData RedirectUrl where
  toUrlPiece = TE.decodeUtf8With TErr.lenientDecode . toHeader
  toHeader = serializeURIRef' . unRedirectUrl

instance FromHttpApiData RedirectUrl where
  parseUrlPiece = parseHeader . TE.encodeUtf8
  parseHeader = bimap (T.pack . show) RedirectUrl . parseURI strictURIParserOptions

newtype OAuthApplicationName = OAuthApplicationName {unOAuthApplicationName :: Range 1 256 Text}
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthApplicationName)

instance ToSchema OAuthApplicationName where
  schema = OAuthApplicationName <$> unOAuthApplicationName .= schema

data NewOAuthClient = NewOAuthClient
  { nocApplicationName :: OAuthApplicationName,
    nocRedirectUrl :: RedirectUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema NewOAuthClient)

instance ToSchema NewOAuthClient where
  schema =
    object "NewOAuthClient" $
      NewOAuthClient
        <$> nocApplicationName .= field "applicationName" schema
        <*> nocRedirectUrl .= field "redirectUrl" schema

newtype OAuthClientPlainTextSecret = OAuthClientPlainTextSecret {unOAuthClientPlainTextSecret :: AsciiBase16}
  deriving stock (Eq, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthClientPlainTextSecret)

instance Show OAuthClientPlainTextSecret where
  show _ = "<OAuthClientPlainTextSecret>"

instance ToSchema OAuthClientPlainTextSecret where
  schema = (toText . unOAuthClientPlainTextSecret) .= parsedText "OAuthClientPlainTextSecret" (fmap OAuthClientPlainTextSecret . validateBase16)

data OAuthClientCredentials = OAuthClientCredentials
  { occClientId :: OAuthClientId,
    occClientSecret :: OAuthClientPlainTextSecret
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthClientCredentials)

instance ToSchema OAuthClientCredentials where
  schema =
    object "OAuthClientCredentials" $
      OAuthClientCredentials
        <$> occClientId .= field "clientId" schema
        <*> occClientSecret .= field "clientSecret" schema

data OAuthClient = OAuthClient
  { ocId :: OAuthClientId,
    ocName :: OAuthApplicationName,
    ocRedirectUrl :: RedirectUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthClient)

instance ToSchema OAuthClient where
  schema =
    object "OAuthClient" $
      OAuthClient
        <$> ocId .= field "clientId" schema
        <*> ocName .= field "applicationName" schema
        <*> ocRedirectUrl .= field "redirectUrl" schema

data OAuthResponseType = OAuthResponseTypeCode
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthResponseType)

instance ToSchema OAuthResponseType where
  schema :: ValueSchema NamedSwaggerDoc OAuthResponseType
  schema =
    enum @Text "OAuthResponseType" $
      mconcat
        [ element "code" OAuthResponseTypeCode
        ]

data OAuthScope
  = ConversationCreate
  | ConversationCodeCreate
  deriving stock (Eq, Show, Generic, Ord)

instance ToByteString OAuthScope where
  builder = \case
    ConversationCreate -> "conversation:create"
    ConversationCodeCreate -> "conversation-code:create"

instance FromByteString OAuthScope where
  parser = do
    s <- parser @Text
    case s & T.toLower of
      "conversation:create" -> pure ConversationCreate
      "conversation-code:create" -> pure ConversationCodeCreate
      _ -> fail "invalid scope"

oauthScopesToText :: Set OAuthScope -> Text
oauthScopesToText = T.intercalate " " . fmap (cs . toByteString') . Set.toList

oauthScopeParser :: Text -> A.Parser (Set OAuthScope)
oauthScopeParser "" = pure Set.empty
oauthScopeParser scope =
  pure $ (not . T.null) `filter` T.splitOn " " scope & maybe Set.empty Set.fromList . mapM (fromByteString' . cs)

data NewOAuthAuthCode = NewOAuthAuthCode
  { noacClientId :: OAuthClientId,
    noacScope :: Set OAuthScope,
    noacResponseType :: OAuthResponseType,
    noacRedirectUri :: RedirectUrl,
    noacState :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema NewOAuthAuthCode)

instance ToSchema NewOAuthAuthCode where
  schema =
    object "NewOAuthAuthCode" $
      NewOAuthAuthCode
        <$> noacClientId .= field "clientId" schema
        <*> (oauthScopesToText . noacScope) .= field "scope" (withParser schema oauthScopeParser)
        <*> noacResponseType .= field "responseType" schema
        <*> noacRedirectUri .= field "redirectUri" schema
        <*> noacState .= field "state" schema

newtype OAuthAuthCode = OAuthAuthCode {unOAuthAuthCode :: AsciiBase64Url}
  deriving stock (Show, Eq, Generic)

instance ToByteString OAuthAuthCode where
  builder = builder . unOAuthAuthCode

instance FromByteString OAuthAuthCode where
  parser = OAuthAuthCode <$> parser

--------------------------------------------------------------------------------
-- API Internal

type IOAuthAPI =
  Named
    "create-oauth-client"
    ( Summary "Register an OAuth client"
        :> "i"
        :> "oauth"
        :> "clients"
        :> ReqBody '[JSON] NewOAuthClient
        :> Post '[JSON] OAuthClientCredentials
    )

internalOauthAPI :: ServerT IOAuthAPI (Handler r)
internalOauthAPI =
  Named @"create-oauth-client" createNewOAuthClient

--------------------------------------------------------------------------------
-- API Public

data OAuthError
  = OAuthClientNotFound
  | RedirectUrlMissMatch
  | UnsupportedResponseType

type instance MapError 'OAuthClientNotFound = 'StaticError 404 "not-found" "OAuth client not found"

type instance MapError 'RedirectUrlMissMatch = 'StaticError 400 "redirect-url-miss-match" "Redirect URL miss match"

type instance MapError 'UnsupportedResponseType = 'StaticError 400 "unsupported-response-type" "Unsupported response type"

type OAuthAPI =
  Named
    "get-oauth-client"
    ( Summary "Get OAuth client information"
        :> ZUser
        :> "oauth"
        :> "clients"
        :> Capture "ClientId" OAuthClientId
        :> MultiVerb
             'GET
             '[JSON]
             '[ ErrorResponse 'OAuthClientNotFound,
                Respond 200 "OAuth client found" OAuthClient
              ]
             (Maybe OAuthClient)
    )
    :<|> Named
           "create-oauth-auth-code"
           ( Summary ""
               :> ZUser
               :> "oauth"
               :> "authorization"
               :> "codes"
               :> ReqBody '[JSON] NewOAuthAuthCode
               :> MultiVerb
                    'POST
                    '[JSON]
                    '[WithHeaders '[Header "Location" RedirectUrl] RedirectUrl (RespondEmpty 302 "Found")]
                    RedirectUrl
           )

oauthAPI :: ServerT OAuthAPI (Handler r)
oauthAPI =
  Named @"get-oauth-client" getOAuthClient
    :<|> Named @"create-oauth-auth-code" createNewOAuthAuthCode

--------------------------------------------------------------------------------
-- Handlers

createNewOAuthClient :: NewOAuthClient -> (Handler r) OAuthClientCredentials
createNewOAuthClient (NewOAuthClient name uri) = do
  credentials@(OAuthClientCredentials cid secret) <- OAuthClientCredentials <$> randomId <*> createSecret
  safeSecret <- liftIO $ hashClientSecret secret
  lift $ wrapClient $ insertOAuthClient cid name uri safeSecret
  pure credentials
  where
    createSecret :: MonadIO m => m OAuthClientPlainTextSecret
    createSecret = OAuthClientPlainTextSecret <$> (liftIO . fmap encodeBase16 $ randBytes 32)

    hashClientSecret :: MonadIO m => OAuthClientPlainTextSecret -> m Password
    hashClientSecret = mkSafePassword . PlainTextPassword . toText . unOAuthClientPlainTextSecret

getOAuthClient :: UserId -> OAuthClientId -> (Handler r) (Maybe OAuthClient)
getOAuthClient _ cid = lift $ wrapClient $ lookupOauthClient cid

createNewOAuthAuthCode :: UserId -> NewOAuthAuthCode -> (Handler r) RedirectUrl
createNewOAuthAuthCode uid (NewOAuthAuthCode cid scope responseType redirectUrl state) = do
  unless (responseType == OAuthResponseTypeCode) $ throwStd (errorToWai @'UnsupportedResponseType)
  OAuthClient _ _ uri <- getOAuthClient uid cid >>= maybe (throwStd $ errorToWai @'OAuthClientNotFound) pure
  unless (uri == redirectUrl) $ throwStd $ errorToWai @'RedirectUrlMissMatch
  oauthCode <- OAuthAuthCode <$> rand32Bytes
  lift $ wrapClient $ insertOAuthAuthCode oauthCode cid uid scope redirectUrl
  let queryParams = [("code", toByteString' oauthCode), ("state", cs state)]
      returnedRedirectUrl = redirectUrl & unRedirectUrl & (queryL . queryPairsL) .~ queryParams & RedirectUrl
  pure returnedRedirectUrl

rand32Bytes :: MonadIO m => m AsciiBase64Url
rand32Bytes = liftIO . fmap encodeBase64Url $ randBytes 32

--------------------------------------------------------------------------------
-- DB

insertOAuthClient :: (MonadClient m, MonadReader Env m) => OAuthClientId -> OAuthApplicationName -> RedirectUrl -> Password -> m ()
insertOAuthClient cid name uri pw = retry x5 . write q $ params LocalQuorum (cid, name, uri, pw)
  where
    q :: PrepQuery W (OAuthClientId, OAuthApplicationName, RedirectUrl, Password) ()
    q = "INSERT INTO oauth_client (id, name, redirect_uri, secret) VALUES (?, ?, ?, ?)"

lookupOauthClient :: (MonadClient m, MonadReader Env m) => OAuthClientId -> m (Maybe OAuthClient)
lookupOauthClient cid = do
  mNameUrl <- retry x5 . query1 q $ params LocalQuorum (Identity cid)
  pure $ mNameUrl <&> uncurry (OAuthClient cid)
  where
    q :: PrepQuery R (Identity OAuthClientId) (OAuthApplicationName, RedirectUrl)
    q = "SELECT name, redirect_uri FROM oauth_client WHERE id = ?"

insertOAuthAuthCode :: (MonadClient m, MonadReader Env m) => OAuthAuthCode -> OAuthClientId -> UserId -> Set OAuthScope -> RedirectUrl -> m ()
insertOAuthAuthCode code cid uid scope uri = do
  let cqlScope = C.Set (Set.toList scope)
  retry x5 . write q $ params LocalQuorum (code, cid, uid, cqlScope, uri)
  where
    q :: PrepQuery W (OAuthAuthCode, OAuthClientId, UserId, C.Set OAuthScope, RedirectUrl) ()
    q = "INSERT INTO oauth_auth_code (code, client, user, scope, redirect_uri) VALUES (?, ?, ?, ?, ?) USING TTL 300"

--------------------------------------------------------------------------------
-- CQL instances

instance Cql OAuthApplicationName where
  ctype = Tagged TextColumn
  toCql = CqlText . fromRange . unOAuthApplicationName
  fromCql (CqlText t) = checkedEither t <&> OAuthApplicationName
  fromCql _ = Left "OAuthApplicationName: Text expected"

instance Cql RedirectUrl where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . toByteString
  fromCql (CqlBlob t) = runParser parser (toStrict t)
  fromCql _ = Left "RedirectUrl: Blob expected"

instance Cql OAuthAuthCode where
  ctype = Tagged AsciiColumn
  toCql = CqlAscii . toText . unOAuthAuthCode
  fromCql (CqlAscii t) = OAuthAuthCode <$> validateBase64Url t
  fromCql _ = Left "OAuthAuthCode: Ascii expected"

instance Cql OAuthScope where
  ctype = Tagged TextColumn
  toCql = CqlText . cs . toByteString'
  fromCql (CqlText t) = maybe (Left "invalid oauth scope") Right $ fromByteString' (cs t)
  fromCql _ = Left "OAuthScope: Text expected"
