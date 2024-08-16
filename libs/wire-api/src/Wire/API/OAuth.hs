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

module Wire.API.OAuth where

import Cassandra hiding (Set)
import Control.Lens (preview, view, (%~), (?~))
import Crypto.Hash as Crypto
import Crypto.JWT hiding (Context, params, uri, verify)
import Data.Aeson.KeyMap qualified as M
import Data.Aeson.Types qualified as A
import Data.ByteArray (convert)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Either.Combinators (mapLeft)
import Data.HashMap.Strict qualified as HM
import Data.Id as Id
import Data.Json.Util
import Data.OpenApi (ToParamSchema (..))
import Data.OpenApi qualified as S
import Data.Range
import Data.Schema
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Ascii
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error as TErr
import Data.Time
import GHC.TypeLits (Nat, symbolVal)
import Imports hiding (exp, head)
import Prelude.Singletons (Show_)
import Servant hiding (Handler, JSON, Tagged, addHeader, respond)
import Servant.OpenApi.Internal.Orphans ()
import Test.QuickCheck (Arbitrary (..))
import URI.ByteString
import URI.ByteString.QQ qualified as URI.QQ
import Web.FormUrlEncoded (Form (..), FromForm (..), ToForm (..), parseUnique)
import Wire.API.Error
import Wire.Arbitrary (GenericUniform (..))

--------------------------------------------------------------------------------
-- Types

newtype RedirectUrl = RedirectUrl {unRedirectUrl :: URIRef Absolute}
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema RedirectUrl)

addParams :: [(ByteString, ByteString)] -> RedirectUrl -> RedirectUrl
addParams ps (RedirectUrl uri) = uri & (queryL . queryPairsL) %~ (ps <>) & RedirectUrl

instance ToParamSchema RedirectUrl where
  toParamSchema _ = toParamSchema (Proxy @Text)

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

instance Arbitrary RedirectUrl where
  arbitrary = pure $ RedirectUrl [URI.QQ.uri|https://example.com|]

type OAuthApplicationNameMinLength = (6 :: Nat)

type OAuthApplicationNameMaxLength = (256 :: Nat)

newtype OAuthApplicationName = OAuthApplicationName {unOAuthApplicationName :: Range OAuthApplicationNameMinLength OAuthApplicationNameMaxLength Text}
  deriving (Eq, Show, Generic, Ord, Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthApplicationName)

instance ToSchema OAuthApplicationName where
  schema = OAuthApplicationName <$> unOAuthApplicationName .= schema

data OAuthClientConfig = OAuthClientConfig
  { applicationName :: OAuthApplicationName,
    redirectUrl :: RedirectUrl
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OAuthClientConfig)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthClientConfig)

instance ToSchema OAuthClientConfig where
  schema =
    object "OAuthClientConfig" $
      OAuthClientConfig
        <$> applicationName
          .= fieldWithDocModifier "application_name" applicationNameDescription schema
        <*> (.redirectUrl)
          .= fieldWithDocModifier "redirect_url" redirectUrlDescription schema
    where
      applicationNameDescription = description ?~ "The name of the application. This will be shown to the user when they are asked to authorize the application. The name must be between " <> minL <> " and " <> maxL <> " characters long."
      redirectUrlDescription = description ?~ "The URL to redirect to after the user has authorized the application."
      minL = T.pack $ symbolVal $ Proxy @(Show_ OAuthApplicationNameMinLength)
      maxL = T.pack $ symbolVal $ Proxy @(Show_ OAuthApplicationNameMaxLength)

newtype OAuthClientPlainTextSecret = OAuthClientPlainTextSecret {unOAuthClientPlainTextSecret :: AsciiBase16}
  deriving (Eq, Generic, Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthClientPlainTextSecret)

instance Show OAuthClientPlainTextSecret where
  show _ = "<OAuthClientPlainTextSecret>"

instance ToSchema OAuthClientPlainTextSecret where
  schema :: ValueSchema NamedSwaggerDoc OAuthClientPlainTextSecret
  schema = (toText . unOAuthClientPlainTextSecret) .= parsedText "OAuthClientPlainTextSecret" (fmap OAuthClientPlainTextSecret . validateBase16)

instance FromHttpApiData OAuthClientPlainTextSecret where
  parseQueryParam = bimap T.pack OAuthClientPlainTextSecret . validateBase16

instance ToHttpApiData OAuthClientPlainTextSecret where
  toQueryParam = toText . unOAuthClientPlainTextSecret

data OAuthClientCredentials = OAuthClientCredentials
  { clientId :: OAuthClientId,
    clientSecret :: OAuthClientPlainTextSecret
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OAuthClientCredentials)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthClientCredentials)

instance ToSchema OAuthClientCredentials where
  schema =
    object "OAuthClientCredentials" $
      OAuthClientCredentials
        <$> (.clientId)
          .= fieldWithDocModifier "client_id" clientIdDescription schema
        <*> (.clientSecret)
          .= fieldWithDocModifier "client_secret" clientSecretDescription schema
    where
      clientIdDescription = description ?~ "The ID of the application."
      clientSecretDescription = description ?~ "The secret of the application."

data OAuthClient = OAuthClient
  { clientId :: OAuthClientId,
    name :: OAuthApplicationName,
    redirectUrl :: RedirectUrl
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OAuthClient)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthClient)

instance ToSchema OAuthClient where
  schema =
    object "OAuthClient" $
      OAuthClient
        <$> (.clientId)
          .= field "client_id" schema
        <*> (.name)
          .= field "application_name" schema
        <*> (.redirectUrl)
          .= field "redirect_url" schema

data OAuthResponseType = OAuthResponseTypeCode
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OAuthResponseType)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthResponseType)

instance ToSchema OAuthResponseType where
  schema :: ValueSchema NamedSwaggerDoc OAuthResponseType
  schema =
    enum @Text "OAuthResponseType" $
      mconcat
        [ element "code" OAuthResponseTypeCode
        ]

-- | The OAuth scopes that are supported by the backend.
-- This type is a bit redundant and unfortunately has to be kept in sync
-- with the supported scopes defined in the nginx configs.
-- However, having this typed makes it easier to handle scopes in the backend,
-- and e.g. provide more meaningful error messages when the scope is invalid.
data OAuthScope
  = ReadFeatureConfigs
  | ReadSelf
  | WriteConversations
  | WriteConversationsCode
  deriving (Eq, Show, Generic, Ord)
  deriving (Arbitrary) via (GenericUniform OAuthScope)

class IsOAuthScope scope where
  toOAuthScope :: OAuthScope

instance IsOAuthScope 'WriteConversations where
  toOAuthScope = WriteConversations

instance IsOAuthScope 'WriteConversationsCode where
  toOAuthScope = WriteConversationsCode

instance IsOAuthScope 'ReadSelf where
  toOAuthScope = ReadSelf

instance IsOAuthScope 'ReadFeatureConfigs where
  toOAuthScope = ReadFeatureConfigs

instance ToByteString OAuthScope where
  builder = \case
    WriteConversations -> "write:conversations"
    WriteConversationsCode -> "write:conversations_code"
    ReadSelf -> "read:self"
    ReadFeatureConfigs -> "read:feature_configs"

instance FromByteString OAuthScope where
  parser = do
    s <- parser
    case T.toLower s of
      "write:conversations" -> pure WriteConversations
      "write:conversations_code" -> pure WriteConversationsCode
      "read:self" -> pure ReadSelf
      "read:feature_configs" -> pure ReadFeatureConfigs
      _ -> fail "invalid scope"

newtype OAuthScopes = OAuthScopes {unOAuthScopes :: Set OAuthScope}
  deriving (Eq, Show, Generic, Monoid, Semigroup, Arbitrary)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthScopes)

instance ToSchema OAuthScopes where
  schema = OAuthScopes <$> (oauthScopesToText . unOAuthScopes) .= withParser schema oauthScopeParser
    where
      oauthScopesToText :: Set OAuthScope -> Text
      oauthScopesToText =
        T.intercalate " "
          . fmap (TE.decodeUtf8With lenientDecode . toByteString')
          . Set.toList

      oauthScopeParser :: Text -> A.Parser (Set OAuthScope)
      oauthScopeParser scope =
        pure $
          (not . T.null)
            `filter` T.splitOn " " scope
            & maybe Set.empty Set.fromList
              . mapM (fromByteString' . fromStrict . TE.encodeUtf8)

data CodeChallengeMethod = S256
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CodeChallengeMethod)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema CodeChallengeMethod)

instance ToSchema CodeChallengeMethod where
  schema :: ValueSchema NamedSwaggerDoc CodeChallengeMethod
  schema =
    enum @Text "CodeChallengeMethod" $
      mconcat
        [ element "S256" S256
        ]

newtype OAuthCodeVerifier = OAuthCodeVerifier {unOAuthCodeVerifier :: Range 43 128 Text}
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OAuthCodeVerifier)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthCodeVerifier)

instance ToSchema OAuthCodeVerifier where
  schema :: ValueSchema NamedSwaggerDoc OAuthCodeVerifier
  schema = OAuthCodeVerifier <$> unOAuthCodeVerifier .= schema

instance FromHttpApiData OAuthCodeVerifier where
  parseQueryParam = fmap OAuthCodeVerifier . mapLeft T.pack . checkedEither

instance ToHttpApiData OAuthCodeVerifier where
  toQueryParam = fromRange . unOAuthCodeVerifier

newtype OAuthCodeChallenge = OAuthCodeChallenge {unOAuthCodeChallenge :: Text}
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OAuthCodeChallenge)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthCodeChallenge)

instance ToSchema OAuthCodeChallenge where
  schema = named "OAuthCodeChallenge" $ unOAuthCodeChallenge .= fmap OAuthCodeChallenge (unnamed schema)

instance ToByteString OAuthCodeChallenge where
  builder = builder . unOAuthCodeChallenge

instance FromByteString OAuthCodeChallenge where
  parser = OAuthCodeChallenge <$> parser

verifyCodeChallenge :: OAuthCodeVerifier -> OAuthCodeChallenge -> Bool
verifyCodeChallenge verifier challenge = challenge == mkChallenge verifier

mkChallenge :: OAuthCodeVerifier -> OAuthCodeChallenge
mkChallenge =
  OAuthCodeChallenge
    . toText
    . encodeBase64UrlUnpadded
    . convert
    . Crypto.hash @ByteString @Crypto.SHA256
    . TE.encodeUtf8
    . fromRange
    . unOAuthCodeVerifier

data CreateOAuthAuthorizationCodeRequest = CreateOAuthAuthorizationCodeRequest
  { clientId :: OAuthClientId,
    scope :: OAuthScopes,
    responseType :: OAuthResponseType,
    redirectUri :: RedirectUrl,
    state :: Text,
    codeChallengeMethod :: CodeChallengeMethod,
    codeChallenge :: OAuthCodeChallenge
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CreateOAuthAuthorizationCodeRequest)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema CreateOAuthAuthorizationCodeRequest)

instance ToSchema CreateOAuthAuthorizationCodeRequest where
  schema =
    object "CreateOAuthAuthorizationCodeRequest" $
      CreateOAuthAuthorizationCodeRequest
        <$> (.clientId)
          .= fieldWithDocModifier "client_id" clientIdDescription schema
        <*> (.scope)
          .= fieldWithDocModifier "scope" scopeDescription schema
        <*> (.responseType)
          .= fieldWithDocModifier "response_type" responseTypeDescription schema
        <*> (.redirectUri)
          .= fieldWithDocModifier "redirect_uri" redirectUriDescription schema
        <*> (.state)
          .= fieldWithDocModifier "state" stateDescription schema
        <*> (.codeChallengeMethod)
          .= fieldWithDocModifier "code_challenge_method" codeChallengeMethodDescription schema
        <*> (.codeChallenge)
          .= fieldWithDocModifier "code_challenge" codeChallengeDescription schema
    where
      clientIdDescription = description ?~ "The ID of the OAuth client"
      scopeDescription = description ?~ "The scopes which are requested to get authorization for, separated by a space"
      responseTypeDescription = description ?~ "Indicates which authorization flow to use. Use `code` for authorization code flow."
      redirectUriDescription = description ?~ "The URL to which to redirect the browser after authorization has been granted by the user."
      stateDescription = description ?~ "An opaque value used by the client to maintain state between the request and callback. The authorization server includes this value when redirecting the user-agent back to the client.  The parameter SHOULD be used for preventing cross-site request forgery"
      codeChallengeMethodDescription = description ?~ "The method used to encode the code challenge. Only `S256` is supported."
      codeChallengeDescription = description ?~ "Generated by the client from the code verifier (unpadded base64url-encoded SHA256 hash of the code verifier)"

newtype OAuthAuthorizationCode = OAuthAuthorizationCode {unOAuthAuthorizationCode :: AsciiBase16}
  deriving (Eq, Generic, Arbitrary)

instance Show OAuthAuthorizationCode where
  show _ = "<OAuthAuthorizationCode>"

instance ToSchema OAuthAuthorizationCode where
  schema = (toText . unOAuthAuthorizationCode) .= parsedText "OAuthAuthorizationCode" (fmap OAuthAuthorizationCode . validateBase16)

instance ToByteString OAuthAuthorizationCode where
  builder = builder . unOAuthAuthorizationCode

instance FromByteString OAuthAuthorizationCode where
  parser = OAuthAuthorizationCode <$> parser

instance FromHttpApiData OAuthAuthorizationCode where
  parseQueryParam = bimap T.pack OAuthAuthorizationCode . validateBase16

instance ToHttpApiData OAuthAuthorizationCode where
  toQueryParam = toText . unOAuthAuthorizationCode

data OAuthGrantType = OAuthGrantTypeAuthorizationCode | OAuthGrantTypeRefreshToken
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OAuthGrantType)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthGrantType)

instance ToSchema OAuthGrantType where
  schema =
    enum @Text "OAuthGrantType" $
      mconcat
        [ element "authorization_code" OAuthGrantTypeAuthorizationCode,
          element "refresh_token" OAuthGrantTypeRefreshToken
        ]

instance FromByteString OAuthGrantType where
  parser = do
    s <- parser
    case T.toLower s of
      "authorization_code" -> pure OAuthGrantTypeAuthorizationCode
      "refresh_token" -> pure OAuthGrantTypeRefreshToken
      _ -> fail "invalid OAuthGrantType"

instance ToByteString OAuthGrantType where
  builder = \case
    OAuthGrantTypeAuthorizationCode -> "authorization_code"
    OAuthGrantTypeRefreshToken -> "refresh_token"

instance FromHttpApiData OAuthGrantType where
  parseQueryParam = maybe (Left "invalid OAuthGrantType") pure . fromByteString . TE.encodeUtf8

instance ToHttpApiData OAuthGrantType where
  toQueryParam = TE.decodeUtf8With lenientDecode . toStrict . toByteString

data OAuthAccessTokenRequest = OAuthAccessTokenRequest
  { grantType :: OAuthGrantType,
    clientId :: OAuthClientId,
    codeVerifier :: OAuthCodeVerifier,
    code :: OAuthAuthorizationCode,
    redirectUri :: RedirectUrl
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OAuthAccessTokenRequest)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthAccessTokenRequest)

instance ToSchema OAuthAccessTokenRequest where
  schema =
    object "OAuthAccessTokenRequest" $
      OAuthAccessTokenRequest
        <$> (.grantType)
          .= fieldWithDocModifier "grant_type" grantTypeDescription schema
        <*> (.clientId)
          .= fieldWithDocModifier "client_id" clientIdDescription schema
        <*> (.codeVerifier)
          .= fieldWithDocModifier "code_verifier" codeVerifierDescription schema
        <*> (.code)
          .= fieldWithDocModifier "code" codeDescription schema
        <*> (.redirectUri)
          .= fieldWithDocModifier "redirect_uri" redirectUrlDescription schema
    where
      grantTypeDescription = description ?~ "Indicates which authorization flow to use. Use `authorization_code` for authorization code flow."
      clientIdDescription = description ?~ "The ID of the OAuth client"
      codeVerifierDescription = description ?~ "The code verifier to complete the code challenge"
      codeDescription = description ?~ "The authorization code"
      redirectUrlDescription = description ?~ "The URL must match the URL that was used to generate the authorization code."

instance FromForm OAuthAccessTokenRequest where
  fromForm f =
    OAuthAccessTokenRequest
      <$> parseUnique "grant_type" f
      <*> parseUnique "client_id" f
      <*> parseUnique "code_verifier" f
      <*> parseUnique "code" f
      <*> parseUnique "redirect_uri" f

instance ToForm OAuthAccessTokenRequest where
  toForm req =
    Form $
      mempty
        & HM.insert "grant_type" [toQueryParam (req.grantType)]
        & HM.insert "client_id" [toQueryParam (req.clientId)]
        & HM.insert "code_verifier" [toQueryParam (req.codeVerifier)]
        & HM.insert "code" [toQueryParam (req.code)]
        & HM.insert "redirect_uri" [toQueryParam (req.redirectUri)]

data OAuthAccessTokenType = OAuthAccessTokenTypeBearer
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OAuthAccessTokenType)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthAccessTokenType)

instance ToSchema OAuthAccessTokenType where
  schema =
    enum @Text "OAuthAccessTokenType" $
      mconcat
        [ element "Bearer" OAuthAccessTokenTypeBearer
        ]

data TokenTag = Access | Refresh

newtype OAuthToken a = OAuthToken {unOAuthToken :: SignedJWT}
  deriving (Show, Eq, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema (OAuthToken a)

instance ToByteString (OAuthToken a) where
  builder = builder . encodeCompact . unOAuthToken

instance FromByteString (OAuthToken a) where
  parser = do
    t <- parser @Text
    case decodeCompact (fromStrict (TE.encodeUtf8 t)) of
      Left (err :: JWTError) -> fail $ show err
      Right jwt -> pure $ OAuthToken jwt

instance ToHttpApiData (OAuthToken a) where
  toHeader = toByteString'
  toUrlPiece = TE.decodeUtf8With lenientDecode . toHeader

instance FromHttpApiData (OAuthToken a) where
  parseHeader = either (Left . T.pack) pure . runParser parser
  parseUrlPiece = parseHeader . TE.encodeUtf8

instance ToSchema (OAuthToken a) where
  schema =
    (TE.decodeUtf8 . toByteString')
      .= withParser
        schema
        ( either fail pure
            . runParser parser
            . TE.encodeUtf8
        )

type OAuthAccessToken = OAuthToken 'Access

type OAuthRefreshToken = OAuthToken 'Refresh

data OAuthAccessTokenResponse = OAuthAccessTokenResponse
  { accessToken :: OAuthAccessToken,
    tokenType :: OAuthAccessTokenType,
    expiresIn :: NominalDiffTime,
    refreshToken :: OAuthRefreshToken
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthAccessTokenResponse)

instance ToSchema OAuthAccessTokenResponse where
  schema =
    object "OAuthAccessTokenResponse" $
      OAuthAccessTokenResponse
        <$> accessToken
          .= fieldWithDocModifier "access_token" accessTokenDescription schema
        <*> tokenType
          .= fieldWithDocModifier "token_type" tokenTypeDescription schema
        <*> expiresIn
          .= fieldWithDocModifier "expires_in" expiresInDescription (fromIntegral <$> roundDiffTime .= schema)
        <*> (.refreshToken)
          .= fieldWithDocModifier "refresh_token" refreshTokenDescription schema
    where
      roundDiffTime :: NominalDiffTime -> Int32
      roundDiffTime = round
      accessTokenDescription = description ?~ "The access token, which has a relatively short lifetime"
      tokenTypeDescription = description ?~ "The type of the access token. Currently only `Bearer` is supported."
      expiresInDescription = description ?~ "The lifetime of the access token in seconds"
      refreshTokenDescription = description ?~ "The refresh token, which has a relatively long lifetime, and can be used to obtain a new access token"

data OAuthClaimsSet = OAuthClaimsSet {jwtClaims :: ClaimsSet, scope :: OAuthScopes}
  deriving (Eq, Show, Generic)

instance HasClaimsSet OAuthClaimsSet where
  claimsSet f s = fmap (\a' -> s {jwtClaims = a'}) (f (jwtClaims s))

instance A.FromJSON OAuthClaimsSet where
  parseJSON = A.withObject "OAuthClaimsSet" $ \o ->
    OAuthClaimsSet
      <$> A.parseJSON (A.Object o)
      <*> o
        A..: "scope"

instance A.ToJSON OAuthClaimsSet where
  toJSON s =
    ins "scope" (s.scope) (A.toJSON (jwtClaims s))
    where
      ins k v (A.Object o) = A.Object $ M.insert k (A.toJSON v) o
      ins _ _ a = a

hcsSub :: (HasClaimsSet hcs) => hcs -> Maybe (Id a)
hcsSub =
  view claimSub
    >=> preview string
    >=> either (const Nothing) pure
    . parseIdFromText

-- | Verify a JWT and return the claims set. Use this function if you have a custom claims set.
verify :: JWK -> SignedJWT -> IO (Either JWTError OAuthClaimsSet)
verify key token = do
  let audCheck = const True
  runJOSE $ verifyJWT (defaultJWTValidationSettings audCheck) key token

-- | Verify a JWT and return the claims set. Use this if you are using the default claims set.
verify' :: JWK -> SignedJWT -> IO (Either JWTError ClaimsSet)
verify' key token = do
  let audCheck = const True
  runJOSE (verifyClaims (defaultJWTValidationSettings audCheck) key token)

data OAuthRefreshTokenInfo = OAuthRefreshTokenInfo
  { refreshTokenId :: OAuthRefreshTokenId,
    clientId :: OAuthClientId,
    userId :: UserId,
    scopes :: OAuthScopes,
    createdAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

data OAuthRefreshAccessTokenRequest = OAuthRefreshAccessTokenRequest
  { grantType :: OAuthGrantType,
    clientId :: OAuthClientId,
    refreshToken :: OAuthRefreshToken
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthRefreshAccessTokenRequest)

instance ToSchema OAuthRefreshAccessTokenRequest where
  schema :: ValueSchema NamedSwaggerDoc OAuthRefreshAccessTokenRequest
  schema =
    object "OAuthRefreshAccessTokenRequest" $
      OAuthRefreshAccessTokenRequest
        <$> (.grantType)
          .= fieldWithDocModifier "grant_type" grantTypeDescription schema
        <*> (.clientId)
          .= fieldWithDocModifier "client_id" clientIdDescription schema
        <*> (.refreshToken)
          .= fieldWithDocModifier "refresh_token" refreshTokenDescription schema
    where
      grantTypeDescription = description ?~ "The grant type. Must be `refresh_token`"
      clientIdDescription = description ?~ "The OAuth client's ID"
      refreshTokenDescription = description ?~ "The refresh token"

instance FromForm OAuthRefreshAccessTokenRequest where
  fromForm :: Form -> Either Text OAuthRefreshAccessTokenRequest
  fromForm f =
    OAuthRefreshAccessTokenRequest
      <$> parseUnique "grant_type" f
      <*> parseUnique "client_id" f
      <*> parseUnique "refresh_token" f

instance ToForm OAuthRefreshAccessTokenRequest where
  toForm req =
    Form $
      mempty
        & HM.insert "grant_type" [toQueryParam (req.grantType)]
        & HM.insert "client_id" [toQueryParam (req.clientId)]
        & HM.insert "refresh_token" [toQueryParam (req.refreshToken)]

instance FromForm (Either OAuthAccessTokenRequest OAuthRefreshAccessTokenRequest) where
  fromForm :: Form -> Either Text (Either OAuthAccessTokenRequest OAuthRefreshAccessTokenRequest)
  fromForm f = choose (fromForm @OAuthAccessTokenRequest f) (fromForm @OAuthRefreshAccessTokenRequest f)
    where
      choose :: Either Text OAuthAccessTokenRequest -> Either Text OAuthRefreshAccessTokenRequest -> Either Text (Either OAuthAccessTokenRequest OAuthRefreshAccessTokenRequest)
      choose (Right a) _ = Right (Left a)
      choose _ (Right a) = Right (Right a)
      choose (Left err) _ = Left err

data OAuthRevokeRefreshTokenRequest = OAuthRevokeRefreshTokenRequest
  { clientId :: OAuthClientId,
    refreshToken :: OAuthRefreshToken
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthRevokeRefreshTokenRequest)

instance ToSchema OAuthRevokeRefreshTokenRequest where
  schema =
    object "OAuthRevokeRefreshTokenRequest" $
      OAuthRevokeRefreshTokenRequest
        <$> (.clientId)
          .= fieldWithDocModifier "client_id" clientIdDescription schema
        <*> (.refreshToken)
          .= fieldWithDocModifier "refresh_token" refreshTokenDescription schema
    where
      clientIdDescription = description ?~ "The OAuth client's ID"
      refreshTokenDescription = description ?~ "The refresh token"

data OAuthSession = OAuthSession
  { refreshTokenId :: OAuthRefreshTokenId,
    createdAt :: UTCTimeMillis
  }
  deriving (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform OAuthSession)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthSession)

instance ToSchema OAuthSession where
  schema =
    object "OAuthSession" $
      OAuthSession
        <$> (.refreshTokenId) .= fieldWithDocModifier "refresh_token_id" refreshTokenIdDescription schema
        <*> (.createdAt) .= fieldWithDocModifier "created_at" createdAtDescription schema
    where
      refreshTokenIdDescription = description ?~ "The ID of the refresh token"
      createdAtDescription = description ?~ "The time when the session was created"

data OAuthApplication = OAuthApplication
  { applicationId :: OAuthClientId,
    name :: OAuthApplicationName,
    sessions :: [OAuthSession]
  }
  deriving (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform OAuthApplication)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema OAuthApplication)

instance ToSchema OAuthApplication where
  schema =
    object "OAuthApplication" $
      OAuthApplication
        <$> applicationId .= fieldWithDocModifier "id" idDescription schema
        <*> (.name) .= fieldWithDocModifier "name" nameDescription schema
        <*> sessions .= fieldWithDocModifier "sessions" sessionsDescription (array schema)
    where
      idDescription = description ?~ "The OAuth client's ID"
      nameDescription = description ?~ "The OAuth client's name"
      sessionsDescription = description ?~ "The OAuth client's sessions"

--------------------------------------------------------------------------------
-- Errors

data OAuthError
  = OAuthClientNotFound
  | OAuthRedirectUrlMissMatch
  | OAuthUnsupportedResponseType
  | OAuthJwtError
  | OAuthAuthorizationCodeNotFound
  | OAuthFeatureDisabled
  | OAuthInvalidClientCredentials
  | OAuthInvalidGrantType
  | OAuthInvalidRefreshToken
  | OAuthInvalidGrant

instance (Typeable (MapError e), KnownError (MapError e)) => IsSwaggerError (e :: OAuthError) where
  addToOpenApi = addStaticErrorToSwagger @(MapError e)

type instance MapError 'OAuthClientNotFound = 'StaticError 404 "not-found" "OAuth client not found"

type instance MapError 'OAuthRedirectUrlMissMatch = 'StaticError 400 "redirect-url-miss-match" "The redirect URL does not match the one registered with the client"

type instance MapError 'OAuthUnsupportedResponseType = 'StaticError 400 "unsupported-response-type" "Unsupported response type"

type instance MapError 'OAuthJwtError = 'StaticError 500 "jwt-error" "Internal error while handling JWT token"

type instance MapError 'OAuthAuthorizationCodeNotFound = 'StaticError 404 "not-found" "OAuth authorization code not found"

type instance MapError 'OAuthFeatureDisabled = 'StaticError 403 "forbidden" "OAuth is disabled"

type instance MapError 'OAuthInvalidClientCredentials = 'StaticError 403 "forbidden" "Invalid client credentials"

type instance MapError 'OAuthInvalidGrantType = 'StaticError 403 "forbidden" "Invalid grant type"

type instance MapError 'OAuthInvalidRefreshToken = 'StaticError 403 "forbidden" "Invalid refresh token"

type instance MapError 'OAuthInvalidGrant = 'StaticError 403 "invalid_grant" "Invalid grant"

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

instance Cql OAuthAuthorizationCode where
  ctype = Tagged AsciiColumn
  toCql = CqlAscii . toText . unOAuthAuthorizationCode
  fromCql (CqlAscii t) = OAuthAuthorizationCode <$> validateBase16 t
  fromCql _ = Left "OAuthAuthorizationCode: Ascii expected"

instance Cql OAuthScope where
  ctype = Tagged TextColumn
  toCql = CqlText . TE.decodeUtf8With lenientDecode . toByteString'
  fromCql (CqlText t) =
    maybe (Left "invalid oauth scope") Right
      $ fromByteString'
        . fromStrict
        . TE.encodeUtf8
      $ t
  fromCql _ = Left "OAuthScope: Text expected"

instance Cql OAuthCodeChallenge where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . toByteString
  fromCql (CqlBlob t) = runParser parser (toStrict t)
  fromCql _ = Left "OAuthCodeChallenge: Blob expected"
