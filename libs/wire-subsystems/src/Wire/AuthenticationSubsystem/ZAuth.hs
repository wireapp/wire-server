module Wire.AuthenticationSubsystem.ZAuth where

import Control.Exception (throwIO)
import Data.Aeson
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Conversion
import Data.Id
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.Encoding qualified as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.ZAuth.Creation (TokenExpiry (..))
import Data.ZAuth.Creation qualified as ZC
import Data.ZAuth.Token
import Data.ZAuth.Validation qualified as ZV
import Imports
import OpenSSL.Random
import Polysemy
import Polysemy.Input
import Sodium.Crypto.Sign
import Wire.API.User.Auth (bearerToken)
import Wire.API.User.Auth qualified as Auth

data Settings = Settings
  { -- | Secret key index to use
    --   for token creation
    keyIndex :: !Int,
    -- | User token validity timeout
    userTokenTimeout :: !UserTokenTimeout,
    -- | Session token validity timeout
    sessionTokenTimeout :: !SessionTokenTimeout,
    -- | Access token validity timeout
    accessTokenTimeout :: !AccessTokenTimeout,
    -- | Proider token validity timeout
    providerTokenTimeout :: !ProviderTokenTimeout,
    -- | Legal Hold User token validity timeout
    legalHoldUserTokenTimeout :: !LegalHoldUserTokenTimeout,
    -- | Legal Hold Access token validity timeout
    legalHoldAccessTokenTimeout :: !LegalHoldAccessTokenTimeout
  }
  deriving (Show, Generic)

defSettings :: Settings
defSettings =
  Settings
    1
    (UserTokenTimeout (60 * 60 * 24 * 28)) -- 28 days
    (SessionTokenTimeout (60 * 60 * 24)) -- 1 day
    (AccessTokenTimeout 900) -- 15 minutes
    (ProviderTokenTimeout (60 * 60 * 24 * 7)) -- 7 days
    (LegalHoldUserTokenTimeout (60 * 60 * 24 * 56)) -- 56 days
    (LegalHoldAccessTokenTimeout (60 * 15)) -- 15 minutes

data ZAuthEnv = ZAuthEnv
  { private :: !ZC.SigningKey,
    publicKeys :: !(Vector PublicKey),
    settings :: !Settings
  }

newtype UserTokenTimeout = UserTokenTimeout
  {userTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype SessionTokenTimeout = SessionTokenTimeout
  {sessionTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype AccessTokenTimeout = AccessTokenTimeout
  {accessTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype ProviderTokenTimeout = ProviderTokenTimeout
  {providerTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype LegalHoldUserTokenTimeout = LegalHoldUserTokenTimeout
  {legalHoldUserTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype LegalHoldAccessTokenTimeout = LegalHoldAccessTokenTimeout
  {legalHoldAccessTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

instance FromJSON UserTokenTimeout

instance FromJSON SessionTokenTimeout

instance FromJSON AccessTokenTimeout

instance FromJSON ProviderTokenTimeout

instance FromJSON LegalHoldAccessTokenTimeout

instance FromJSON LegalHoldUserTokenTimeout

instance FromJSON Settings where
  parseJSON = withObject "ZAuth.Settings" $ \o ->
    Settings
      <$> o .: "keyIndex"
      <*> (UserTokenTimeout <$> o .: "userTokenTimeout")
      <*> (SessionTokenTimeout <$> o .: "sessionTokenTimeout")
      <*> (AccessTokenTimeout <$> o .: "accessTokenTimeout")
      <*> (ProviderTokenTimeout <$> o .: "providerTokenTimeout")
      <*> (LegalHoldUserTokenTimeout <$> o .: "legalHoldUserTokenTimeout")
      <*> (LegalHoldAccessTokenTimeout <$> o .: "legalHoldAccessTokenTimeout")

readKeys :: (Read k) => FilePath -> IO (Maybe (NonEmpty k))
readKeys fp = nonEmpty . map read . filter (not . null) . lines <$> readFile fp

mkZAuthEnv :: NonEmpty SecretKey -> NonEmpty PublicKey -> Settings -> IO ZAuthEnv
mkZAuthEnv sk pk sets = do
  zc <- createSigningKey sets.keyIndex sk
  let pubKeys = Vector.fromList $ NonEmpty.toList pk
  pure $! ZAuthEnv zc pubKeys sets

createSigningKey :: Int -> NonEmpty SecretKey -> IO ZC.SigningKey
createSigningKey i keys = do
  let signingKey =
        if i > 0 && i <= length keys
          then keys NonEmpty.!! (i - 1)
          else error "keyIndex out of range"
  pure $ ZC.SigningKey i signingKey

class (Body t ~ Access, SerializableToken t) => AccessTokenLike t where
  renewAccessToken :: (Member (Embed IO) r, Member (Input ZAuthEnv) r) => Maybe ClientId -> Token t -> Sem r Auth.AccessToken
  accessTTL :: Settings -> Integer

instance AccessTokenLike A where
  renewAccessToken = renewAccessToken'
  accessTTL = (.accessTokenTimeout.accessTokenTimeoutSeconds)

instance AccessTokenLike LA where
  renewAccessToken = renewLegalHoldAccessToken
  accessTTL = (.legalHoldAccessTokenTimeout.legalHoldAccessTokenTimeoutSeconds)

class (Body t ~ User, AccessTokenLike (AccessTokenType t), SerializableToken t) => UserTokenLike t where
  type AccessTokenType t :: Type
  mkSomeToken :: Token t -> Auth.SomeUserToken
  allowSessionToken :: Bool
  userTTL :: Settings -> Integer

instance UserTokenLike U where
  type AccessTokenType U = A
  mkSomeToken = Auth.PlainUserToken
  allowSessionToken = True
  userTTL = (.userTokenTimeout.userTokenTimeoutSeconds)

instance UserTokenLike LU where
  type AccessTokenType LU = LA
  mkSomeToken = Auth.LHUserToken
  allowSessionToken = False
  userTTL = (.legalHoldUserTokenTimeout.legalHoldUserTokenTimeoutSeconds)

mkUserToken :: (Member (Embed IO) r, Member (Input ZAuthEnv) r, SerializableToken t, Body t ~ User) => UserId -> Maybe ClientId -> Word32 -> UTCTime -> Sem r (Token t)
mkUserToken u cid r t = do
  z <- input
  ZC.newToken z.private (TokenExpiresAt (utcTimeToPOSIXSeconds t)) Nothing (User (toUUID u) (fmap clientToText cid) r)

newUserToken :: forall t r. (Member (Embed IO) r, Member (Input ZAuthEnv) r, UserTokenLike t) => UserId -> Maybe ClientId -> Sem r (Token t)
newUserToken u c = do
  z <- input
  r <- liftIO randomValue
  let ttl = userTTL @t z.settings
   in ZC.newToken z.private (TokenExpiresAfter ttl) Nothing $ User (toUUID u) (fmap clientToText c) r

newSessionToken :: forall t r. (Member (Embed IO) r, Member (Input ZAuthEnv) r, UserTokenLike t) => UserId -> Maybe ClientId -> Sem r (Token t)
newSessionToken u c = do
  unless (allowSessionToken @t) $
    -- TODO: Is this right? It used to be MonadThrow
    liftIO (throwIO ZV.Invalid)
  z <- input
  r <- liftIO randomValue
  let SessionTokenTimeout ttl = z.settings.sessionTokenTimeout
   in ZC.newToken z.private (TokenExpiresAfter ttl) (Just S) $ User (toUUID u) (fmap clientToText c) r

newAccessToken ::
  forall t r.
  (Member (Embed IO) r, Member (Input ZAuthEnv) r, AccessTokenLike (AccessTokenType t), Body t ~ User) =>
  Token t ->
  Sem r Auth.AccessToken
newAccessToken xt = do
  z <- input
  let ttl = accessTTL @(AccessTokenType t) z.settings
  -- TODO: Test that connId is randomly generated, there was a test in
  -- zauth, which got deleted as a result of moving this code here.
  connId <- liftIO randomConnId
  accessToken :: (Token A) <-
    ZC.newToken z.private (TokenExpiresAfter ttl) Nothing $
      Access xt.body.user xt.body.client connId
  pure $
    bearerToken
      (Id accessToken.body.userId)
      (toByteString accessToken)
      ttl

renewAccessToken' :: (Member (Embed IO) r, Member (Input ZAuthEnv) r) => Maybe ClientId -> Token A -> Sem r Auth.AccessToken
renewAccessToken' mcid old = do
  z <- input
  let AccessTokenTimeout ttl = z.settings.accessTokenTimeout
  new <-
    ZC.renewToken
      z.private
      ttl
      (old.header)
      (old.body {clientId = fmap clientToText mcid})
  pure $
    bearerToken
      (Id new.body.userId)
      (toByteString new)
      ttl

newBotToken :: (Member (Embed IO) r, Member (Input ZAuthEnv) r) => ProviderId -> BotId -> ConvId -> Sem r (Token B)
newBotToken pid bid cid = do
  z <- input
  ZC.newToken z.private TokenNeverExpires Nothing $
    Bot (toUUID pid) (toUUID (botUserId bid)) (toUUID cid)

newProviderToken :: (Member (Embed IO) r, Member (Input ZAuthEnv) r) => ProviderId -> Sem r (Token P)
newProviderToken pid = do
  z <- input
  let ProviderTokenTimeout ttl = z.settings.providerTokenTimeout
   in ZC.newToken z.private (TokenExpiresAfter ttl) Nothing $ Provider (toUUID pid)

renewLegalHoldAccessToken ::
  (Member (Embed IO) r, Member (Input ZAuthEnv) r) =>
  Maybe ClientId ->
  Token LA ->
  Sem r Auth.AccessToken
renewLegalHoldAccessToken _mcid old = do
  z <- input
  let LegalHoldAccessTokenTimeout ttl = z.settings.legalHoldAccessTokenTimeout
  new <- ZC.renewToken z.private ttl (old.header) (old.body)
  pure $
    bearerToken
      (Id new.body.userId)
      (toByteString new)
      ttl

validateToken ::
  (Member (Embed IO) r, Member (Input ZAuthEnv) r, SerializableToken t) =>
  Token t ->
  Sem r (Either ZV.Failure ())
validateToken t = do
  z <- input
  ZV.check z.publicKeys t

userTokenClient :: User -> Maybe ClientId
userTokenClient u = fromByteString . T.encodeUtf8 =<< u.client

tokenExpires :: Token a -> POSIXTime
tokenExpires t = fromIntegral (t.header.time)

tokenExpiresUTC :: Token a -> UTCTime
tokenExpiresUTC = posixSecondsToUTCTime . tokenExpires

randomValue :: IO Word32
randomValue = BS.foldl' f 0 <$> randBytes 4
  where
    f r w = shiftL r 8 .|. fromIntegral w

randomConnId :: IO Word64
randomConnId = BS.foldl' f 0 <$> randBytes 8
  where
    f r w = shiftL r 8 .|. fromIntegral w
