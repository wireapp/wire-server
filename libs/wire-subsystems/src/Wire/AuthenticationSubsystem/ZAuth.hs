module Wire.AuthenticationSubsystem.ZAuth where

import Control.Monad.Catch
import Data.Aeson
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Conversion
import Data.Id
import Data.List.NonEmpty (NonEmpty, nonEmpty)
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
import Polysemy.Error
import Sodium.Crypto.Sign
import Wire.API.User.Auth (bearerToken)
import Wire.API.User.Auth qualified as Auth

newtype ZAuth a = ZAuth {unZAuth :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

class (MonadIO m) => MonadZAuth m where
  liftZAuth :: ZAuth a -> m a

instance MonadZAuth ZAuth where
  liftZAuth = id

runZAuth :: (MonadIO m) => Env -> ZAuth a -> m a
runZAuth e za = liftIO $ runReaderT (unZAuth za) e

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

data Env = Env
  { private :: !ZC.Env,
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

mkEnv :: NonEmpty SecretKey -> NonEmpty PublicKey -> Settings -> IO Env
mkEnv sk pk sets = do
  zc <- ZC.mkEnv sets.keyIndex (NonEmpty.head sk) (NonEmpty.tail sk)
  let pubKeys = Vector.fromList $ NonEmpty.toList pk
  pure $! Env zc pubKeys sets

class (Body t ~ Access, SerializableToken t) => AccessTokenLike t where
  renewAccessToken :: (MonadZAuth m) => Maybe ClientId -> Token t -> m Auth.AccessToken
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
  newSessionToken :: (MonadThrow m, MonadZAuth m) => UserId -> Maybe ClientId -> m (Token t)
  userTTL :: Settings -> Integer

instance UserTokenLike U where
  type AccessTokenType U = A
  mkSomeToken = Auth.PlainUserToken
  newSessionToken uid = newSessionToken' uid
  userTTL = (.userTokenTimeout.userTokenTimeoutSeconds)

instance UserTokenLike LU where
  type AccessTokenType LU = LA
  mkSomeToken = Auth.LHUserToken
  newSessionToken _ _ = throwM ZV.Unsupported
  userTTL = (.legalHoldUserTokenTimeout.legalHoldUserTokenTimeoutSeconds)

runCreate :: ZC.Env -> Sem '[ZC.ZAuthCreation, Embed IO] a -> IO a
runCreate env = runM . ZC.interpretZAuthCreation env

mkUserToken :: (MonadZAuth m, SerializableToken t, Body t ~ User) => UserId -> Maybe ClientId -> Word32 -> UTCTime -> m (Token t)
mkUserToken u cid r t = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $
      ZC.newToken (TokenExpiresAt (utcTimeToPOSIXSeconds t)) Nothing (User (toUUID u) (fmap clientToText cid) r)

newUserToken :: forall t m. (MonadZAuth m, UserTokenLike t) => UserId -> Maybe ClientId -> m (Token t)
newUserToken u c = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    runCreate z.private $
      let ttl = userTTL @t z.settings
       in ZC.newToken (TokenExpiresAfter ttl) Nothing $ User (toUUID u) (fmap clientToText c) r

newSessionToken' :: (MonadZAuth m) => UserId -> Maybe ClientId -> m (Token U)
newSessionToken' u c = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    runCreate z.private $
      let SessionTokenTimeout ttl = z.settings.sessionTokenTimeout
       in ZC.newToken (TokenExpiresAfter ttl) (Just S) $ User (toUUID u) (fmap clientToText c) r

newAccessToken ::
  forall t m.
  (MonadZAuth m, AccessTokenLike (AccessTokenType t), Body t ~ User) =>
  Token t ->
  m Auth.AccessToken
newAccessToken xt = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $ do
      let ttl = accessTTL @(AccessTokenType t) z.settings
      -- TODO: Test that connId is randomly generated, there was a test in
      -- zauth, which got deleted as a result of moving this code here.
      connId <- liftIO randomConnId
      accessToken :: (Token A) <-
        ZC.newToken (TokenExpiresAfter ttl) Nothing $
          Access xt.body.user xt.body.client connId
      pure $
        bearerToken
          (Id accessToken.body.userId)
          (toByteString accessToken)
          ttl

renewAccessToken' :: (MonadZAuth m) => Maybe ClientId -> Token A -> m Auth.AccessToken
renewAccessToken' mcid old = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $ do
      let AccessTokenTimeout ttl = z.settings.accessTokenTimeout
      new <-
        ZC.renewToken
          ttl
          (old.header)
          (old.body {clientId = fmap clientToText mcid})
      pure $
        bearerToken
          (Id new.body.userId)
          (toByteString new)
          ttl

newBotToken :: (MonadZAuth m) => ProviderId -> BotId -> ConvId -> m (Token B)
newBotToken pid bid cid = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $
      ZC.newToken TokenNeverExpires Nothing $
        Bot (toUUID pid) (toUUID (botUserId bid)) (toUUID cid)

newProviderToken :: (MonadZAuth m) => ProviderId -> m (Token P)
newProviderToken pid = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $
      let ProviderTokenTimeout ttl = z.settings.providerTokenTimeout
       in ZC.newToken (TokenExpiresAfter ttl) Nothing $ Provider (toUUID pid)

renewLegalHoldAccessToken ::
  (MonadZAuth m) =>
  Maybe ClientId ->
  Token LA ->
  m Auth.AccessToken
renewLegalHoldAccessToken _mcid old = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $ do
      let LegalHoldAccessTokenTimeout ttl = z.settings.legalHoldAccessTokenTimeout
      new <- ZC.renewToken ttl (old.header) (old.body)
      pure $
        bearerToken
          (Id new.body.userId)
          (toByteString new)
          ttl

validateToken ::
  (MonadZAuth m, SerializableToken t) =>
  Token t ->
  m (Either ZV.Failure ())
validateToken t = liftZAuth $ do
  z <- ask
  liftIO
    . runM
    . runError
    . ZV.interpretZAuthValidation z.publicKeys
    $ ZV.check t

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
