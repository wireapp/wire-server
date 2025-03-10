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

class (FromByteString (Token a), ToByteString a) => AccessTokenLike a where
  renewAccessToken :: (MonadZAuth m) => Maybe ClientId -> Token a -> m Auth.AccessToken

instance AccessTokenLike (Access ActualUser) where
  renewAccessToken = renewAccessToken'

instance AccessTokenLike (Access LHUser) where
  renewAccessToken = renewLegalHoldAccessToken

class (FromByteString (Token u), ToByteString u) => UserTokenLike u where
  mkSomeToken :: Token u -> Auth.SomeUserToken
  mkUserToken :: (MonadZAuth m) => UserId -> Maybe ClientId -> Word32 -> UTCTime -> m (Token u)
  newUserToken :: (MonadZAuth m) => UserId -> Maybe ClientId -> m (Token u)
  newAccessToken :: (MonadZAuth m) => Token u -> m Auth.AccessToken
  newSessionToken :: (MonadThrow m, MonadZAuth m) => UserId -> Maybe ClientId -> m (Token u)
  userTTL :: Settings -> Integer

instance UserTokenLike (User ActualUser) where
  mkUserToken = mkUserToken'
  mkSomeToken = Auth.PlainUserToken
  newAccessToken = newAccessToken'
  newUserToken = newUserToken'
  newSessionToken uid = newSessionToken' uid
  userTTL = (.userTokenTimeout.userTokenTimeoutSeconds)

instance UserTokenLike (User LHUser) where
  mkUserToken = mkLegalHoldUserToken
  mkSomeToken = Auth.LHUserToken
  newUserToken = newLegalHoldUserToken
  newAccessToken = newLegalHoldAccessToken
  newSessionToken _ _ = throwM ZV.Unsupported
  userTTL = (.legalHoldUserTokenTimeout.legalHoldUserTokenTimeoutSeconds)

runCreate :: ZC.Env -> Sem '[ZC.ZAuthCreation, Embed IO] a -> IO a
runCreate env = runM . ZC.interpretZAuthCreation env

mkUserToken' :: (MonadZAuth m) => UserId -> Maybe ClientId -> Word32 -> UTCTime -> m (Token (User ActualUser))
mkUserToken' u cid r t = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $
      ZC.newToken (utcTimeToPOSIXSeconds t) U Nothing (User (toUUID u) (fmap clientToText cid) r)

newUserToken' :: (MonadZAuth m) => UserId -> Maybe ClientId -> m (Token (User ActualUser))
newUserToken' u c = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    runCreate z.private $
      let UserTokenTimeout ttl = z.settings.userTokenTimeout
       in ZC.userToken ttl (toUUID u) (fmap clientToText c) r

newSessionToken' :: (MonadZAuth m) => UserId -> Maybe ClientId -> m (Token (User ActualUser))
newSessionToken' u c = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    runCreate z.private $
      let SessionTokenTimeout ttl = z.settings.sessionTokenTimeout
       in ZC.sessionToken ttl (toUUID u) (fmap clientToText c) r

newAccessToken' :: (MonadZAuth m) => Token (User ActualUser) -> m Auth.AccessToken
newAccessToken' xt = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $ do
      let AccessTokenTimeout ttl = z.settings.accessTokenTimeout
      accessToken :: (Token (Access ActualUser)) <- ZC.accessToken1 ttl (xt.body.user) (xt.body.client)
      pure $
        bearerToken
          (Id accessToken.body.userId)
          (toByteString accessToken)
          ttl

renewAccessToken' :: (MonadZAuth m) => Maybe ClientId -> Token (Access ActualUser) -> m Auth.AccessToken
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

newBotToken :: (MonadZAuth m) => ProviderId -> BotId -> ConvId -> m (Token Bot)
newBotToken pid bid cid = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $
      ZC.botToken (toUUID pid) (toUUID (botUserId bid)) (toUUID cid)

newProviderToken :: (MonadZAuth m) => ProviderId -> m (Token Provider)
newProviderToken pid = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $
      let ProviderTokenTimeout ttl = z.settings.providerTokenTimeout
       in ZC.providerToken ttl (toUUID pid)

-- FUTUREWORK: this function is very similar to mkUserToken',
-- the differences are
-- 1) LU / U
-- 2) (mkLegalHoldUser uid r) / (mkUser uid r)
-- Possibly some duplication could be removed.
-- See https://github.com/wireapp/wire-server/pull/761/files#r318612423
mkLegalHoldUserToken ::
  (MonadZAuth m) =>
  UserId ->
  Maybe ClientId ->
  Word32 ->
  UTCTime ->
  m (Token (User LHUser))
mkLegalHoldUserToken u c r t = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $
      ZC.newToken
        (utcTimeToPOSIXSeconds t)
        LU
        Nothing
        (User (toUUID u) (fmap clientToText c) r)

newLegalHoldUserToken :: (MonadZAuth m) => UserId -> Maybe ClientId -> m (Token (User LHUser))
newLegalHoldUserToken u c = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    runCreate z.private $
      let LegalHoldUserTokenTimeout ttl = z.settings.legalHoldUserTokenTimeout
       in ZC.userToken ttl (toUUID u) (fmap clientToText c) r

newLegalHoldAccessToken :: (MonadZAuth m) => Token (User LHUser) -> m Auth.AccessToken
newLegalHoldAccessToken xt = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $ do
      let LegalHoldAccessTokenTimeout ttl = z.settings.legalHoldAccessTokenTimeout
      new :: (Token (Access LHUser)) <-
        ZC.accessToken1
          ttl
          (xt.body.user)
          (xt.body.client)
      pure $
        bearerToken
          (Id new.body.userId)
          (toByteString new)
          ttl

renewLegalHoldAccessToken ::
  (MonadZAuth m) =>
  Maybe ClientId ->
  Token (Access LHUser) ->
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
  (MonadZAuth m, ToByteString a) =>
  Token a ->
  m (Either ZV.Failure ())
validateToken t = liftZAuth $ do
  z <- ask
  liftIO
    . runM
    . runError
    . ZV.interpretZAuthValidation z.publicKeys
    $ ZV.check t

accessTokenOf :: Token (Access t) -> UserId
accessTokenOf t = Id (t.body.userId)

userTokenOf :: Token (User t) -> UserId
userTokenOf t = Id (t.body.user)

userTokenClient :: Token (User t) -> Maybe ClientId
userTokenClient t = fromByteString . T.encodeUtf8 =<< t.body.client

userTokenRand :: Token (User t) -> Word32
userTokenRand t = t.body.rand

tokenExpires :: Token a -> POSIXTime
tokenExpires t = fromIntegral (t.header.time)

tokenExpiresUTC :: Token a -> UTCTime
tokenExpiresUTC = posixSecondsToUTCTime . tokenExpires

randomValue :: IO Word32
randomValue = BS.foldl' f 0 <$> randBytes 4
  where
    f r w = shiftL r 8 .|. fromIntegral w
