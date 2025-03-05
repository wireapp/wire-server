module Wire.AuthenticationSubsystem.ZAuth where

import Control.Lens (to, (.~), (^.))
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
import Data.ZAuth.Creation qualified as ZC
import Data.ZAuth.Token
import Data.ZAuth.Validation qualified as ZV
import Imports
import OpenSSL.Random
import Polysemy
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
    public :: !ZV.Env,
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
  let zv = ZV.mkEnv (NonEmpty.head pk) (NonEmpty.tail pk)
  pure $! Env zc zv sets

class (FromByteString (Token a), ToByteString a) => AccessTokenLike a where
  accessTokenOf :: Token a -> UserId
  accessTokenClient :: Token a -> Maybe ClientId
  renewAccessToken :: (MonadZAuth m) => Maybe ClientId -> Token a -> m Auth.AccessToken

instance AccessTokenLike Access where
  accessTokenOf = accessTokenOf'
  accessTokenClient = accessTokenClient'
  renewAccessToken = renewAccessToken'

instance AccessTokenLike LegalHoldAccess where
  accessTokenOf = legalHoldAccessTokenOf
  accessTokenClient = legalHoldAccessTokenClient
  renewAccessToken = renewLegalHoldAccessToken

class (FromByteString (Token u), ToByteString u) => UserTokenLike u where
  userTokenOf :: Token u -> UserId
  userTokenClient :: Token u -> Maybe ClientId
  mkSomeToken :: Token u -> Auth.SomeUserToken
  mkUserToken :: (MonadZAuth m) => UserId -> Maybe ClientId -> Word32 -> UTCTime -> m (Token u)
  userTokenRand :: Token u -> Word32
  newUserToken :: (MonadZAuth m) => UserId -> Maybe ClientId -> m (Token u)
  newAccessToken :: (MonadZAuth m) => Token u -> m Auth.AccessToken
  newSessionToken :: (MonadThrow m, MonadZAuth m) => UserId -> Maybe ClientId -> m (Token u)
  userTTL :: Settings -> Integer
  zauthType :: Type -- see libs/zauth/src/Token.hs

instance UserTokenLike User where
  mkUserToken = mkUserToken'
  userTokenOf = userTokenOf'
  userTokenClient = userTokenClient'
  mkSomeToken = Auth.PlainUserToken
  userTokenRand = userTokenRand'
  newAccessToken = newAccessToken'
  newUserToken = newUserToken'
  newSessionToken uid = newSessionToken' uid
  userTTL = (.userTokenTimeout.userTokenTimeoutSeconds)
  zauthType = U

instance UserTokenLike LegalHoldUser where
  mkUserToken = mkLegalHoldUserToken
  userTokenOf = legalHoldUserTokenOf
  userTokenClient = legalHoldClientTokenOf
  mkSomeToken = Auth.LHUserToken
  userTokenRand = legalHoldUserTokenRand
  newUserToken = newLegalHoldUserToken
  newAccessToken = newLegalHoldAccessToken
  newSessionToken _ _ = throwM ZV.Unsupported
  userTTL = (.legalHoldUserTokenTimeout.legalHoldUserTokenTimeoutSeconds)
  zauthType = LU

runCreate :: ZC.Env -> Sem '[ZC.ZAuthCreation, Embed IO] a -> IO a
runCreate env = runM . ZC.interpretZAuthCreation env

mkUserToken' :: (MonadZAuth m) => UserId -> Maybe ClientId -> Word32 -> UTCTime -> m (Token User)
mkUserToken' u cid r t = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $
      ZC.newToken (utcTimeToPOSIXSeconds t) U Nothing (mkUser (toUUID u) (fmap clientToText cid) r)

newUserToken' :: (MonadZAuth m) => UserId -> Maybe ClientId -> m (Token User)
newUserToken' u c = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    runCreate z.private $
      let UserTokenTimeout ttl = z.settings.userTokenTimeout
       in ZC.userToken ttl (toUUID u) (fmap clientToText c) r

newSessionToken' :: (MonadZAuth m) => UserId -> Maybe ClientId -> m (Token User)
newSessionToken' u c = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    runCreate z.private $
      let SessionTokenTimeout ttl = z.settings.sessionTokenTimeout
       in ZC.sessionToken ttl (toUUID u) (fmap clientToText c) r

newAccessToken' :: (MonadZAuth m) => Token User -> m Auth.AccessToken
newAccessToken' xt = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $ do
      let AccessTokenTimeout ttl = z.settings.accessTokenTimeout
      accessToken <- ZC.accessToken1 ttl (xt ^. body . user) (xt ^. body . client)
      pure $
        bearerToken
          (accessToken ^. body . userId . to Id)
          (toByteString accessToken)
          ttl

renewAccessToken' :: (MonadZAuth m) => Maybe ClientId -> Token Access -> m Auth.AccessToken
renewAccessToken' mcid old = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $ do
      let AccessTokenTimeout ttl = z.settings.accessTokenTimeout
      new <-
        ZC.renewToken
          ttl
          (old ^. header)
          ( (old ^. body)
              & clientId .~ fmap clientToText mcid
          )
      pure $
        bearerToken
          (new ^. body . userId . to Id)
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
  m (Token LegalHoldUser)
mkLegalHoldUserToken u c r t = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $
      ZC.newToken
        (utcTimeToPOSIXSeconds t)
        LU
        Nothing
        (mkLegalHoldUser (toUUID u) (fmap clientToText c) r)

newLegalHoldUserToken :: (MonadZAuth m) => UserId -> Maybe ClientId -> m (Token LegalHoldUser)
newLegalHoldUserToken u c = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    runCreate z.private $
      let LegalHoldUserTokenTimeout ttl = z.settings.legalHoldUserTokenTimeout
       in ZC.legalHoldUserToken ttl (toUUID u) (fmap clientToText c) r

newLegalHoldAccessToken :: (MonadZAuth m) => Token LegalHoldUser -> m Auth.AccessToken
newLegalHoldAccessToken xt = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $ do
      let LegalHoldAccessTokenTimeout ttl = z.settings.legalHoldAccessTokenTimeout
      new <-
        ZC.legalHoldAccessToken1
          ttl
          (xt ^. body . legalHoldUser . user)
          (xt ^. body . legalHoldUser . client)
      pure $
        bearerToken
          (new ^. body . legalHoldAccess . userId . to Id)
          (toByteString new)
          ttl

renewLegalHoldAccessToken ::
  (MonadZAuth m) =>
  Maybe ClientId ->
  Token LegalHoldAccess ->
  m Auth.AccessToken
renewLegalHoldAccessToken _mcid old = liftZAuth $ do
  z <- ask
  liftIO $
    runCreate z.private $ do
      let LegalHoldAccessTokenTimeout ttl = z.settings.legalHoldAccessTokenTimeout
      new <- ZC.renewToken ttl (old ^. header) (old ^. body)
      pure $
        bearerToken
          (new ^. body . legalHoldAccess . userId . to Id)
          (toByteString new)
          ttl

validateToken ::
  (MonadZAuth m, ToByteString a) =>
  Token a ->
  m (Either ZV.Failure ())
validateToken t = liftZAuth $ do
  z <- ask
  void <$> ZV.runValidate z.public (ZV.check t)

accessTokenOf' :: Token Access -> UserId
accessTokenOf' t = Id (t ^. body . userId)

accessTokenClient' :: Token Access -> Maybe ClientId
accessTokenClient' t = fromByteString . T.encodeUtf8 =<< t ^. body . clientId

userTokenOf' :: Token User -> UserId
userTokenOf' t = Id (t ^. body . user)

userTokenClient' :: Token User -> Maybe ClientId
userTokenClient' t = fromByteString . T.encodeUtf8 =<< t ^. body . client

legalHoldAccessTokenOf :: Token LegalHoldAccess -> UserId
legalHoldAccessTokenOf t = Id (t ^. body . legalHoldAccess . userId)

legalHoldAccessTokenClient :: Token LegalHoldAccess -> Maybe ClientId
legalHoldAccessTokenClient t = fromByteString . T.encodeUtf8 =<< t ^. body . legalHoldAccess . clientId

legalHoldUserTokenOf :: Token LegalHoldUser -> UserId
legalHoldUserTokenOf t = Id (t ^. body . legalHoldUser . user)

legalHoldClientTokenOf :: Token LegalHoldUser -> Maybe ClientId
legalHoldClientTokenOf t = fromByteString . T.encodeUtf8 =<< t ^. body . legalHoldUser . client

userTokenRand' :: Token User -> Word32
userTokenRand' t = t ^. body . rand

legalHoldUserTokenRand :: Token LegalHoldUser -> Word32
legalHoldUserTokenRand t = t ^. body . legalHoldUser . rand

tokenExpires :: Token a -> POSIXTime
tokenExpires t = fromIntegral (t ^. header . time)

tokenExpiresUTC :: Token a -> UTCTime
tokenExpiresUTC = posixSecondsToUTCTime . tokenExpires

randomValue :: IO Word32
randomValue = BS.foldl' f 0 <$> randBytes 4
  where
    f r w = shiftL r 8 .|. fromIntegral w
