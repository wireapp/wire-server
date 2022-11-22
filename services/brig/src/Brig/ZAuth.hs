{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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

-- | 'zauth' token signing and verification.
--
-- REFACTOR: should this be moved to @/libs/zauth@?
module Brig.ZAuth
  ( -- * Monad
    ZAuth,
    MonadZAuth (..),
    runZAuth,

    -- * Env
    Env,
    mkEnv,
    readKeys,

    -- * Settings
    settings,
    Settings (..),
    defSettings,
    localSettings,
    keyIndex,
    SessionTokenTimeout (..),
    sessionTokenTimeout,
    ProviderTokenTimeout (..),
    providerTokenTimeout,

    -- * timeout settings for access and legalholdaccess
    settingsTTL,
    userTTL,

    -- * Token Creation
    Token,
    mkUserToken,
    mkSomeToken,
    newUserToken,
    newSessionToken,
    newAccessToken,
    newProviderToken,
    newBotToken,
    renewAccessToken,

    -- * TODO find a better names?
    UserTokenLike,
    AccessTokenLike,
    TokenPair,

    -- * Token Validation
    validateToken,
    ZV.Failure (..),

    -- * Token Inspection
    accessTokenOf,
    accessTokenClient,
    userTokenOf,
    userTokenClient,
    legalHoldAccessTokenOf,
    legalHoldUserTokenOf,
    userTokenRand,
    tokenExpires,
    tokenExpiresUTC,
    tokenKeyIndex,
    zauthType,

    -- * Re-exports
    SecretKey,
    PublicKey,
  )
where

import Control.Lens (Lens', makeLenses, over, (.~), (^.))
import Control.Monad.Catch
import Data.Aeson
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import Data.Id hiding (client)
import qualified Data.Id
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.ZAuth.Creation as ZC
import Data.ZAuth.Token
import qualified Data.ZAuth.Validation as ZV
import Imports
import OpenSSL.Random
import Sodium.Crypto.Sign
import qualified Wire.API.User.Auth as Auth

newtype ZAuth a = ZAuth {unZAuth :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

class MonadIO m => MonadZAuth m where
  liftZAuth :: ZAuth a -> m a

instance MonadZAuth ZAuth where
  liftZAuth = id

runZAuth :: MonadIO m => Env -> ZAuth a -> m a
runZAuth e za = liftIO $ runReaderT (unZAuth za) e

data Settings = Settings
  { -- | Secret key index to use
    --   for token creation
    _keyIndex :: !Int,
    -- | User token validity timeout
    _userTokenTimeout :: !UserTokenTimeout,
    -- | Session token validity timeout
    _sessionTokenTimeout :: !SessionTokenTimeout,
    -- | Access token validity timeout
    _accessTokenTimeout :: !AccessTokenTimeout,
    -- | Proider token validity timeout
    _providerTokenTimeout :: !ProviderTokenTimeout,
    -- | Legal Hold User token validity timeout
    _legalHoldUserTokenTimeout :: !LegalHoldUserTokenTimeout,
    -- | Legal Hold Access token validity timeout
    _legalHoldAccessTokenTimeout :: !LegalHoldAccessTokenTimeout
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
  { _private :: !ZC.Env,
    _public :: !ZV.Env,
    _settings :: !Settings
  }

newtype UserTokenTimeout = UserTokenTimeout
  {_userTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype SessionTokenTimeout = SessionTokenTimeout
  {sessionTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype AccessTokenTimeout = AccessTokenTimeout
  {_accessTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype ProviderTokenTimeout = ProviderTokenTimeout
  {providerTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype LegalHoldUserTokenTimeout = LegalHoldUserTokenTimeout
  {_legalHoldUserTokenTimeoutSeconds :: Integer}
  deriving (Show, Generic)

newtype LegalHoldAccessTokenTimeout = LegalHoldAccessTokenTimeout
  {_legalHoldAccessTokenTimeoutSeconds :: Integer}
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

makeLenses ''LegalHoldAccessTokenTimeout

makeLenses ''AccessTokenTimeout

makeLenses ''UserTokenTimeout

makeLenses ''LegalHoldUserTokenTimeout

makeLenses ''Settings

makeLenses ''Env

localSettings :: (Settings -> Settings) -> ZAuth a -> ZAuth a
localSettings f za = ZAuth (local (over settings f) (unZAuth za))

readKeys :: Read k => FilePath -> IO (Maybe (NonEmpty k))
readKeys fp = nonEmpty . map read . filter (not . null) . lines <$> readFile fp

mkEnv :: NonEmpty SecretKey -> NonEmpty PublicKey -> Settings -> IO Env
mkEnv sk pk sets = do
  zc <- ZC.mkEnv (NonEmpty.head sk) (NonEmpty.tail sk)
  let zv = ZV.mkEnv (NonEmpty.head pk) (NonEmpty.tail pk)
  pure $! Env zc zv sets

class (UserTokenLike u, AccessTokenLike a) => TokenPair u a where
  newAccessToken :: MonadZAuth m => Token u -> m (Token a)

instance TokenPair User Access where
  newAccessToken = newAccessToken'

instance TokenPair LegalHoldUser LegalHoldAccess where
  newAccessToken = newLegalHoldAccessToken

class (FromByteString (Token a), ToByteString a) => AccessTokenLike a where
  accessTokenOf :: Token a -> UserId
  accessTokenClient :: Token a -> Maybe ClientId
  renewAccessToken :: MonadZAuth m => Maybe ClientId -> Token a -> m (Token a)
  settingsTTL :: Proxy a -> Lens' Settings Integer

instance AccessTokenLike Access where
  accessTokenOf = accessTokenOf'
  accessTokenClient = accessTokenClient'
  renewAccessToken = renewAccessToken'
  settingsTTL _ = accessTokenTimeout . accessTokenTimeoutSeconds

instance AccessTokenLike LegalHoldAccess where
  accessTokenOf = legalHoldAccessTokenOf
  accessTokenClient = legalHoldAccessTokenClient
  renewAccessToken = renewLegalHoldAccessToken
  settingsTTL _ = legalHoldAccessTokenTimeout . legalHoldAccessTokenTimeoutSeconds

class (FromByteString (Token u), ToByteString u) => UserTokenLike u where
  userTokenOf :: Token u -> UserId
  userTokenClient :: Token u -> Maybe ClientId
  mkSomeToken :: Token u -> Auth.SomeUserToken
  mkUserToken :: MonadZAuth m => UserId -> Maybe ClientId -> Word32 -> UTCTime -> m (Token u)
  userTokenRand :: Token u -> Word32
  newUserToken :: MonadZAuth m => UserId -> Maybe ClientId -> m (Token u)
  newSessionToken :: (MonadThrow m, MonadZAuth m) => UserId -> Maybe ClientId -> m (Token u)
  userTTL :: Proxy u -> Lens' Settings Integer
  zauthType :: Type -- see libs/zauth/src/Token.hs

instance UserTokenLike User where
  mkUserToken = mkUserToken'
  userTokenOf = userTokenOf'
  userTokenClient = userTokenClient'
  mkSomeToken = Auth.PlainUserToken
  userTokenRand = userTokenRand'
  newUserToken = newUserToken'
  newSessionToken uid = newSessionToken' uid
  userTTL _ = userTokenTimeout . userTokenTimeoutSeconds
  zauthType = U

instance UserTokenLike LegalHoldUser where
  mkUserToken = mkLegalHoldUserToken
  userTokenOf = legalHoldUserTokenOf
  userTokenClient = legalHoldClientTokenOf
  mkSomeToken = Auth.LHUserToken
  userTokenRand = legalHoldUserTokenRand
  newUserToken = newLegalHoldUserToken
  newSessionToken _ _ = throwM ZV.Unsupported
  userTTL _ = legalHoldUserTokenTimeout . legalHoldUserTokenTimeoutSeconds
  zauthType = LU

mkUserToken' :: MonadZAuth m => UserId -> Maybe ClientId -> Word32 -> UTCTime -> m (Token User)
mkUserToken' u cid r t = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      ZC.newToken (utcTimeToPOSIXSeconds t) U Nothing (mkUser (toUUID u) (fmap Data.Id.client cid) r)

newUserToken' :: MonadZAuth m => UserId -> Maybe ClientId -> m (Token User)
newUserToken' u c = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let UserTokenTimeout ttl = z ^. settings . userTokenTimeout
       in ZC.userToken ttl (toUUID u) (fmap Data.Id.client c) r

newSessionToken' :: MonadZAuth m => UserId -> Maybe ClientId -> m (Token User)
newSessionToken' u c = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let SessionTokenTimeout ttl = z ^. settings . sessionTokenTimeout
       in ZC.sessionToken ttl (toUUID u) (fmap Data.Id.client c) r

newAccessToken' :: MonadZAuth m => Token User -> m (Token Access)
newAccessToken' xt = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let AccessTokenTimeout ttl = z ^. settings . accessTokenTimeout
       in ZC.accessToken1 ttl (xt ^. body . user) (xt ^. body . client)

renewAccessToken' :: MonadZAuth m => Maybe ClientId -> Token Access -> m (Token Access)
renewAccessToken' mcid old = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let AccessTokenTimeout ttl = z ^. settings . accessTokenTimeout
       in ZC.renewToken
            ttl
            (old ^. header)
            ( clientId
                .~ fmap Data.Id.client mcid
                $ (old ^. body)
            )

newBotToken :: MonadZAuth m => ProviderId -> BotId -> ConvId -> m (Token Bot)
newBotToken pid bid cid = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      ZC.botToken (toUUID pid) (toUUID (botUserId bid)) (toUUID cid)

newProviderToken :: MonadZAuth m => ProviderId -> m (Token Provider)
newProviderToken pid = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let ProviderTokenTimeout ttl = z ^. settings . providerTokenTimeout
       in ZC.providerToken ttl (toUUID pid)

-- FUTUREWORK: this function is very similar to mkUserToken',
-- the differences are
-- 1) LU / U
-- 2) (mkLegalHoldUser uid r) / (mkUser uid r)
-- Possibly some duplication could be removed.
-- See https://github.com/wireapp/wire-server/pull/761/files#r318612423
mkLegalHoldUserToken ::
  MonadZAuth m =>
  UserId ->
  Maybe ClientId ->
  Word32 ->
  UTCTime ->
  m (Token LegalHoldUser)
mkLegalHoldUserToken u c r t = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      ZC.newToken
        (utcTimeToPOSIXSeconds t)
        LU
        Nothing
        (mkLegalHoldUser (toUUID u) (fmap Data.Id.client c) r)

newLegalHoldUserToken :: MonadZAuth m => UserId -> Maybe ClientId -> m (Token LegalHoldUser)
newLegalHoldUserToken u c = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let LegalHoldUserTokenTimeout ttl = z ^. settings . legalHoldUserTokenTimeout
       in ZC.legalHoldUserToken ttl (toUUID u) (fmap Data.Id.client c) r

newLegalHoldAccessToken :: MonadZAuth m => Token LegalHoldUser -> m (Token LegalHoldAccess)
newLegalHoldAccessToken xt = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let LegalHoldAccessTokenTimeout ttl = z ^. settings . legalHoldAccessTokenTimeout
       in ZC.legalHoldAccessToken1
            ttl
            (xt ^. body . legalHoldUser . user)
            (xt ^. body . legalHoldUser . client)

renewLegalHoldAccessToken ::
  MonadZAuth m =>
  Maybe ClientId ->
  Token LegalHoldAccess ->
  m (Token LegalHoldAccess)
renewLegalHoldAccessToken _mcid old = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let LegalHoldAccessTokenTimeout ttl = z ^. settings . legalHoldAccessTokenTimeout
       in ZC.renewToken
            ttl
            (old ^. header)
            (old ^. body)

validateToken ::
  (MonadZAuth m, ToByteString a) =>
  Token a ->
  m (Either ZV.Failure ())
validateToken t = liftZAuth $ do
  z <- ask
  void <$> ZV.runValidate (z ^. public) (ZV.check t)

accessTokenOf' :: Token Access -> UserId
accessTokenOf' t = Id (t ^. body . userId)

accessTokenClient' :: Token Access -> Maybe ClientId
accessTokenClient' t = fmap ClientId (t ^. body . clientId)

userTokenOf' :: Token User -> UserId
userTokenOf' t = Id (t ^. body . user)

userTokenClient' :: Token User -> Maybe ClientId
userTokenClient' t = fmap ClientId (t ^. body . client)

legalHoldAccessTokenOf :: Token LegalHoldAccess -> UserId
legalHoldAccessTokenOf t = Id (t ^. body . legalHoldAccess . userId)

legalHoldAccessTokenClient :: Token LegalHoldAccess -> Maybe ClientId
legalHoldAccessTokenClient t = fmap ClientId (t ^. body . legalHoldAccess . clientId)

legalHoldUserTokenOf :: Token LegalHoldUser -> UserId
legalHoldUserTokenOf t = Id (t ^. body . legalHoldUser . user)

legalHoldClientTokenOf :: Token LegalHoldUser -> Maybe ClientId
legalHoldClientTokenOf t = fmap ClientId (t ^. body . legalHoldUser . client)

userTokenRand' :: Token User -> Word32
userTokenRand' t = t ^. body . rand

legalHoldUserTokenRand :: Token LegalHoldUser -> Word32
legalHoldUserTokenRand t = t ^. body . legalHoldUser . rand

tokenKeyIndex :: Token a -> Int
tokenKeyIndex t = t ^. header . key

tokenExpires :: Token a -> POSIXTime
tokenExpires t = fromIntegral (t ^. header . time)

tokenExpiresUTC :: Token a -> UTCTime
tokenExpiresUTC = posixSecondsToUTCTime . tokenExpires

randomValue :: IO Word32
randomValue = BS.foldl' f 0 <$> randBytes 4
  where
    f r w = shiftL r 8 .|. fromIntegral w
