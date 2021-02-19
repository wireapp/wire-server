{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
module Brig.Types.User.ZAuth
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
    userTokenOf,
    legalHoldAccessTokenOf,
    legalHoldUserTokenOf,
    userTokenRand,
    tokenExpires,
    tokenExpiresUTC,
    tokenKeyIndex,
    zauthType,

    -- * cookies
    cookies,

    -- * Re-exports
    SecretKey,
    PublicKey,
    -- | For "/i/check-cookie"
    ValidateTokenRequest (..),
    ValidateTokenResponse (..),
  )
where

import Control.Lens (Lens', makeLenses, over, (^.))
import Control.Monad.Catch
import Data.Aeson
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import Data.Id
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List1 (List1)
import qualified Data.List1 as List1
import Data.Misc (simpleFromJSONWrapper, simpleToJSONWrapper)
import Data.Proxy
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.ZAuth.Creation as ZC
import Data.ZAuth.Token
import qualified Data.ZAuth.Validation as ZV
import Imports
import qualified Network.Wai.Predicate as P
import qualified Network.Wai.Predicate.Request as R
import OpenSSL.Random
import Sodium.Crypto.Sign
import Wire.API.Arbitrary (Arbitrary)

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
  return $! Env zc zv sets

class (UserTokenLike u, AccessTokenLike a) => TokenPair u a where
  newAccessToken :: MonadZAuth m => Token u -> m (Token a)

instance TokenPair User Access where
  newAccessToken = newAccessToken'

instance TokenPair LegalHoldUser LegalHoldAccess where
  newAccessToken = newLegalHoldAccessToken

class (FromByteString (Token a), ToByteString a) => AccessTokenLike a where
  accessTokenOf :: Token a -> UserId
  renewAccessToken :: MonadZAuth m => Token a -> m (Token a)
  settingsTTL :: Proxy a -> Lens' Settings Integer

instance AccessTokenLike Access where
  accessTokenOf = accessTokenOf'
  renewAccessToken = renewAccessToken'
  settingsTTL _ = accessTokenTimeout . accessTokenTimeoutSeconds

instance AccessTokenLike LegalHoldAccess where
  accessTokenOf = legalHoldAccessTokenOf
  renewAccessToken = renewLegalHoldAccessToken
  settingsTTL _ = legalHoldAccessTokenTimeout . legalHoldAccessTokenTimeoutSeconds

class (FromByteString (Token u), ToByteString u) => UserTokenLike u where
  userTokenOf :: Token u -> UserId
  mkUserToken :: MonadZAuth m => UserId -> Word32 -> UTCTime -> m (Token u)
  userTokenRand :: Token u -> Word32
  newUserToken :: MonadZAuth m => UserId -> m (Token u)
  newSessionToken :: (MonadThrow m, MonadZAuth m) => UserId -> m (Token u)
  userTTL :: Proxy u -> Lens' Settings Integer
  zauthType :: Type -- see libs/zauth/src/Token.hs

instance UserTokenLike User where
  mkUserToken = mkUserToken'
  userTokenOf = userTokenOf'
  userTokenRand = userTokenRand'
  newUserToken = newUserToken'
  newSessionToken uid = newSessionToken' uid
  userTTL _ = userTokenTimeout . userTokenTimeoutSeconds
  zauthType = U

instance UserTokenLike LegalHoldUser where
  mkUserToken = mkLegalHoldUserToken
  userTokenOf = legalHoldUserTokenOf
  userTokenRand = legalHoldUserTokenRand
  newUserToken = newLegalHoldUserToken
  newSessionToken _ = throwM ZV.Unsupported
  userTTL _ = legalHoldUserTokenTimeout . legalHoldUserTokenTimeoutSeconds
  zauthType = LU

mkUserToken' :: MonadZAuth m => UserId -> Word32 -> UTCTime -> m (Token User)
mkUserToken' u r t = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      ZC.newToken (utcTimeToPOSIXSeconds t) U Nothing (mkUser (toUUID u) r)

newUserToken' :: MonadZAuth m => UserId -> m (Token User)
newUserToken' u = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let UserTokenTimeout ttl = z ^. settings . userTokenTimeout
       in ZC.userToken ttl (toUUID u) r

newSessionToken' :: MonadZAuth m => UserId -> m (Token User)
newSessionToken' u = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let SessionTokenTimeout ttl = z ^. settings . sessionTokenTimeout
       in ZC.sessionToken ttl (toUUID u) r

newAccessToken' :: MonadZAuth m => Token User -> m (Token Access)
newAccessToken' xt = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let AccessTokenTimeout ttl = z ^. settings . accessTokenTimeout
       in ZC.accessToken1 ttl (xt ^. body . user)

renewAccessToken' :: MonadZAuth m => Token Access -> m (Token Access)
renewAccessToken' old = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let AccessTokenTimeout ttl = z ^. settings . accessTokenTimeout
       in ZC.renewToken ttl old

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
mkLegalHoldUserToken :: MonadZAuth m => UserId -> Word32 -> UTCTime -> m (Token LegalHoldUser)
mkLegalHoldUserToken u r t = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      ZC.newToken (utcTimeToPOSIXSeconds t) LU Nothing (mkLegalHoldUser (toUUID u) r)

newLegalHoldUserToken :: MonadZAuth m => UserId -> m (Token LegalHoldUser)
newLegalHoldUserToken u = liftZAuth $ do
  z <- ask
  r <- liftIO randomValue
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let LegalHoldUserTokenTimeout ttl = z ^. settings . legalHoldUserTokenTimeout
       in ZC.legalHoldUserToken ttl (toUUID u) r

newLegalHoldAccessToken :: MonadZAuth m => Token LegalHoldUser -> m (Token LegalHoldAccess)
newLegalHoldAccessToken xt = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let LegalHoldAccessTokenTimeout ttl = z ^. settings . legalHoldAccessTokenTimeout
       in ZC.legalHoldAccessToken1 ttl (xt ^. body . legalHoldUser . user)

renewLegalHoldAccessToken :: MonadZAuth m => Token LegalHoldAccess -> m (Token LegalHoldAccess)
renewLegalHoldAccessToken old = liftZAuth $ do
  z <- ask
  liftIO $
    ZC.runCreate (z ^. private) (z ^. settings . keyIndex) $
      let LegalHoldAccessTokenTimeout ttl = z ^. settings . legalHoldAccessTokenTimeout
       in ZC.renewToken ttl old

validateToken ::
  (MonadZAuth m, ToByteString a) =>
  Token a ->
  m (Either ZV.Failure ())
validateToken t = liftZAuth $ do
  z <- ask
  void <$> ZV.runValidate (z ^. public) (ZV.check t)

-- | for "/i/check-cookie"
newtype ValidateTokenRequest = ValidateTokenRequest {fromValidateTokenRequest :: Token User}
  deriving (Eq, Show, Generic)

instance ToJSON ValidateTokenRequest where
  toJSON = simpleToJSONWrapper "cookie" . fromValidateTokenRequest

instance FromJSON ValidateTokenRequest where
  parseJSON = fmap ValidateTokenRequest . simpleFromJSONWrapper "cookie"

newtype ValidateTokenResponse = ValidateTokenResponse {fromValidateTokenResponse :: UserId}
  deriving (Eq, Show, Generic, Arbitrary)

instance ToJSON ValidateTokenResponse where
  toJSON = simpleToJSONWrapper "user" . fromValidateTokenResponse

instance FromJSON ValidateTokenResponse where
  parseJSON = fmap ValidateTokenResponse . simpleFromJSONWrapper "user"

accessTokenOf' :: Token Access -> UserId
accessTokenOf' t = Id (t ^. body . userId)

userTokenOf' :: Token User -> UserId
userTokenOf' t = Id (t ^. body . user)

legalHoldAccessTokenOf :: Token LegalHoldAccess -> UserId
legalHoldAccessTokenOf t = Id (t ^. body . legalHoldAccess . userId)

legalHoldUserTokenOf :: Token LegalHoldUser -> UserId
legalHoldUserTokenOf t = Id (t ^. body . legalHoldUser . user)

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

-- | Internal utilities: These functions are nearly copies verbatim from the original
-- project: https://gitlab.com/twittner/wai-predicates/-/blob/develop/src/Network/Wai/Predicate.hs#L106-112
-- Main difference: the original stops after finding the first valid cookie which
-- is a problem if clients send more than 1 cookie and one of them happens to be invalid
-- We should also be dropping this in favor of servant which will make this redundant.
cookies :: (R.HasCookies r, FromByteString a) => ByteString -> P.Predicate r P.Error (List1 a)
cookies k r =
  case R.lookupCookie k r of
    [] -> P.Fail . P.addLabel "cookie" $ notAvailable k
    cc ->
      case mapMaybe fromByteString cc of
        [] -> (P.Fail . P.addLabel "cookie" . typeError k $ "Failed to get zuid cookies")
        (x : xs) -> return $ List1.list1 x xs

notAvailable :: ByteString -> P.Error
notAvailable k = P.e400 & P.setReason P.NotAvailable . P.setSource k
{-# INLINE notAvailable #-}

typeError :: ByteString -> ByteString -> P.Error
typeError k m = P.e400 & P.setReason P.TypeError . P.setSource k . P.setMessage m
{-# INLINE typeError #-}
