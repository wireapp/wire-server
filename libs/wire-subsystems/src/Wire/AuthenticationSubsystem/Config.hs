module Wire.AuthenticationSubsystem.Config where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Qualified
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.ZAuth.Creation qualified as ZC
import Imports
import Sodium.Crypto.Sign
import Wire.API.Allowlists (AllowlistEmailDomains)
import Wire.AuthenticationSubsystem.Cookie.Limit

data AuthenticationSubsystemConfig = AuthenticationSubsystemConfig
  { local :: Local (),
    allowlistEmailDomains :: Maybe AllowlistEmailDomains,
    zauthEnv :: ZAuthEnv,
    userCookieRenewAge :: Integer,
    userCookieLimit :: Int,
    userCookieThrottle :: CookieThrottle
  }

data ZAuthSettings = ZAuthSettings
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

defSettings :: ZAuthSettings
defSettings =
  ZAuthSettings
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
    settings :: !ZAuthSettings
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

instance FromJSON ZAuthSettings where
  parseJSON = withObject "ZAuth.Settings" $ \o ->
    ZAuthSettings
      <$> o .: "keyIndex"
      <*> (UserTokenTimeout <$> o .: "userTokenTimeout")
      <*> (SessionTokenTimeout <$> o .: "sessionTokenTimeout")
      <*> (AccessTokenTimeout <$> o .: "accessTokenTimeout")
      <*> (ProviderTokenTimeout <$> o .: "providerTokenTimeout")
      <*> (LegalHoldUserTokenTimeout <$> o .: "legalHoldUserTokenTimeout")
      <*> (LegalHoldAccessTokenTimeout <$> o .: "legalHoldAccessTokenTimeout")

readKeys :: (Read k) => FilePath -> IO (Maybe (NonEmpty k))
readKeys fp = nonEmpty . map read . filter (not . null) . lines <$> readFile fp

mkZAuthEnv :: NonEmpty SecretKey -> NonEmpty PublicKey -> ZAuthSettings -> IO ZAuthEnv
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
