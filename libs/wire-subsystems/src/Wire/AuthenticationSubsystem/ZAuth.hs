module Wire.AuthenticationSubsystem.ZAuth where

import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Conversion
import Data.Id
import Data.Text.Encoding qualified as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.ZAuth.Creation (TokenExpiry (..))
import Data.ZAuth.Creation qualified as ZC
import Data.ZAuth.CryptoSign
import Data.ZAuth.Token
import Data.ZAuth.Validation qualified as ZV
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.User.Auth (bearerToken)
import Wire.API.User.Auth qualified as Auth
import Wire.AuthenticationSubsystem.Config
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random

class (Body t ~ Access, SerializableToken t) => AccessTokenLike t where
  renewAccessToken :: (Member CryptoSign r, Member Now r, Member (Input AuthenticationSubsystemConfig) r) => Maybe ClientId -> Token t -> Sem r Auth.AccessToken
  accessTTL :: ZAuthSettings -> Integer

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
  userTTL :: ZAuthSettings -> Integer

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

mkUserToken :: (Member CryptoSign r, Member Now r, Member (Input AuthenticationSubsystemConfig) r, SerializableToken t, Body t ~ User) => UserId -> Maybe ClientId -> Word32 -> UTCTime -> Sem r (Token t)
mkUserToken u cid r t = do
  z <- inputs (.zauthEnv)
  ZC.newToken z.private (TokenExpiresAt (utcTimeToPOSIXSeconds t)) Nothing (User (toUUID u) (fmap clientToText cid) r)

newUserToken :: forall t r. (Member CryptoSign r, Member Now r, Member (Input AuthenticationSubsystemConfig) r, UserTokenLike t, Member Random r) => UserId -> Maybe ClientId -> Sem r (Token t)
newUserToken u c = do
  z <- inputs (.zauthEnv)
  r <- randomValue
  let ttl = userTTL @t z.settings
   in ZC.newToken z.private (TokenExpiresAfter ttl) Nothing $ User (toUUID u) (fmap clientToText c) r

newSessionToken :: forall t r. (Member CryptoSign r, Member Now r, Member (Input AuthenticationSubsystemConfig) r, UserTokenLike t, Member Random r) => UserId -> Maybe ClientId -> Sem r (Either ZV.Failure (Token t))
newSessionToken u c = runError $ do
  unless (allowSessionToken @t) $
    throw ZV.Unsupported
  z <- inputs (.zauthEnv)
  r <- randomValue
  let SessionTokenTimeout ttl = z.settings.sessionTokenTimeout
   in ZC.newToken z.private (TokenExpiresAfter ttl) (Just S) $ User (toUUID u) (fmap clientToText c) r

newAccessToken ::
  forall t r.
  (Member CryptoSign r, Member Now r, Member (Input AuthenticationSubsystemConfig) r, AccessTokenLike (AccessTokenType t), Body t ~ User, Member Random r) =>
  Token t ->
  Sem r Auth.AccessToken
newAccessToken xt = do
  z <- inputs (.zauthEnv)
  let ttl = accessTTL @(AccessTokenType t) z.settings
  connId <- randomConnId
  accessToken :: Token (AccessTokenType t) <-
    ZC.newToken z.private (TokenExpiresAfter ttl) Nothing $
      Access xt.body.user xt.body.client connId
  pure $
    bearerToken
      (Id accessToken.body.userId)
      (toByteString accessToken)
      ttl

renewAccessToken' :: (Member CryptoSign r, Member Now r, Member (Input AuthenticationSubsystemConfig) r) => Maybe ClientId -> Token A -> Sem r Auth.AccessToken
renewAccessToken' mcid old = do
  z <- inputs (.zauthEnv)
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

newBotToken :: (Member CryptoSign r, Member Now r, Member (Input AuthenticationSubsystemConfig) r) => ProviderId -> BotId -> ConvId -> Sem r (Token B)
newBotToken pid bid cid = do
  z <- inputs (.zauthEnv)
  ZC.newToken z.private TokenNeverExpires Nothing $
    Bot (toUUID pid) (toUUID (botUserId bid)) (toUUID cid)

newProviderToken :: (Member CryptoSign r, Member Now r, Member (Input AuthenticationSubsystemConfig) r) => ProviderId -> Sem r (Token P)
newProviderToken pid = do
  z <- inputs (.zauthEnv)
  let ProviderTokenTimeout ttl = z.settings.providerTokenTimeout
   in ZC.newToken z.private (TokenExpiresAfter ttl) Nothing $ Provider (toUUID pid)

renewLegalHoldAccessToken ::
  (Member CryptoSign r, Member Now r, Member (Input AuthenticationSubsystemConfig) r) =>
  Maybe ClientId ->
  Token LA ->
  Sem r Auth.AccessToken
renewLegalHoldAccessToken _mcid old = do
  z <- inputs (.zauthEnv)
  let LegalHoldAccessTokenTimeout ttl = z.settings.legalHoldAccessTokenTimeout
  new <- ZC.renewToken z.private ttl (old.header) (old.body)
  pure $
    bearerToken
      (Id new.body.userId)
      (toByteString new)
      ttl

validateToken ::
  (Member CryptoSign r, Member Now r, Member (Input AuthenticationSubsystemConfig) r, SerializableToken t) =>
  Token t ->
  Sem r (Either ZV.Failure ())
validateToken t = do
  z <- inputs (.zauthEnv)
  ZV.check z.publicKeys t

userTokenClient :: User -> Maybe ClientId
userTokenClient u = fromByteString . T.encodeUtf8 =<< u.client

tokenExpires :: Token a -> POSIXTime
tokenExpires t = fromIntegral (t.header.time)

tokenExpiresUTC :: Token a -> UTCTime
tokenExpiresUTC = posixSecondsToUTCTime . tokenExpires

randomValue :: (Member Random r) => Sem r Word32
randomValue = BS.foldl' f 0 <$> Random.bytes 4
  where
    f r w = shiftL r 8 .|. fromIntegral w

randomConnId :: (Member Random r) => Sem r Word64
randomConnId = BS.foldl' f 0 <$> Random.bytes 8
  where
    f r w = shiftL r 8 .|. fromIntegral w
