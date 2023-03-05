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

-- | High-level user authentication and access control.
module Brig.User.Auth
  ( Access,
    sendLoginCode,
    login,
    logout,
    renewAccess,
    validateTokens,
    revokeAccess,
    verifyCode,

    -- * Internal
    lookupLoginCode,
    ssoLogin,
    legalHoldLogin,

    -- * Re-exports
    listCookies,
  )
where

import Bilge.IO
import Bilge.RPC
import Brig.API.Types
import Brig.API.User (changeSingleAccountStatus)
import Brig.App
import Brig.Budget
import qualified Brig.Code as Code
import qualified Brig.Data.Activation as Data
import Brig.Data.Client
import qualified Brig.Data.LoginCode as Data
import qualified Brig.Data.User as Data
import Brig.Data.UserKey
import qualified Brig.Data.UserKey as Data
import Brig.Effects.GalleyProvider (GalleyProvider)
import qualified Brig.Effects.GalleyProvider as GalleyProvider
import Brig.Email
import qualified Brig.Options as Opt
import Brig.Phone
import Brig.Types.Intra
import Brig.User.Auth.Cookie
import Brig.User.Handle
import Brig.User.Phone
import Brig.User.Search.Index
import qualified Brig.ZAuth as ZAuth
import Cassandra
import Control.Error hiding (bool)
import Control.Lens (to, view)
import Control.Monad.Catch
import Control.Monad.Except
import Data.ByteString.Conversion (toByteString)
import Data.Handle (Handle)
import Data.Id
import qualified Data.List.NonEmpty as NE
import Data.List1 (List1)
import qualified Data.List1 as List1
import Data.Misc (PlainTextPassword (..))
import qualified Data.ZAuth.Token as ZAuth
import Imports
import Network.Wai.Utilities.Error ((!>>))
import Polysemy
import System.Logger (field, msg, val, (~~))
import qualified System.Logger.Class as Log
import Wire.API.Team.Feature
import qualified Wire.API.Team.Feature as Public
import Wire.API.User
import Wire.API.User.Auth
import Wire.API.User.Auth.LegalHold
import Wire.API.User.Auth.Sso

sendLoginCode ::
  ( MonadClient m,
    MonadReader Env m,
    MonadCatch m,
    Log.MonadLogger m
  ) =>
  Phone ->
  Bool ->
  Bool ->
  ExceptT SendLoginCodeError m PendingLoginCode
sendLoginCode phone call force = do
  pk <-
    maybe
      (throwE $ SendLoginInvalidPhone phone)
      (pure . userPhoneKey)
      =<< lift (validatePhone phone)
  user <- lift $ Data.lookupKey pk
  case user of
    Nothing -> throwE $ SendLoginInvalidPhone phone
    Just u -> do
      lift . Log.debug $ field "user" (toByteString u) . field "action" (Log.val "User.sendLoginCode")
      pw <- lift $ Data.lookupPassword u
      unless (isNothing pw || force) $
        throwE SendLoginPasswordExists
      lift $ do
        l <- Data.lookupLocale u
        c <- Data.createLoginCode u
        void . forPhoneKey pk $ \ph ->
          if call
            then sendLoginCall ph (pendingLoginCode c) l
            else sendLoginSms ph (pendingLoginCode c) l
        pure c

lookupLoginCode ::
  ( MonadClient m,
    Log.MonadLogger m,
    MonadReader Env m
  ) =>
  Phone ->
  m (Maybe PendingLoginCode)
lookupLoginCode phone =
  Data.lookupKey (userPhoneKey phone) >>= \case
    Nothing -> pure Nothing
    Just u -> do
      Log.debug $ field "user" (toByteString u) . field "action" (Log.val "User.lookupLoginCode")
      Data.lookupLoginCode u

login ::
  forall r.
  (Member GalleyProvider r) =>
  Login ->
  CookieType ->
  ExceptT LoginError (AppT r) (Access ZAuth.User)
login (PasswordLogin (PasswordLoginData li pw label code)) typ = do
  uid <- wrapHttpClientE $ resolveLoginId li
  lift . Log.debug $ field "user" (toByteString uid) . field "action" (Log.val "User.login")
  wrapHttpClientE $ checkRetryLimit uid
  wrapHttpClientE $
    Data.authenticate uid pw `catchE` \case
      AuthInvalidUser -> loginFailed uid
      AuthInvalidCredentials -> loginFailed uid
      AuthSuspended -> throwE LoginSuspended
      AuthEphemeral -> throwE LoginEphemeral
      AuthPendingInvitation -> throwE LoginPendingActivation
  verifyLoginCode code uid
  wrapHttpClientE $ newAccess @ZAuth.User @ZAuth.Access uid Nothing typ label
  where
    verifyLoginCode :: Maybe Code.Value -> UserId -> ExceptT LoginError (AppT r) ()
    verifyLoginCode mbCode uid =
      verifyCode mbCode Login uid
        `catchE` \case
          VerificationCodeNoPendingCode -> wrapHttpClientE $ loginFailedWith LoginCodeInvalid uid
          VerificationCodeRequired -> wrapHttpClientE $ loginFailedWith LoginCodeRequired uid
          VerificationCodeNoEmail -> wrapHttpClientE $ loginFailed uid
login (SmsLogin (SmsLoginData phone code label)) typ = do
  uid <- wrapHttpClientE $ resolveLoginId (LoginByPhone phone)
  lift . Log.debug $ field "user" (toByteString uid) . field "action" (Log.val "User.login")
  wrapHttpClientE $ checkRetryLimit uid
  ok <- wrapHttpClientE $ Data.verifyLoginCode uid code
  unless ok $
    wrapHttpClientE $
      loginFailed uid
  wrapHttpClientE $ newAccess @ZAuth.User @ZAuth.Access uid Nothing typ label

verifyCode ::
  forall r.
  Member GalleyProvider r =>
  Maybe Code.Value ->
  VerificationAction ->
  UserId ->
  ExceptT VerificationCodeError (AppT r) ()
verifyCode mbCode action uid = do
  (mbEmail, mbTeamId) <- getEmailAndTeamId uid
  featureEnabled <- lift $ do
    mbFeatureEnabled <- liftSem $ GalleyProvider.getVerificationCodeEnabled `traverse` mbTeamId
    pure $ fromMaybe (Public.wsStatus (Public.defFeatureStatus @Public.SndFactorPasswordChallengeConfig) == Public.FeatureStatusEnabled) mbFeatureEnabled
  isSsoUser <- wrapHttpClientE $ Data.isSamlUser uid
  when (featureEnabled && not isSsoUser) $ do
    case (mbCode, mbEmail) of
      (Just code, Just email) -> do
        key <- Code.mkKey $ Code.ForEmail email
        codeValid <- isJust <$> wrapHttpClientE (Code.verify key (Code.scopeFromAction action) code)
        unless codeValid $ throwE VerificationCodeNoPendingCode
      (Nothing, _) -> throwE VerificationCodeRequired
      (_, Nothing) -> throwE VerificationCodeNoEmail
  where
    getEmailAndTeamId ::
      UserId ->
      ExceptT e (AppT r) (Maybe Email, Maybe TeamId)
    getEmailAndTeamId u = do
      mbAccount <- wrapHttpClientE $ Data.lookupAccount u
      pure (userEmail <$> accountUser =<< mbAccount, userTeam <$> accountUser =<< mbAccount)

loginFailedWith :: (MonadClient m, MonadReader Env m) => LoginError -> UserId -> ExceptT LoginError m ()
loginFailedWith e uid = decrRetryLimit uid >> throwE e

loginFailed :: (MonadClient m, MonadReader Env m) => UserId -> ExceptT LoginError m ()
loginFailed = loginFailedWith LoginFailed

decrRetryLimit :: (MonadClient m, MonadReader Env m) => UserId -> ExceptT LoginError m ()
decrRetryLimit = withRetryLimit (\k b -> withBudget k b $ pure ())

checkRetryLimit :: (MonadClient m, MonadReader Env m) => UserId -> ExceptT LoginError m ()
checkRetryLimit = withRetryLimit checkBudget

withRetryLimit ::
  MonadReader Env m =>
  (BudgetKey -> Budget -> ExceptT LoginError m (Budgeted ())) ->
  UserId ->
  ExceptT LoginError m ()
withRetryLimit action uid = do
  mLimitFailedLogins <- view (settings . to Opt.setLimitFailedLogins)
  forM_ mLimitFailedLogins $ \opts -> do
    let bkey = BudgetKey ("login#" <> idToText uid)
        budget =
          Budget
            (Opt.timeoutDiff $ Opt.timeout opts)
            (fromIntegral $ Opt.retryLimit opts)
    bresult <- action bkey budget
    case bresult of
      BudgetExhausted ttl -> throwE . LoginBlocked . RetryAfter . floor $ ttl
      BudgetedValue () _ -> pure ()

logout ::
  ( ZAuth.TokenPair u a,
    ZAuth.MonadZAuth m,
    MonadClient m
  ) =>
  List1 (ZAuth.Token u) ->
  ZAuth.Token a ->
  ExceptT ZAuth.Failure m ()
logout uts at = do
  (u, ck) <- validateTokens uts (Just at)
  lift $ revokeCookies u [cookieId ck] []

renewAccess ::
  forall m u a.
  ( ZAuth.TokenPair u a,
    MonadClient m,
    ZAuth.MonadZAuth m,
    Log.MonadLogger m,
    MonadReader Env m,
    MonadIndexIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadUnliftIO m
  ) =>
  List1 (ZAuth.Token u) ->
  Maybe (ZAuth.Token a) ->
  Maybe ClientId ->
  ExceptT ZAuth.Failure m (Access u)
renewAccess uts at mcid = do
  (uid, ck) <- validateTokens uts at
  traverse_ (checkClientId uid) mcid
  lift . Log.debug $ field "user" (toByteString uid) . field "action" (Log.val "User.renewAccess")
  catchSuspendInactiveUser uid ZAuth.Expired
  ck' <- nextCookie ck mcid
  at' <- lift $ newAccessToken (fromMaybe ck ck') at
  pure $ Access at' ck'

revokeAccess ::
  (MonadClient m, Log.MonadLogger m, MonadReader Env m) =>
  UserId ->
  PlainTextPassword ->
  [CookieId] ->
  [CookieLabel] ->
  ExceptT AuthError m ()
revokeAccess u pw cc ll = do
  lift $ Log.debug $ field "user" (toByteString u) . field "action" (Log.val "User.revokeAccess")
  unlessM (Data.isSamlUser u) $ Data.authenticate u pw
  lift $ revokeCookies u cc ll

--------------------------------------------------------------------------------
-- Internal

catchSuspendInactiveUser ::
  ( MonadClient m,
    MonadIndexIO m,
    MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadUnliftIO m,
    Log.MonadLogger m
  ) =>
  UserId ->
  e ->
  ExceptT e m ()
catchSuspendInactiveUser uid errval = do
  mustsuspend <- lift $ mustSuspendInactiveUser uid
  when mustsuspend $ do
    lift . Log.warn $
      msg (val "Suspending user due to inactivity")
        ~~ field "user" (toByteString uid)
        ~~ field "action" ("user.suspend" :: String)
    lift $ runExceptT (changeSingleAccountStatus uid Suspended) >>= explicitlyIgnoreErrors
    throwE errval
  where
    explicitlyIgnoreErrors :: Monad m => Either AccountStatusError () -> m ()
    explicitlyIgnoreErrors = \case
      Left InvalidAccountStatus -> pure ()
      Left AccountNotFound -> pure ()
      Right () -> pure ()

newAccess ::
  forall u a m.
  ( ZAuth.TokenPair u a,
    MonadReader Env m,
    MonadClient m,
    ZAuth.MonadZAuth m,
    Log.MonadLogger m,
    MonadIndexIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadUnliftIO m
  ) =>
  UserId ->
  Maybe ClientId ->
  CookieType ->
  Maybe CookieLabel ->
  ExceptT LoginError m (Access u)
newAccess uid cid ct cl = do
  catchSuspendInactiveUser uid LoginSuspended
  r <- lift $ newCookieLimited uid cid ct cl
  case r of
    Left delay -> throwE $ LoginThrottled delay
    Right ck -> do
      t <- lift $ newAccessToken @u @a ck Nothing
      pure $ Access t (Just ck)

resolveLoginId :: (MonadClient m, MonadReader Env m) => LoginId -> ExceptT LoginError m UserId
resolveLoginId li = do
  usr <- validateLoginId li >>= lift . either lookupKey lookupHandle
  case usr of
    Nothing -> do
      pending <- lift $ isPendingActivation li
      throwE $
        if pending
          then LoginPendingActivation
          else LoginFailed
    Just uid -> pure uid

validateLoginId :: (MonadClient m, MonadReader Env m) => LoginId -> ExceptT LoginError m (Either UserKey Handle)
validateLoginId (LoginByEmail email) =
  either
    (const $ throwE LoginFailed)
    (pure . Left . userEmailKey)
    (validateEmail email)
validateLoginId (LoginByPhone phone) =
  maybe
    (throwE LoginFailed)
    (pure . Left . userPhoneKey)
    =<< lift (validatePhone phone)
validateLoginId (LoginByHandle h) =
  pure (Right h)

isPendingActivation :: (MonadClient m, MonadReader Env m) => LoginId -> m Bool
isPendingActivation ident = case ident of
  (LoginByHandle _) -> pure False
  (LoginByEmail e) -> checkKey (userEmailKey e)
  (LoginByPhone p) -> checkKey (userPhoneKey p)
  where
    checkKey k = do
      usr <- (>>= fst) <$> Data.lookupActivationCode k
      case usr of
        Nothing -> pure False
        Just u -> maybe False (checkAccount k) <$> Data.lookupAccount u
    checkAccount k a =
      let i = userIdentity (accountUser a)
          statusAdmitsPending = case accountStatus a of
            Active -> True
            Suspended -> False
            Deleted -> False
            Ephemeral -> False
            PendingInvitation -> True
       in statusAdmitsPending && case i of
            Just (EmailIdentity e) -> userEmailKey e /= k
            Just (PhoneIdentity p) -> userPhoneKey p /= k
            Just (FullIdentity e p) -> userEmailKey e /= k && userPhoneKey p /= k
            Just SSOIdentity {} -> False -- sso-created users are activated immediately.
            Nothing -> True

-- | Validate a list of (User/LH) tokens potentially with an associated access token.
--   If there are multiple valid cookies, we try all of them. When an access token is
--   given, we perform the usual checks.
--   If multiple cookies are given and several are valid, we return the first valid one.
validateTokens ::
  (ZAuth.TokenPair u a, ZAuth.MonadZAuth m, MonadClient m) =>
  List1 (ZAuth.Token u) ->
  Maybe (ZAuth.Token a) ->
  ExceptT ZAuth.Failure m (UserId, Cookie (ZAuth.Token u))
validateTokens uts at = do
  tokens <- forM uts $ \ut -> lift $ runExceptT (validateToken ut at)
  getFirstSuccessOrFirstFail tokens
  where
    -- FUTUREWORK: There is surely a better way to do this
    getFirstSuccessOrFirstFail ::
      Monad m =>
      List1 (Either ZAuth.Failure (UserId, Cookie (ZAuth.Token u))) ->
      ExceptT ZAuth.Failure m (UserId, Cookie (ZAuth.Token u))
    getFirstSuccessOrFirstFail tks = case (lefts $ NE.toList $ List1.toNonEmpty tks, rights $ NE.toList $ List1.toNonEmpty tks) of
      (_, suc : _) -> pure suc
      (e : _, _) -> throwE e
      _ -> throwE ZAuth.Invalid -- Impossible

validateToken ::
  ( ZAuth.TokenPair u a,
    ZAuth.MonadZAuth m,
    MonadClient m
  ) =>
  ZAuth.Token u ->
  Maybe (ZAuth.Token a) ->
  ExceptT ZAuth.Failure m (UserId, Cookie (ZAuth.Token u))
validateToken ut at = do
  unless (maybe True ((ZAuth.userTokenOf ut ==) . ZAuth.accessTokenOf) at) $
    throwE ZAuth.Invalid
  ExceptT (ZAuth.validateToken ut)
  forM_ at $ \token ->
    ExceptT (ZAuth.validateToken token)
      `catchE` \e ->
        unless (e == ZAuth.Expired) (throwE e)
  ck <- lift (lookupCookie ut) >>= maybe (throwE ZAuth.Invalid) pure
  pure (ZAuth.userTokenOf ut, ck)

-- | Allow to login as any user without having the credentials.
ssoLogin ::
  ( MonadClient m,
    MonadReader Env m,
    ZAuth.MonadZAuth m,
    Log.MonadLogger m,
    MonadIndexIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadUnliftIO m
  ) =>
  SsoLogin ->
  CookieType ->
  ExceptT LoginError m (Access ZAuth.User)
ssoLogin (SsoLogin uid label) typ = do
  Data.reauthenticate uid Nothing `catchE` \case
    ReAuthMissingPassword -> pure ()
    ReAuthCodeVerificationRequired -> pure ()
    ReAuthCodeVerificationNoPendingCode -> pure ()
    ReAuthCodeVerificationNoEmail -> pure ()
    ReAuthError e -> case e of
      AuthInvalidUser -> throwE LoginFailed
      AuthInvalidCredentials -> pure ()
      AuthSuspended -> throwE LoginSuspended
      AuthEphemeral -> throwE LoginEphemeral
      AuthPendingInvitation -> throwE LoginPendingActivation
  newAccess @ZAuth.User @ZAuth.Access uid Nothing typ label

-- | Log in as a LegalHold service, getting LegalHoldUser/Access Tokens.
legalHoldLogin ::
  (Member GalleyProvider r) =>
  LegalHoldLogin ->
  CookieType ->
  ExceptT LegalHoldLoginError (AppT r) (Access ZAuth.LegalHoldUser)
legalHoldLogin (LegalHoldLogin uid plainTextPassword label) typ = do
  wrapHttpClientE (Data.reauthenticate uid plainTextPassword) !>> LegalHoldReAuthError
  -- legalhold login is only possible if
  -- the user is a team user
  -- and the team has legalhold enabled
  mteam <- lift $ liftSem $ GalleyProvider.getTeamId uid
  case mteam of
    Nothing -> throwE LegalHoldLoginNoBindingTeam
    Just tid -> assertLegalHoldEnabled tid
  -- create access token and cookie
  wrapHttpClientE (newAccess @ZAuth.LegalHoldUser @ZAuth.LegalHoldAccess uid Nothing typ label)
    !>> LegalHoldLoginError

assertLegalHoldEnabled ::
  Member GalleyProvider r =>
  TeamId ->
  ExceptT LegalHoldLoginError (AppT r) ()
assertLegalHoldEnabled tid = do
  stat <- lift $ liftSem $ GalleyProvider.getTeamLegalHoldStatus tid
  case wsStatus stat of
    FeatureStatusDisabled -> throwE LegalHoldLoginLegalHoldNotEnabled
    FeatureStatusEnabled -> pure ()

checkClientId :: MonadClient m => UserId -> ClientId -> ExceptT ZAuth.Failure m ()
checkClientId uid cid =
  lookupClient uid cid >>= maybe (throwE ZAuth.Invalid) (const (pure ()))
