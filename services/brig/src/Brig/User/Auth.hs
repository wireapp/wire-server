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
  ( Access (..),
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

import Brig.API.Types
import Brig.API.User (changeSingleAccountStatus)
import Brig.App
import qualified Brig.Code as Code
import qualified Brig.Data.Activation as Data
import qualified Brig.Data.LoginCode as Data
import qualified Brig.Data.User as Data
import Brig.Data.UserKey
import qualified Brig.Data.UserKey as Data
import Brig.Effects.BudgetStore
import Brig.Effects.Common
import Brig.Effects.GalleyAccess
import Brig.Effects.GundeckAccess
import Brig.Effects.Twilio (Twilio)
import Brig.Effects.UserHandleStore
import Brig.Effects.UserKeyStore (UserKeyStore)
import Brig.Effects.UserQuery (UserQuery)
import Brig.Effects.UserQuery.Cassandra
import Brig.Effects.VerificationCodeStore (VerificationCodeStore)
import Brig.Email
import qualified Brig.IO.Intra as Intra
import qualified Brig.Options as Opt
import Brig.Phone
import Brig.Types.Common
import Brig.Types.Intra
import Brig.Types.User.Auth
import Brig.User.Auth.Cookie
import Brig.User.Phone
import qualified Brig.ZAuth as ZAuth
import Cassandra
import Control.Error hiding (bool)
import Control.Lens (to, view)
import Data.ByteString.Conversion (toByteString)
import Data.Either.Combinators
import Data.Handle (Handle)
import Data.Id
import qualified Data.List.NonEmpty as NE
import Data.List1 (List1)
import qualified Data.List1 as List1
import Data.Misc (PlainTextPassword (..))
import Data.Qualified
import qualified Data.ZAuth.Token as ZAuth
import Imports
import Network.Wai.Utilities.Error ((!>>))
import Polysemy
import Polysemy.Error
import qualified Polysemy.Error as P
import Polysemy.Input
import Polysemy.TinyLog
import qualified Polysemy.TinyLog as P
import qualified Ropes.Twilio as Twilio
import System.Logger (field, msg, val, (~~))
import qualified System.Logger.Class as Log
import Wire.API.Team.Feature
import qualified Wire.API.Team.Feature as Public
import Wire.API.User
import Wire.API.User.Auth

data Access u = Access
  { accessToken :: !AccessToken,
    accessCookie :: !(Maybe (Cookie (ZAuth.Token u)))
  }

sendLoginCode ::
  forall r.
  Members
    '[ Error Twilio.ErrorResponse,
       P.TinyLog,
       Twilio,
       UserKeyStore,
       UserQuery
     ]
    r =>
  Phone ->
  Bool ->
  Bool ->
  ExceptT SendLoginCodeError (AppT r) PendingLoginCode
sendLoginCode phone call force = do
  pk <-
    maybe
      (throwE $ SendLoginInvalidPhone phone)
      (pure . userPhoneKey)
      =<< lift (liftSem $ validatePhone phone)
  user <- lift . liftSem $ Data.getKey pk
  case user of
    Nothing -> throwE $ SendLoginInvalidPhone phone
    Just u -> do
      lift . liftSem
        . P.debug
        $ field "user" (toByteString u) . field "action" (Log.val "User.sendLoginCode")
      pw <- lift . wrapClient $ Data.lookupPassword u
      unless (isNothing pw || force) $
        throwE SendLoginPasswordExists
      lift $ do
        defLoc <- Opt.setDefaultUserLocale <$> view settings
        l <- liftSem $ Data.lookupLocale defLoc u
        c <- wrapClient $ Data.createLoginCode u
        void . forPhoneKey pk $ \ph ->
          wrapClient $
            if call
              then sendLoginCall ph (pendingLoginCode c) l
              else sendLoginSms ph (pendingLoginCode c) l
        pure c

lookupLoginCode ::
  Members
    '[ P.TinyLog,
       UserKeyStore
     ]
    r =>
  Phone ->
  AppT r (Maybe PendingLoginCode)
lookupLoginCode phone =
  liftSem (Data.getKey (userPhoneKey phone)) >>= \case
    Nothing -> pure Nothing
    Just u -> do
      liftSem $ P.debug $ field "user" (toByteString u) . field "action" (Log.val "User.lookupLoginCode")
      wrapClient $ Data.lookupLoginCode u

login ::
  forall r.
  Members
    '[ BudgetStore,
       Error Twilio.ErrorResponse,
       GalleyAccess,
       GundeckAccess,
       Input (Local ()),
       P.TinyLog,
       Twilio,
       UserHandleStore,
       UserKeyStore,
       UserQuery,
       VerificationCodeStore
     ]
    r =>
  Login ->
  CookieType ->
  ExceptT LoginError (AppT r) (Access ZAuth.User)
login (PasswordLogin li pw label code) typ = do
  uid <- resolveLoginId li
  lift . liftSem . P.debug $ field "user" (toByteString uid) . field "action" (Log.val "User.login")
  mLimitFailedLogins <- view (settings . to Opt.setLimitFailedLogins)
  liftSemE . semErrToExceptT $ checkRetryLimit uid mLimitFailedLogins
  o <- lift . liftSem . runError @AuthError $ Data.authenticate uid pw
  whenLeft o $ \case
    AuthInvalidUser -> liftSemE . semErrToExceptT $ loginFailed uid mLimitFailedLogins
    AuthInvalidCredentials -> liftSemE . semErrToExceptT $ loginFailed uid mLimitFailedLogins
    AuthSuspended -> throwE LoginSuspended
    AuthEphemeral -> throwE LoginEphemeral
    AuthPendingInvitation -> throwE LoginPendingActivation
  verifyLoginCode code uid mLimitFailedLogins
  newAccess @ZAuth.User @ZAuth.Access uid typ label
  where
    verifyLoginCode ::
      Maybe Code.Value ->
      UserId ->
      Maybe Opt.LimitFailedLogins ->
      ExceptT LoginError (AppT r) ()
    verifyLoginCode mbCode uid mLimitFailedLogins =
      verifyCode mbCode Login uid
        `catchE` \case
          VerificationCodeNoPendingCode -> liftSemE . semErrToExceptT $ loginFailedWith LoginCodeInvalid uid mLimitFailedLogins
          VerificationCodeRequired -> liftSemE . semErrToExceptT $ loginFailedWith LoginCodeRequired uid mLimitFailedLogins
          VerificationCodeNoEmail -> liftSemE . semErrToExceptT $ loginFailed uid mLimitFailedLogins
login (SmsLogin phone code label) typ = do
  uid <- resolveLoginId (LoginByPhone phone)
  lift . Log.debug $ field "user" (toByteString uid) . field "action" (Log.val "User.login")
  mLimitFailedLogins <- view (settings . to Opt.setLimitFailedLogins)
  e <- lift . liftSem . runError $ checkRetryLimit uid mLimitFailedLogins
  whenLeft e throwE
  ok <- lift . wrapClient $ Data.verifyLoginCode uid code
  unless ok $ do
    r <- lift . liftSem . runError $ loginFailed uid mLimitFailedLogins
    whenLeft r throwE
  newAccess @ZAuth.User @ZAuth.Access uid typ label

verifyCode ::
  forall r.
  Members
    '[ GalleyAccess,
       Input (Local ()),
       UserQuery,
       VerificationCodeStore
     ]
    r =>
  Maybe Code.Value ->
  VerificationAction ->
  UserId ->
  ExceptT VerificationCodeError (AppT r) ()
verifyCode mbCode action uid = do
  (mbEmail, mbTeamId) <- getEmailAndTeamId uid
  featureEnabled <- lift $ do
    mbFeatureEnabled <- wrapHttpClient $ Intra.getVerificationCodeEnabled `traverse` mbTeamId
    -- pure $
    --   maybe
    --     (Public.tfwoapsStatus (Public.defTeamFeatureStatus @'Public.TeamFeatureSndFactorPasswordChallenge) == Public.TeamFeatureEnabled)
    --     (== TeamFeatureEnabled)
    --     mbFeatureEnabled
    pure $ fromMaybe (Public.wsStatus (Public.defFeatureStatus @Public.SndFactorPasswordChallengeConfig) == Public.FeatureStatusEnabled) mbFeatureEnabled
  locale <- Opt.setDefaultUserLocale <$> view settings
  isSsoUser <- lift . liftSem $ Data.isSamlUser locale uid
  when (featureEnabled && not isSsoUser) $ do
    case (mbCode, mbEmail) of
      (Just code, Just email) -> do
        key <- Code.mkKey $ Code.ForEmail email
        codeValid <- lift . liftSem $ isJust <$> Code.verifyCode key (Code.scopeFromAction action) code
        unless codeValid $ throwE VerificationCodeNoPendingCode
      (Nothing, _) -> throwE VerificationCodeRequired
      (_, Nothing) -> throwE VerificationCodeNoEmail
  where
    getEmailAndTeamId ::
      UserId ->
      ExceptT e (AppT r) (Maybe Email, Maybe TeamId)
    getEmailAndTeamId u = do
      locale <- Opt.setDefaultUserLocale <$> view settings
      mbAccount <- lift . liftSem $ Data.lookupAccount locale u
      pure (userEmail <$> accountUser =<< mbAccount, userTeam <$> accountUser =<< mbAccount)

loginFailedWith ::
  Member BudgetStore r =>
  LoginError ->
  UserId ->
  Maybe Opt.LimitFailedLogins ->
  Sem (Error LoginError ': r) ()
loginFailedWith e uid mLimitFailedLogins =
  decrRetryLimit uid mLimitFailedLogins >> throw e

loginFailed ::
  Member BudgetStore r =>
  UserId ->
  Maybe Opt.LimitFailedLogins ->
  Sem (Error LoginError ': r) ()
loginFailed = loginFailedWith LoginFailed

decrRetryLimit ::
  Member BudgetStore r =>
  UserId ->
  Maybe Opt.LimitFailedLogins ->
  Sem (Error LoginError ': r) ()
decrRetryLimit = withRetryLimit (\k b -> withBudget k b $ pure ())

checkRetryLimit ::
  Member BudgetStore r =>
  UserId ->
  Maybe Opt.LimitFailedLogins ->
  Sem (Error LoginError ': r) ()
checkRetryLimit = withRetryLimit checkBudget

withRetryLimit ::
  Member BudgetStore r =>
  (BudgetKey -> Budget -> Sem r (Budgeted ())) ->
  UserId ->
  Maybe Opt.LimitFailedLogins ->
  Sem (Error LoginError ': r) ()
withRetryLimit action uid mLimitFailedLogins = do
  forM_ mLimitFailedLogins $ \opts -> do
    let bkey = BudgetKey ("login#" <> idToText uid)
        budget =
          Budget
            (Opt.timeoutDiff $ Opt.timeout opts)
            (fromIntegral $ Opt.retryLimit opts)
    raise @(Error LoginError) (action bkey budget) >>= \case
      BudgetExhausted ttl -> throw . LoginBlocked . RetryAfter . floor $ ttl
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
  forall u a r.
  ZAuth.TokenPair u a =>
  Members
    '[ GalleyAccess,
       GundeckAccess,
       UserQuery
     ]
    r =>
  List1 (ZAuth.Token u) ->
  Maybe (ZAuth.Token a) ->
  ExceptT ZAuth.Failure (AppT r) (Access u)
renewAccess uts at = do
  (uid, ck) <- wrapHttpClientE $ validateTokens uts at
  lift . Log.debug $ field "user" (toByteString uid) . field "action" (Log.val "User.renewAccess")
  catchSuspendInactiveUser uid ZAuth.Expired
  ck' <- lift . wrapHttpClient $ nextCookie ck
  at' <- lift $ newAccessToken (fromMaybe ck ck') at
  pure $ Access at' ck'

revokeAccess ::
  Members '[Input (Local ()), TinyLog, UserQuery] r =>
  UserId ->
  PlainTextPassword ->
  [CookieId] ->
  [CookieLabel] ->
  ExceptT AuthError (AppT r) ()
revokeAccess u pw cc ll = do
  lift . liftSem $
    debug $ field "user" (toByteString u) . field "action" (Log.val "User.revokeAccess")
  locale <- Opt.setDefaultUserLocale <$> view settings
  unlessM (lift . liftSem $ Data.isSamlUser locale u)
    . mapExceptT liftSem
    . semErrToExceptT
    $ Data.authenticate u pw
  lift . wrapClient $ revokeCookies u cc ll

--------------------------------------------------------------------------------
-- Internal

catchSuspendInactiveUser ::
  Members
    '[ GalleyAccess,
       GundeckAccess,
       UserQuery
     ]
    r =>
  UserId ->
  e ->
  ExceptT e (AppT r) ()
catchSuspendInactiveUser uid errval = do
  mustsuspend <- wrapClientE $ mustSuspendInactiveUser uid
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
  forall u a r.
  ZAuth.TokenPair u a =>
  Members
    '[ GalleyAccess,
       GundeckAccess,
       UserQuery
     ]
    r =>
  UserId ->
  CookieType ->
  Maybe CookieLabel ->
  ExceptT LoginError (AppT r) (Access u)
newAccess uid ct cl = do
  catchSuspendInactiveUser uid LoginSuspended
  r <- lift . wrapHttpClient $ newCookieLimited uid ct cl
  case r of
    Left delay -> throwE $ LoginThrottled delay
    Right ck -> do
      t <- lift $ newAccessToken @u @a ck Nothing
      pure $ Access t (Just ck)

resolveLoginId ::
  Members
    '[ P.Error Twilio.ErrorResponse,
       UserHandleStore,
       UserKeyStore,
       Twilio
     ]
    r =>
  LoginId ->
  ExceptT LoginError (AppT r) UserId
resolveLoginId li = do
  usr <-
    liftSemE (validateLoginId li)
      >>= lift
        . either
          (liftSem . getKey)
          (liftSem . lookupHandle)
  case usr of
    Nothing -> do
      pending <- lift . wrapClient $ isPendingActivation li
      throwE $
        if pending
          then LoginPendingActivation
          else LoginFailed
    Just uid -> pure uid

validateLoginId ::
  Members '[P.Error Twilio.ErrorResponse, Twilio] r =>
  LoginId ->
  ExceptT LoginError (Sem r) (Either UserKey Handle)
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

isPendingActivation :: forall m. (MonadClient m, MonadReader Env m) => LoginId -> m Bool
isPendingActivation ident = case ident of
  (LoginByHandle _) -> pure False
  (LoginByEmail e) -> checkKey (userEmailKey e)
  (LoginByPhone p) -> checkKey (userPhoneKey p)
  where
    checkKey k = do
      usr <- (>>= fst) <$> Data.lookupActivationCode k
      locale <- Opt.setDefaultUserLocale <$> view settings
      locDomain <- qualifyLocal ()
      case usr of
        Nothing -> pure False
        Just u ->
          maybe False (checkAccount k)
            <$> ( runM . userQueryToCassandra @m @'[Embed m]
                    . runInputConst locDomain
                    $ Data.lookupAccount locale u
                )
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
    Monad m,
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
  Members
    '[ GalleyAccess,
       GundeckAccess,
       Input (Local ()),
       UserQuery
     ]
    r =>
  SsoLogin ->
  CookieType ->
  ExceptT LoginError (AppT r) (Access ZAuth.User)
ssoLogin (SsoLogin uid label) typ = do
  locale <- Opt.setDefaultUserLocale <$> view settings
  lift
    ( liftSem (runError @ReAuthError (Data.reauthenticate locale uid Nothing))
    )
    >>= \case
      Right _ -> pure ()
      Left ReAuthMissingPassword -> pure ()
      Left ReAuthCodeVerificationRequired -> pure ()
      Left ReAuthCodeVerificationNoPendingCode -> pure ()
      Left ReAuthCodeVerificationNoEmail -> pure ()
      Left (ReAuthError e) -> case e of
        AuthInvalidUser -> throwE LoginFailed
        AuthInvalidCredentials -> pure ()
        AuthSuspended -> throwE LoginSuspended
        AuthEphemeral -> throwE LoginEphemeral
        AuthPendingInvitation -> throwE LoginPendingActivation
  newAccess @ZAuth.User @ZAuth.Access uid typ label

-- | Log in as a LegalHold service, getting LegalHoldUser/Access Tokens.
legalHoldLogin ::
  Members
    '[ GalleyAccess,
       GundeckAccess,
       Input (Local ()),
       UserQuery
     ]
    r =>
  LegalHoldLogin ->
  CookieType ->
  ExceptT LegalHoldLoginError (AppT r) (Access ZAuth.LegalHoldUser)
legalHoldLogin (LegalHoldLogin uid plainTextPassword label) typ = do
  locale <- Opt.setDefaultUserLocale <$> view settings
  mapExceptT liftSem . semErrToExceptT . mapError LegalHoldReAuthError $
    Data.reauthenticate locale uid plainTextPassword
  -- legalhold login is only possible if
  -- the user is a team user
  -- and the team has legalhold enabled
  mteam <- lift . liftSem $ getTeamId uid
  case mteam of
    Nothing -> throwE LegalHoldLoginNoBindingTeam
    Just tid -> mapExceptT liftSem . semErrToExceptT $ assertLegalHoldEnabled tid
  -- create access token and cookie
  newAccess @ZAuth.LegalHoldUser @ZAuth.LegalHoldAccess uid typ label
    !>> LegalHoldLoginError

assertLegalHoldEnabled ::
  Members '[Error LegalHoldLoginError, GalleyAccess] r =>
  TeamId ->
  Sem r ()
assertLegalHoldEnabled tid =
  wsStatus <$> getTeamLegalHoldStatus tid >>= \case
    FeatureStatusDisabled -> throw LegalHoldLoginLegalHoldNotEnabled
    FeatureStatusEnabled -> pure ()
