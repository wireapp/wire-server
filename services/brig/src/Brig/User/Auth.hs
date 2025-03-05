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
    login,
    logout,
    renewAccess,
    validateTokens,
    revokeAccess,
    verifyCode,

    -- * Internal
    ssoLogin,
    legalHoldLogin,

    -- * Re-exports
    listCookies,
  )
where

import Brig.API.Types
import Brig.API.User (changeSingleAccountStatus)
import Brig.App
import Brig.Budget
import Brig.Data.Client
import Brig.Options qualified as Opt
import Brig.Types.Intra
import Brig.User.Auth.Cookie
import Cassandra
import Control.Error hiding (bool)
import Data.ByteString.Conversion (toByteString)
import Data.Code qualified as Code
import Data.Default
import Data.Handle (Handle)
import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.List1 (List1)
import Data.List1 qualified as List1
import Data.Misc (PlainTextPassword6)
import Data.Qualified
import Data.ZAuth.Token qualified as ZAuth
import Data.ZAuth.Validation qualified as ZAuth
import Debug.Trace
import Imports
import Network.Wai.Utilities.Error ((!>>))
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger (field, msg, val, (~~))
import Util.Timeout
import Wire.API.Team.Feature
import Wire.API.Team.Feature qualified as Public
import Wire.API.User
import Wire.API.User.Auth
import Wire.API.User.Auth.LegalHold
import Wire.API.User.Auth.Sso
import Wire.ActivationCodeStore (ActivationCodeStore)
import Wire.ActivationCodeStore qualified as ActivationCode
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem qualified as Authentication
import Wire.AuthenticationSubsystem.ZAuth qualified as ZAuth
import Wire.Events (Events)
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.UserKeyStore
import Wire.UserStore
import Wire.UserSubsystem (UserSubsystem)
import Wire.UserSubsystem qualified as User
import Wire.VerificationCode qualified as VerificationCode
import Wire.VerificationCodeGen qualified as VerificationCodeGen
import Wire.VerificationCodeSubsystem (VerificationCodeSubsystem)
import Wire.VerificationCodeSubsystem qualified as VerificationCodeSubsystem

login ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member (Input (Local ())) r,
    Member ActivationCodeStore r,
    Member Events r,
    Member TinyLog r,
    Member UserKeyStore r,
    Member UserStore r,
    Member UserSubsystem r,
    Member VerificationCodeSubsystem r,
    Member AuthenticationSubsystem r
  ) =>
  Login ->
  CookieType ->
  ExceptT LoginError (AppT r) (Access ZAuth.User)
login (MkLogin li pw label code) typ = do
  uid <- resolveLoginId li
  lift . liftSem . Log.debug $ field "user" (toByteString uid) . field "action" (val "User.login")
  wrapClientE $ checkRetryLimit uid

  (lift . liftSem $ Authentication.authenticateEither uid pw) >>= \case
    Right a -> pure a
    Left e -> case e of
      AuthInvalidUser -> lift (decrRetryLimit uid) >> throwE LoginFailed
      AuthInvalidCredentials -> lift (decrRetryLimit uid) >> throwE LoginFailed
      AuthSuspended -> throwE LoginSuspended
      AuthEphemeral -> throwE LoginEphemeral
      AuthPendingInvitation -> throwE LoginPendingActivation
  verifyLoginCode code uid
  newAccess @ZAuth.User @ZAuth.Access uid Nothing typ label
  where
    verifyLoginCode :: Maybe Code.Value -> UserId -> ExceptT LoginError (AppT r) ()
    verifyLoginCode mbCode uid = do
      luid <- lift $ qualifyLocal uid
      verifyCode mbCode Login luid
        `catchE` \case
          VerificationCodeNoPendingCode -> lift (decrRetryLimit uid) >> throwE LoginCodeInvalid
          VerificationCodeRequired -> lift (decrRetryLimit uid) >> throwE LoginCodeRequired
          VerificationCodeNoEmail -> lift (decrRetryLimit uid) >> throwE LoginFailed

verifyCode ::
  forall r.
  (Member GalleyAPIAccess r, Member VerificationCodeSubsystem r, Member UserSubsystem r) =>
  Maybe Code.Value ->
  VerificationAction ->
  Local UserId ->
  ExceptT VerificationCodeError (AppT r) ()
verifyCode mbCode action luid = do
  (mbEmail, mbTeamId) <- getEmailAndTeamId luid
  featureEnabled <- lift $ do
    mbFeatureEnabled <- liftSem $ GalleyAPIAccess.getVerificationCodeEnabled `traverse` mbTeamId
    pure $ fromMaybe ((def @(Feature Public.SndFactorPasswordChallengeConfig)).status == Public.FeatureStatusEnabled) mbFeatureEnabled
  account <- lift . liftSem $ User.getAccountNoFilter luid
  let isSsoUser = maybe False isSamlUser account
  when (featureEnabled && not isSsoUser) $ do
    case (mbCode, mbEmail) of
      (Just code, Just email) -> do
        let key = VerificationCodeGen.mkKey email
            scope = VerificationCode.scopeFromAction action
        codeValid <- isJust <$> lift (liftSem $ VerificationCodeSubsystem.verifyCode key scope code)
        unless codeValid $ throwE VerificationCodeNoPendingCode
      (Nothing, _) -> throwE VerificationCodeRequired
      (_, Nothing) -> throwE VerificationCodeNoEmail
  where
    getEmailAndTeamId ::
      Local UserId ->
      ExceptT e (AppT r) (Maybe EmailAddress, Maybe TeamId)
    getEmailAndTeamId u = do
      mbAccount <- lift . liftSem $ User.getAccountNoFilter u
      pure
        ( userEmail =<< mbAccount,
          userTeam =<< mbAccount
        )

decrRetryLimit :: UserId -> (AppT r) ()
decrRetryLimit = wrapClient . withRetryLimit (\k b -> withBudget k b $ pure ())

checkRetryLimit ::
  ( MonadReader Env m,
    MonadClient m
  ) =>
  UserId ->
  ExceptT LoginError m ()
checkRetryLimit uid =
  flip withRetryLimit uid $ \budgetKey budget ->
    checkBudget budgetKey budget >>= \case
      BudgetExhausted ttl -> throwE . LoginBlocked . RetryAfter . floor $ ttl
      BudgetedValue () remaining -> pure $ BudgetedValue () remaining

withRetryLimit ::
  (MonadReader Env m) =>
  (BudgetKey -> Budget -> m (Budgeted ())) ->
  UserId ->
  m ()
withRetryLimit action uid = do
  mLimitFailedLogins <- asks (.settings.limitFailedLogins)
  forM_ mLimitFailedLogins $ \opts -> do
    let bkey = BudgetKey ("login#" <> idToText uid)
        budget =
          Budget
            (timeoutDiff $ Opt.timeout opts)
            (fromIntegral $ Opt.retryLimit opts)
    action bkey budget

logout ::
  (ZAuth.UserTokenLike u, ZAuth.AccessTokenLike a) =>
  List1 (ZAuth.Token u) ->
  ZAuth.Token a ->
  ExceptT ZAuth.Failure (AppT r) ()
logout uts at = do
  (u, ck) <- validateTokens uts (Just at)
  lift $ wrapClient $ revokeCookies u [cookieId ck] []

renewAccess ::
  forall r u a.
  ( Member TinyLog r,
    Member UserSubsystem r,
    Member Events r,
    Show u,
    ZAuth.UserTokenLike u,
    ZAuth.AccessTokenLike a
  ) =>
  List1 (ZAuth.Token u) ->
  Maybe (ZAuth.Token a) ->
  Maybe ClientId ->
  ExceptT ZAuth.Failure (AppT r) (Access u)
renewAccess uts at mcid = do
  (uid, ck) <- validateTokens uts at
  traceShowM uid
  traceShowM ck
  wrapClientE $ traverse_ (checkClientId uid) mcid
  lift . liftSem . Log.debug $ field "user" (toByteString uid) . field "action" (val "User.renewAccess")
  catchSuspendInactiveUser uid ZAuth.Expired
  ck' <- wrapHttpClientE $ nextCookie ck mcid
  at' <- lift $ newAccessToken (fromMaybe ck ck') at
  pure $ Access at' ck'

revokeAccess ::
  ( Member TinyLog r,
    Member UserSubsystem r,
    Member AuthenticationSubsystem r
  ) =>
  Local UserId ->
  PlainTextPassword6 ->
  [CookieId] ->
  [CookieLabel] ->
  ExceptT AuthError (AppT r) ()
revokeAccess luid@(tUnqualified -> u) pw cc ll = do
  lift . liftSem $ Log.debug $ field "user" (toByteString u) . field "action" (val "User.revokeAccess")
  isSaml <- lift . liftSem $ do
    account <- User.getAccountNoFilter luid
    pure $ maybe False isSamlUser account
  unless isSaml do
    (lift . liftSem $ Authentication.authenticateEither u pw)
      >>= either throwE pure
  lift $ wrapHttpClient $ revokeCookies u cc ll

--------------------------------------------------------------------------------
-- Internal

catchSuspendInactiveUser ::
  ( Member TinyLog r,
    Member UserSubsystem r,
    Member Events r
  ) =>
  UserId ->
  e ->
  ExceptT e (AppT r) ()
catchSuspendInactiveUser uid errval = do
  mustsuspend <- lift $ wrapHttpClient $ mustSuspendInactiveUser uid
  when mustsuspend $ do
    lift . liftSem . Log.warn $
      msg (val "Suspending user due to inactivity")
        ~~ field "user" (toByteString uid)
        ~~ field "action" ("user.suspend" :: String)
    lift $ runExceptT (changeSingleAccountStatus uid Suspended) >>= explicitlyIgnoreErrors
    throwE errval
  where
    explicitlyIgnoreErrors :: (Monad m) => Either AccountStatusError () -> m ()
    explicitlyIgnoreErrors = \case
      Left InvalidAccountStatus -> pure ()
      Left AccountNotFound -> pure ()
      Right () -> pure ()

newAccess ::
  forall u a r.
  ( Member TinyLog r,
    Member UserSubsystem r,
    Member Events r,
    ZAuth.UserTokenLike u,
    ZAuth.AccessTokenLike a
  ) =>
  UserId ->
  Maybe ClientId ->
  CookieType ->
  Maybe CookieLabel ->
  ExceptT LoginError (AppT r) (Access u)
newAccess uid cid ct cl = do
  catchSuspendInactiveUser uid LoginSuspended
  r <- lift $ wrapHttpClient $ newCookieLimited uid cid ct cl
  case r of
    Left delay -> throwE $ LoginThrottled delay
    Right ck -> do
      t <- lift $ newAccessToken @u @a ck Nothing
      pure $ Access t (Just ck)

resolveLoginId ::
  ( Member UserKeyStore r,
    Member UserStore r,
    Member UserSubsystem r,
    Member ActivationCodeStore r,
    Member (Input (Local ())) r
  ) =>
  LoginId ->
  ExceptT LoginError (AppT r) UserId
resolveLoginId li = do
  usr <- lift . liftSem . either lookupKey lookupHandle $ validateLoginId li
  case usr of
    Nothing -> do
      pending <- lift $ isPendingActivation li
      throwE $
        if pending
          then LoginPendingActivation
          else LoginFailed
    Just uid -> pure uid

validateLoginId :: LoginId -> Either EmailKey Handle
validateLoginId (LoginByEmail email) = (Left . mkEmailKey) email
validateLoginId (LoginByHandle h) = Right h

isPendingActivation ::
  forall r.
  ( Member UserSubsystem r,
    Member ActivationCodeStore r,
    Member (Input (Local ())) r
  ) =>
  LoginId ->
  AppT r Bool
isPendingActivation ident = case ident of
  (LoginByHandle _) -> pure False
  (LoginByEmail e) -> checkKey (mkEmailKey e)
  where
    checkKey :: EmailKey -> AppT r Bool
    checkKey k = do
      musr <- (>>= fst) <$> liftSem (ActivationCode.lookupActivationCode k)
      case musr of
        Nothing -> pure False
        Just usr -> liftSem do
          lusr <- qualifyLocal' usr
          maybe False (checkAccount k) <$> User.getAccountNoFilter lusr

    checkAccount :: EmailKey -> User -> Bool
    checkAccount k a =
      let i = userIdentity a
          statusAdmitsPending = case userStatus a of
            Active -> True
            Suspended -> False
            Deleted -> False
            Ephemeral -> False
            PendingInvitation -> True
       in statusAdmitsPending && case i of
            Just (EmailIdentity e) -> mkEmailKey e /= k
            Just SSOIdentity {} -> False -- sso-created users are activated immediately.
            Nothing -> True

-- | Validate a list of (User/LH) tokens potentially with an associated access token.
--   If there are multiple valid cookies, we try all of them. When an access token is
--   given, we perform the usual checks.
--   If multiple cookies are given and several are valid, we return the first valid one.
validateTokens ::
  (ZAuth.UserTokenLike u, ZAuth.AccessTokenLike a) =>
  List1 (ZAuth.Token u) ->
  Maybe (ZAuth.Token a) ->
  ExceptT ZAuth.Failure (AppT r) (UserId, Cookie (ZAuth.Token u))
validateTokens uts at = do
  tokens <- forM uts $ \ut -> lift $ runExceptT (validateToken ut at)
  getFirstSuccessOrFirstFail tokens
  where
    -- FUTUREWORK: There is surely a better way to do this
    getFirstSuccessOrFirstFail ::
      (Monad m) =>
      List1 (Either ZAuth.Failure (UserId, Cookie (ZAuth.Token u))) ->
      ExceptT ZAuth.Failure m (UserId, Cookie (ZAuth.Token u))
    getFirstSuccessOrFirstFail tks = case (lefts $ NE.toList $ List1.toNonEmpty tks, rights $ NE.toList $ List1.toNonEmpty tks) of
      (_, suc : _) -> pure suc
      (e : _, _) -> throwE e
      _ -> throwE ZAuth.Invalid -- Impossible

validateToken ::
  (ZAuth.UserTokenLike u, ZAuth.AccessTokenLike a) =>
  ZAuth.Token u ->
  Maybe (ZAuth.Token a) ->
  ExceptT ZAuth.Failure (AppT r) (UserId, Cookie (ZAuth.Token u))
validateToken ut at = do
  unless (maybe True ((ZAuth.userTokenOf ut ==) . ZAuth.accessTokenOf) at) $
    throwE ZAuth.Invalid
  ExceptT (ZAuth.validateToken ut)
  forM_ at $ \token ->
    ExceptT (ZAuth.validateToken token)
      `catchE` \e ->
        unless (e == ZAuth.Expired) (throwE e)
  ck <- lift (wrapClient $ lookupCookie ut) >>= maybe (throwE ZAuth.Invalid) pure
  pure (ZAuth.userTokenOf ut, ck)

-- | Allow to login as any user without having the credentials.
ssoLogin ::
  ( Member TinyLog r,
    Member UserSubsystem r,
    Member Events r,
    Member AuthenticationSubsystem r
  ) =>
  SsoLogin ->
  CookieType ->
  ExceptT LoginError (AppT r) (Access ZAuth.User)
ssoLogin (SsoLogin uid label) typ = do
  lift
    (liftSem $ Authentication.reauthenticateEither uid Nothing)
    >>= \case
      Right a -> pure a
      Left loginErr -> case loginErr of
        -- Important: We throw on Missing Password here because this can only be thrown
        -- for non-SSO users, so if we got this error, someone tried to authenticate
        -- a regular user as if they were an SSO user, bypassing pwd requirements.
        -- This would be a serious security issue if this weren't an internal endpoint.
        ReAuthMissingPassword -> throwE LoginFailed
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
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member AuthenticationSubsystem r,
    Member Events r
  ) =>
  LegalHoldLogin ->
  CookieType ->
  ExceptT LegalHoldLoginError (AppT r) (Access ZAuth.LegalHoldUser)
legalHoldLogin (LegalHoldLogin uid pw label) typ = do
  (lift . liftSem $ Authentication.reauthenticateEither uid pw)
    >>= either (throwE . LegalHoldReAuthError) (const $ pure ())
  -- legalhold login is only possible if
  -- the user is a team user
  -- and the team has legalhold enabled
  mteam <- lift $ liftSem $ GalleyAPIAccess.getTeamId uid
  case mteam of
    Nothing -> throwE LegalHoldLoginNoBindingTeam
    Just tid -> assertLegalHoldEnabled tid
  -- create access token and cookie
  newAccess @ZAuth.LegalHoldUser @ZAuth.LegalHoldAccess uid Nothing typ label
    !>> LegalHoldLoginError

assertLegalHoldEnabled ::
  (Member GalleyAPIAccess r) =>
  TeamId ->
  ExceptT LegalHoldLoginError (AppT r) ()
assertLegalHoldEnabled tid = do
  feat <- lift $ liftSem $ GalleyAPIAccess.getTeamLegalHoldStatus tid
  case feat.status of
    FeatureStatusDisabled -> throwE LegalHoldLoginLegalHoldNotEnabled
    FeatureStatusEnabled -> pure ()

checkClientId :: (MonadClient m) => UserId -> ClientId -> ExceptT ZAuth.Failure m ()
checkClientId uid cid =
  lookupClient uid cid >>= maybe (throwE ZAuth.Invalid) (const (pure ()))
