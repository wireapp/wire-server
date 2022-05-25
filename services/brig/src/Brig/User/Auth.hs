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

import Bilge.IO
import Bilge.RPC
import Brig.API.Types
import Brig.API.User (changeSingleAccountStatus)
import Brig.App
import qualified Brig.Code as Code
import qualified Brig.Data.Activation as Data
import qualified Brig.Data.LoginCode as Data
import qualified Brig.Data.User as Data
import Brig.Data.UserKey
import qualified Brig.Data.UserKey as Data
import Brig.Email
import qualified Brig.IO.Intra as Intra
import qualified Brig.Options as Opt
import Brig.Phone
import Brig.Sem.BudgetStore
import Brig.Sem.BudgetStore.Cassandra
import Brig.Sem.UserQuery (UserQuery)
import Brig.Sem.UserQuery.Cassandra
import Brig.Types.Common
import Brig.Types.Intra
import Brig.Types.User
import Brig.Types.User.Auth hiding (user)
import Brig.User.Auth.Cookie
import Brig.User.Handle
import Brig.User.Phone
import Brig.User.Search.Index
import qualified Brig.ZAuth as ZAuth
import Cassandra
import Control.Error hiding (bool)
import Control.Lens (to, view)
import Control.Monad.Catch
import Control.Monad.Trans.Except
import Data.ByteString.Conversion (toByteString)
import Data.Either.Combinators
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
import Polysemy.Error
import Polysemy.Input
import System.Logger (field, msg, val, (~~))
import qualified System.Logger.Class as Log
import Wire.API.Team.Feature (TeamFeatureStatusNoConfig (..), TeamFeatureStatusValue (..))
import qualified Wire.API.Team.Feature as Public
import Wire.API.User (VerificationAction (..))

data Access u = Access
  { accessToken :: !AccessToken,
    accessCookie :: !(Maybe (Cookie (ZAuth.Token u)))
  }

sendLoginCode ::
  forall m.
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
        defLoc <- Opt.setDefaultUserLocale <$> view settings
        l <- runM $ userQueryToCassandra @m @'[Embed m] $ Data.lookupLocale defLoc u
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
  forall m.
  ( MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    MonadIO m,
    HasRequestId m,
    Log.MonadLogger m,
    MonadClient m,
    ZAuth.MonadZAuth m,
    MonadIndexIO m,
    MonadUnliftIO m
  ) =>
  Login ->
  CookieType ->
  ExceptT LoginError m (Access ZAuth.User)
login (PasswordLogin li pw label code) typ = do
  uid <- resolveLoginId li
  lift . Log.debug $ field "user" (toByteString uid) . field "action" (Log.val "User.login")
  mLimitFailedLogins <- view (settings . to Opt.setLimitFailedLogins)
  runBudgetStoreAction $ checkRetryLimit uid mLimitFailedLogins
  o <- runUserQueryAction $ Data.authenticate uid pw
  whenLeft o $ \case
    AuthInvalidUser -> runBudgetStoreAction $ loginFailed uid mLimitFailedLogins
    AuthInvalidCredentials -> runBudgetStoreAction $ loginFailed uid mLimitFailedLogins
    AuthSuspended -> throwE LoginSuspended
    AuthEphemeral -> throwE LoginEphemeral
    AuthPendingInvitation -> throwE LoginPendingActivation
  verifyLoginCode code uid mLimitFailedLogins
  newAccess @ZAuth.User @ZAuth.Access uid typ label
  where
    verifyLoginCode :: Maybe Code.Value -> UserId -> Maybe Opt.LimitFailedLogins -> ExceptT LoginError m ()
    verifyLoginCode mbCode uid mLimitFailedLogins =
      verifyCode mbCode Login uid
        `catchE` \case
          VerificationCodeNoPendingCode -> runBudgetStoreAction $ loginFailedWith LoginCodeInvalid uid mLimitFailedLogins
          VerificationCodeRequired -> runBudgetStoreAction $ loginFailedWith LoginCodeRequired uid mLimitFailedLogins
          VerificationCodeNoEmail -> runBudgetStoreAction $ loginFailed uid mLimitFailedLogins
login (SmsLogin phone code label) typ = do
  uid <- resolveLoginId (LoginByPhone phone)
  lift . Log.debug $ field "user" (toByteString uid) . field "action" (Log.val "User.login")
  mLimitFailedLogins <- view (settings . to Opt.setLimitFailedLogins)
  runBudgetStoreAction $ checkRetryLimit uid mLimitFailedLogins
  ok <- lift $ Data.verifyLoginCode uid code
  unless ok $
    runBudgetStoreAction $ loginFailed uid mLimitFailedLogins
  newAccess @ZAuth.User @ZAuth.Access uid typ label

verifyCode ::
  forall m.
  ( MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    MonadIO m,
    HasRequestId m,
    Log.MonadLogger m,
    MonadClient m
  ) =>
  Maybe Code.Value ->
  VerificationAction ->
  UserId ->
  ExceptT VerificationCodeError m ()
verifyCode mbCode action uid = do
  (mbEmail, mbTeamId) <- getEmailAndTeamId uid
  featureEnabled <- lift $ do
    mbFeatureEnabled <- Intra.getVerificationCodeEnabled `traverse` mbTeamId
    pure $ fromMaybe (Public.tfwoapsStatus (Public.defTeamFeatureStatus @'Public.TeamFeatureSndFactorPasswordChallenge) == Public.TeamFeatureEnabled) mbFeatureEnabled
  when featureEnabled $ do
    case (mbCode, mbEmail) of
      (Just code, Just email) -> do
        key <- Code.mkKey $ Code.ForEmail email
        codeValid <- isJust <$> Code.verify key (Code.scopeFromAction action) code
        unless codeValid $ throwE VerificationCodeNoPendingCode
      (Nothing, _) -> throwE VerificationCodeRequired
      (_, Nothing) -> throwE VerificationCodeNoEmail
  where
    getEmailAndTeamId ::
      UserId ->
      ExceptT e m (Maybe Email, Maybe TeamId)
    getEmailAndTeamId u = do
      locDomain <- qualifyLocal ()
      locale <- Opt.setDefaultUserLocale <$> view settings
      mbAccount <-
        lift
          . runM
          . userQueryToCassandra @m @'[Embed m]
          . runInputConst locDomain
          $ Data.lookupAccount locale u
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
  ExceptT ZAuth.Failure m (Access u)
renewAccess uts at = do
  (uid, ck) <- validateTokens uts at
  lift . Log.debug $ field "user" (toByteString uid) . field "action" (Log.val "User.renewAccess")
  catchSuspendInactiveUser uid ZAuth.Expired
  ck' <- lift $ nextCookie ck
  at' <- lift $ newAccessToken (fromMaybe ck ck') at
  pure $ Access at' ck'

revokeAccess ::
  forall m.
  (MonadClient m, Log.MonadLogger m, MonadReader Env m) =>
  UserId ->
  PlainTextPassword ->
  [CookieId] ->
  [CookieLabel] ->
  ExceptT AuthError m ()
revokeAccess u pw cc ll = do
  lift $ Log.debug $ field "user" (toByteString u) . field "action" (Log.val "User.revokeAccess")
  locDomain <- qualifyLocal ()
  locale <- Opt.setDefaultUserLocale <$> view settings
  unlessM (lift . runM . userQueryToCassandra @m @'[Embed m] $ runInputConst locDomain $ Data.isSamlUser locale u) $
    runUserQueryAction (Data.authenticate u pw) >>= except
  lift $ revokeCookies u cc ll

--------------------------------------------------------------------------------
-- Internal

catchSuspendInactiveUser ::
  ( MonadClient m,
    Log.MonadLogger m,
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
  CookieType ->
  Maybe CookieLabel ->
  ExceptT LoginError m (Access u)
newAccess uid ct cl = do
  catchSuspendInactiveUser uid LoginSuspended
  r <- lift $ newCookieLimited uid ct cl
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
  forall m.
  ( MonadClient m,
    MonadReader Env m,
    ZAuth.MonadZAuth m,
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
  locale <- Opt.setDefaultUserLocale <$> view settings
  locDomain <- qualifyLocal ()
  o <- runUserQueryAction $ runInputConst locDomain $ Data.reauthenticate locale uid Nothing
  whenLeft o $ \case
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
  newAccess @ZAuth.User @ZAuth.Access uid typ label

-- | Log in as a LegalHold service, getting LegalHoldUser/Access Tokens.
legalHoldLogin ::
  ( MonadClient m,
    MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    Log.MonadLogger m,
    ZAuth.MonadZAuth m,
    MonadIndexIO m,
    MonadUnliftIO m
  ) =>
  LegalHoldLogin ->
  CookieType ->
  ExceptT LegalHoldLoginError m (Access ZAuth.LegalHoldUser)
legalHoldLogin (LegalHoldLogin uid plainTextPassword label) typ = do
  locale <- Opt.setDefaultUserLocale <$> view settings
  locDomain <- qualifyLocal ()
  o <- runUserQueryAction $ runInputConst locDomain $ Data.reauthenticate locale uid plainTextPassword
  except o !>> LegalHoldReAuthError
  -- legalhold login is only possible if
  -- the user is a team user
  -- and the team has legalhold enabled
  mteam <- lift $ Intra.getTeamId uid
  case mteam of
    Nothing -> throwE LegalHoldLoginNoBindingTeam
    Just tid -> assertLegalHoldEnabled tid
  -- create access token and cookie
  newAccess @ZAuth.LegalHoldUser @ZAuth.LegalHoldAccess uid typ label
    !>> LegalHoldLoginError

assertLegalHoldEnabled ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    Log.MonadLogger m
  ) =>
  TeamId ->
  ExceptT LegalHoldLoginError m ()
assertLegalHoldEnabled tid = do
  stat <- lift $ Intra.getTeamLegalHoldStatus tid
  case tfwoStatus stat of
    TeamFeatureDisabled -> throwE LegalHoldLoginLegalHoldNotEnabled
    TeamFeatureEnabled -> pure ()

--------------------------------------------------------------------------------
-- Polysemy crutches
--
-- These can be removed once functions in this module run in 'Sem r' instead of
-- 'ExceptT e m' or 'm' for some constrained 'm'.

runBudgetStoreAction ::
  forall m e a.
  MonadClient m =>
  Sem '[Error e, BudgetStore, Embed m] a ->
  ExceptT e m a
runBudgetStoreAction = runStoreActionExceptT (budgetStoreToCassandra @m)

runUserQueryAction ::
  forall m e a t.
  (MonadClient m, MonadTrans t) =>
  Sem '[Error e, UserQuery, Embed m] a ->
  t m (Either e a)
runUserQueryAction = runStoreAction (userQueryToCassandra @m)

runStoreAction ::
  forall m e a t store.
  (MonadClient m, MonadTrans t) =>
  (forall n b. (MonadClient n, n ~ m) => Sem '[store, Embed n] b -> Sem '[Embed n] b) ->
  Sem '[Error e, store, Embed m] a ->
  t m (Either e a)
runStoreAction interpreter = lift . runM . interpreter @m . runError @e

runStoreActionExceptT ::
  forall m e a store.
  MonadClient m =>
  (forall n b. (MonadClient n, n ~ m) => Sem '[store, Embed n] b -> Sem '[Embed n] b) ->
  Sem '[Error e, store, Embed m] a ->
  ExceptT e m a
runStoreActionExceptT interpreter = runStoreAction interpreter >=> except
