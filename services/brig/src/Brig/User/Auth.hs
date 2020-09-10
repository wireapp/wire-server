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

-- | High-level user authentication and access control.
module Brig.User.Auth
  ( Access (..),
    sendLoginCode,
    login,
    logout,
    renewAccess,
    revokeAccess,

    -- * Internal
    lookupLoginCode,
    ssoLogin,
    legalHoldLogin,

    -- * Re-exports
    listCookies,
  )
where

import Brig.API.Types
import Brig.API.User (suspendAccount)
import Brig.App
import Brig.Budget
import qualified Brig.Data.Activation as Data
import qualified Brig.Data.LoginCode as Data
import qualified Brig.Data.User as Data
import Brig.Data.UserKey
import qualified Brig.Data.UserKey as Data
import Brig.Email
import qualified Brig.IO.Intra as Intra
import qualified Brig.Options as Opt
import Brig.Phone
import Brig.Types.Common
import Brig.Types.Intra
import Brig.Types.User
import Brig.Types.User.Auth hiding (user)
import Brig.User.Auth.Cookie
import Brig.User.Handle
import Brig.User.Phone
import qualified Brig.ZAuth as ZAuth
import Control.Error hiding (bool)
import Control.Lens (to, view)
import Data.ByteString.Conversion (toByteString)
import Data.Handle (Handle)
import Data.Id
import Data.List1 (singleton)
import Data.Misc (PlainTextPassword (..))
import qualified Data.ZAuth.Token as ZAuth
import Imports
import Network.Wai.Utilities.Error ((!>>))
import System.Logger (field, msg, val, (~~))
import qualified System.Logger.Class as Log
import Wire.API.Team.Feature (TeamFeatureStatus (..), TeamFeatureStatusValue (..))

data Access u = Access
  { accessToken :: !AccessToken,
    accessCookie :: !(Maybe (Cookie (ZAuth.Token u)))
  }

sendLoginCode :: Phone -> Bool -> Bool -> ExceptT SendLoginCodeError AppIO PendingLoginCode
sendLoginCode phone call force = do
  pk <-
    maybe
      (throwE $ SendLoginInvalidPhone phone)
      (return . userPhoneKey)
      =<< lift (validatePhone phone)
  user <- lift $ Data.lookupKey pk
  case user of
    Nothing -> throwE $ SendLoginInvalidPhone phone
    Just u -> do
      Log.debug $ field "user" (toByteString u) . field "action" (Log.val "User.sendLoginCode")
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
        return c

lookupLoginCode :: Phone -> AppIO (Maybe PendingLoginCode)
lookupLoginCode phone =
  Data.lookupKey (userPhoneKey phone) >>= \case
    Nothing -> return Nothing
    Just u -> do
      Log.debug $ field "user" (toByteString u) . field "action" (Log.val "User.lookupLoginCode")
      Data.lookupLoginCode u

login :: Login -> CookieType -> ExceptT LoginError AppIO (Access ZAuth.User)
login (PasswordLogin li pw label) typ = do
  uid <- resolveLoginId li
  Log.debug $ field "user" (toByteString uid) . field "action" (Log.val "User.login")
  checkRetryLimit uid
  Data.authenticate uid pw `catchE` \case
    AuthSuspended -> throwE LoginSuspended
    AuthEphemeral -> throwE LoginEphemeral
    AuthInvalidCredentials -> loginFailed uid
    AuthInvalidUser -> loginFailed uid
  newAccess @ZAuth.User @ZAuth.Access uid typ label
login (SmsLogin phone code label) typ = do
  uid <- resolveLoginId (LoginByPhone phone)
  Log.debug $ field "user" (toByteString uid) . field "action" (Log.val "User.login")
  checkRetryLimit uid
  ok <- lift $ Data.verifyLoginCode uid code
  unless ok $
    loginFailed uid
  newAccess @ZAuth.User @ZAuth.Access uid typ label

loginFailed :: UserId -> ExceptT LoginError AppIO ()
loginFailed uid = decrRetryLimit uid >> throwE LoginFailed

decrRetryLimit :: UserId -> ExceptT LoginError AppIO ()
decrRetryLimit = withRetryLimit (\k b -> withBudget k b $ pure ())

checkRetryLimit :: UserId -> ExceptT LoginError AppIO ()
checkRetryLimit = withRetryLimit checkBudget

withRetryLimit ::
  (BudgetKey -> Budget -> ExceptT LoginError AppIO (Budgeted ())) ->
  UserId ->
  ExceptT LoginError AppIO ()
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

logout :: ZAuth.TokenPair u a => ZAuth.Token u -> ZAuth.Token a -> ExceptT ZAuth.Failure AppIO ()
logout ut at = do
  (u, ck) <- validateTokens ut (Just at)
  lift $ revokeCookies u [cookieId ck] []

renewAccess ::
  ZAuth.TokenPair u a =>
  ZAuth.Token u ->
  Maybe (ZAuth.Token a) ->
  ExceptT ZAuth.Failure AppIO (Access u)
renewAccess ut at = do
  (uid, ck) <- validateTokens ut at
  Log.debug $ field "user" (toByteString uid) . field "action" (Log.val "User.renewAccess")
  catchSuspendInactiveUser uid ZAuth.Expired
  ck' <- lift $ nextCookie ck
  at' <- lift $ newAccessToken (fromMaybe ck ck') at
  return $ Access at' ck'

revokeAccess ::
  UserId ->
  PlainTextPassword ->
  [CookieId] ->
  [CookieLabel] ->
  ExceptT AuthError AppIO ()
revokeAccess u pw cc ll = do
  Log.debug $ field "user" (toByteString u) . field "action" (Log.val "User.revokeAccess")
  Data.authenticate u pw
  lift $ revokeCookies u cc ll

--------------------------------------------------------------------------------
-- Internal

catchSuspendInactiveUser :: UserId -> e -> ExceptT e AppIO ()
catchSuspendInactiveUser uid errval = do
  mustsuspend <- lift $ mustSuspendInactiveUser uid
  when mustsuspend $ do
    Log.warn $
      msg (val "Suspending user due to inactivity")
        ~~ field "user" (toByteString uid)
        ~~ field "action" ("user.suspend" :: String)
    lift $ suspendAccount (singleton uid)
    throwE errval

newAccess :: forall u a. ZAuth.TokenPair u a => UserId -> CookieType -> Maybe CookieLabel -> ExceptT LoginError AppIO (Access u)
newAccess uid ct cl = do
  catchSuspendInactiveUser uid LoginSuspended
  r <- lift $ newCookieLimited uid ct cl
  case r of
    Left delay -> throwE $ LoginThrottled delay
    Right ck -> do
      t <- lift $ newAccessToken @u @a ck Nothing
      return $ Access t (Just ck)

resolveLoginId :: LoginId -> ExceptT LoginError AppIO UserId
resolveLoginId li = do
  usr <- validateLoginId li >>= lift . either lookupKey lookupHandle
  case usr of
    Nothing -> do
      pending <- lift $ isPendingActivation li
      throwE $
        if pending
          then LoginPendingActivation
          else LoginFailed
    Just uid -> return uid

validateLoginId :: LoginId -> ExceptT LoginError AppIO (Either UserKey Handle)
validateLoginId (LoginByEmail email) =
  either
    (const $ throwE LoginFailed)
    (return . Left . userEmailKey)
    (validateEmail email)
validateLoginId (LoginByPhone phone) =
  maybe
    (throwE LoginFailed)
    (return . Left . userPhoneKey)
    =<< lift (validatePhone phone)
validateLoginId (LoginByHandle h) =
  return (Right h)

isPendingActivation :: LoginId -> AppIO Bool
isPendingActivation ident = case ident of
  (LoginByHandle _) -> return False
  (LoginByEmail e) -> checkKey (userEmailKey e)
  (LoginByPhone p) -> checkKey (userPhoneKey p)
  where
    checkKey k = do
      usr <- (>>= fst) <$> Data.lookupActivationCode k
      case usr of
        Nothing -> return False
        Just u -> maybe False (checkAccount k) <$> Data.lookupAccount u
    checkAccount k a =
      let s = accountStatus a
          i = userIdentity (accountUser a)
       in s == Active && case i of
            Just (EmailIdentity e) -> userEmailKey e /= k
            Just (PhoneIdentity p) -> userPhoneKey p /= k
            Just (FullIdentity e p) -> userEmailKey e /= k && userPhoneKey p /= k
            Just SSOIdentity {} -> False -- sso-created users are activated immediately.
            Nothing -> True

validateTokens ::
  ZAuth.TokenPair u a =>
  ZAuth.Token u ->
  Maybe (ZAuth.Token a) ->
  ExceptT ZAuth.Failure AppIO (UserId, Cookie (ZAuth.Token u))
validateTokens ut at = do
  unless (maybe True ((ZAuth.userTokenOf ut ==) . ZAuth.accessTokenOf) at) $
    throwE ZAuth.Invalid
  ExceptT (ZAuth.validateToken ut)
  forM_ at $ \token ->
    ExceptT (ZAuth.validateToken token)
      `catchE` \e ->
        unless (e == ZAuth.Expired) (throwE e)
  ck <- lift (lookupCookie ut) >>= maybe (throwE ZAuth.Invalid) return
  return (ZAuth.userTokenOf ut, ck)

-- | Allow to login as any user without having the credentials.
ssoLogin :: SsoLogin -> CookieType -> ExceptT LoginError AppIO (Access ZAuth.User)
ssoLogin (SsoLogin uid label) typ = do
  Data.reauthenticate uid Nothing `catchE` \case
    ReAuthMissingPassword -> pure ()
    ReAuthError e -> case e of
      AuthInvalidCredentials -> pure ()
      AuthSuspended -> throwE LoginSuspended
      AuthEphemeral -> throwE LoginEphemeral
      AuthInvalidUser -> throwE LoginFailed
  newAccess @ZAuth.User @ZAuth.Access uid typ label

-- | Log in as a LegalHold service, getting LegalHoldUser/Access Tokens.
legalHoldLogin :: LegalHoldLogin -> CookieType -> ExceptT LegalHoldLoginError AppIO (Access ZAuth.LegalHoldUser)
legalHoldLogin (LegalHoldLogin uid plainTextPassword label) typ = do
  Data.reauthenticate uid plainTextPassword !>> LegalHoldReAuthError
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

assertLegalHoldEnabled :: TeamId -> ExceptT LegalHoldLoginError AppIO ()
assertLegalHoldEnabled tid = do
  stat <- lift $ Intra.getTeamLegalHoldStatus tid
  case teamFeatureStatusValue stat of
    TeamFeatureDisabled -> throwE LegalHoldLoginLegalHoldNotEnabled
    TeamFeatureEnabled -> pure ()
