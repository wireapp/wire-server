{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | High-level user authentication and access control.
module Brig.User.Auth
    ( Access (..)
    , sendLoginCode
    , login
    , logout
    , renewAccess
    , revokeAccess

      -- * Internal
    , lookupLoginCode
    , ssoLogin

      -- * Re-exports
    , listCookies
    ) where

import Imports
import Brig.App
import Brig.API.Types
import Brig.Email
import Brig.Data.UserKey
import Brig.Phone
import Brig.User.Auth.Cookie
import Brig.User.Handle
import Brig.User.Phone
import Brig.Types.Common
import Brig.Types.Intra
import Brig.Types.User
import Brig.Types.User.Auth hiding (user)
import Control.Error
import Data.Id
import Data.Misc (PlainTextPassword (..))

import qualified Brig.Data.Activation as Data
import qualified Brig.Data.LoginCode as Data
import qualified Brig.Data.User as Data
import qualified Brig.Data.UserKey as Data
import qualified Brig.ZAuth as ZAuth

data Access = Access
    { accessToken  :: !AccessToken
    , accessCookie :: !(Maybe (Cookie ZAuth.UserToken))
    }

sendLoginCode :: Phone -> Bool -> Bool -> ExceptT SendLoginCodeError AppIO PendingLoginCode
sendLoginCode phone call force = do
    pk <- maybe (throwE $ SendLoginInvalidPhone phone)
                (return . userPhoneKey)
                =<< lift (validatePhone phone)
    user <- lift $ Data.lookupKey pk
    case user of
        Nothing -> throwE $ SendLoginInvalidPhone phone
        Just  u -> do
            pw <- lift $ Data.lookupPassword u
            unless (isNothing pw || force) $
                throwE SendLoginPasswordExists
            lift $ do
                l <- Data.lookupLocale u
                c <- Data.createLoginCode u
                void . forPhoneKey pk $ \ph ->
                    if call then sendLoginCall ph (pendingLoginCode c) l
                            else sendLoginSms ph (pendingLoginCode c) l
                return c

lookupLoginCode :: Phone -> AppIO (Maybe PendingLoginCode)
lookupLoginCode phone = Data.lookupKey (userPhoneKey phone) >>= \case
    Nothing -> return Nothing
    Just  u -> Data.lookupLoginCode u

login :: Login -> CookieType -> ExceptT LoginError AppIO Access
login (PasswordLogin li pw label) typ = do
    uid <- resolveLoginId li
    Data.authenticate uid pw `catchE` \case
        AuthSuspended          -> throwE LoginSuspended
        AuthEphemeral          -> throwE LoginEphemeral
        AuthInvalidCredentials -> throwE LoginFailed
        AuthInvalidUser        -> throwE LoginFailed
    newAccess uid typ label
login (SmsLogin phone code label) typ = do
    uid <- resolveLoginId (LoginByPhone phone)
    ok <- lift $ Data.verifyLoginCode uid code
    unless ok $
        throwE LoginFailed
    newAccess uid typ label

logout :: ZAuth.UserToken -> ZAuth.AccessToken -> ExceptT ZAuth.Failure AppIO ()
logout ut at = do
    (u, ck) <- validateTokens ut (Just at)
    lift $ revokeCookies u [cookieId ck] []

renewAccess
    :: ZAuth.UserToken
    -> Maybe ZAuth.AccessToken
    -> ExceptT ZAuth.Failure AppIO Access
renewAccess ut at = do
    (_, ck) <- validateTokens ut at
    ck' <- lift $ nextCookie ck
    at' <- lift $ newAccessToken (fromMaybe ck ck') at
    return $ Access at' ck'

revokeAccess
    :: UserId
    -> PlainTextPassword
    -> [CookieId]
    -> [CookieLabel]
    -> ExceptT AuthError AppIO ()
revokeAccess u pw cc ll = do
    Data.authenticate u pw
    lift $ revokeCookies u cc ll

--------------------------------------------------------------------------------
-- Internal

newAccess :: UserId -> CookieType -> Maybe CookieLabel -> ExceptT LoginError AppIO Access
newAccess u ct cl = do
    r <- lift $ newCookieLimited u ct cl
    case r of
        Left delay -> throwE $ LoginThrottled delay
        Right ck   -> do
            t <- lift $ newAccessToken ck Nothing
            return $ Access t (Just ck)

resolveLoginId :: LoginId -> ExceptT LoginError AppIO UserId
resolveLoginId li = do
    usr <- validateLoginId li >>= lift . either lookupKey lookupHandle
    case usr of
        Nothing  -> do
            pending <- lift $ isPendingActivation li
            throwE $ if pending
                then LoginPendingActivation
                else LoginFailed
        Just uid -> return uid

validateLoginId :: LoginId -> ExceptT LoginError AppIO (Either UserKey Handle)
validateLoginId (LoginByEmail email) =
    either (const $ throwE LoginFailed)
           (return . Left . userEmailKey)
           (validateEmail email)
validateLoginId (LoginByPhone phone) =
    maybe (throwE LoginFailed)
          (return . Left . userPhoneKey)
          =<< lift (validatePhone phone)
validateLoginId (LoginByHandle h) =
    return (Right h)

isPendingActivation :: LoginId -> AppIO Bool
isPendingActivation ident = case ident of
    (LoginByHandle _) -> return False
    (LoginByEmail  e) -> checkKey (userEmailKey e)
    (LoginByPhone  p) -> checkKey (userPhoneKey p)
  where
    checkKey k = do
        usr <- (>>= fst) <$> Data.lookupActivationCode k
        case usr of
            Nothing -> return False
            Just  u -> maybe False (checkAccount k) <$> Data.lookupAccount u

    checkAccount k a =
        let s = accountStatus a
            i = userIdentity (accountUser a)
        in s == Active && case i of
            Just (EmailIdentity  e) -> userEmailKey e /= k
            Just (PhoneIdentity  p) -> userPhoneKey p /= k
            Just (FullIdentity e p) -> userEmailKey e /= k && userPhoneKey p /= k
            Just SSOIdentity {}     -> False  -- sso-created users are activated immediately.
            Nothing                 -> True

validateTokens
    :: ZAuth.UserToken
    -> Maybe ZAuth.AccessToken
    -> ExceptT ZAuth.Failure AppIO (UserId, Cookie ZAuth.UserToken)
validateTokens ut at = do
    unless (maybe True ((ZAuth.userTokenOf ut ==) . ZAuth.accessTokenOf) at) $
        throwE ZAuth.Invalid
    ExceptT (ZAuth.validateToken ut)
    forM_ at $ \a ->
        ExceptT (ZAuth.validateToken a)
            `catchE` \e ->
                unless (e == ZAuth.Expired) (throwE e)
    ck <- lift (lookupCookie ut) >>= maybe (throwE ZAuth.Invalid) return
    return (ZAuth.userTokenOf ut, ck)

-- | Allow to login as any user without having the credentials.
ssoLogin :: SsoLogin -> CookieType -> ExceptT LoginError AppIO Access
ssoLogin (SsoLogin uid label) typ = do
    Data.reauthenticate uid Nothing `catchE` \case
        ReAuthMissingPassword -> pure ()
        ReAuthError e -> case e of
            AuthInvalidCredentials -> pure ()
            AuthSuspended          -> throwE LoginSuspended
            AuthEphemeral          -> throwE LoginEphemeral
            AuthInvalidUser        -> throwE LoginFailed
    newAccess uid typ label
