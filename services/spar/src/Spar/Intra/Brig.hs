{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Spar talking to Brig.
module Spar.Intra.Brig where

-- TODO: when creating user, we need to be able to provide more
-- master data (first name, last name, ...)

import Bilge
import Brig.Types.User
import Brig.Types.User.Auth (SsoLogin(..))
import Control.Monad.Except
import Data.Aeson (FromJSON, eitherDecode')
import Data.ByteString.Conversion
import Data.Id (Id(Id), UserId, TeamId)
import Data.String.Conversions
import GHC.Stack
import Lens.Micro
import Network.HTTP.Types.Method
import Spar.Error
import URI.ByteString
import Web.Cookie

import qualified SAML2.WebSSO as SAML


----------------------------------------------------------------------

toUserSSOId :: SAML.UserRef -> UserSSOId
toUserSSOId (SAML.UserRef tenant subject) =
  UserSSOId (cs $ SAML.encodeElem tenant) (cs $ SAML.encodeElem subject)

fromUserSSOId :: MonadError String m => UserSSOId -> m SAML.UserRef
fromUserSSOId (UserSSOId (cs -> tenant) (cs -> subject)) =
  case (SAML.decodeElem tenant, SAML.decodeElem subject) of
    (Right t, Right s) -> pure $ SAML.UserRef t s
    (Left msg, _)      -> throwError msg
    (_, Left msg)      -> throwError msg


parseResponse :: (FromJSON a, MonadError SparError m) => Response (Maybe LBS) -> m a
parseResponse resp = do
    bdy <- maybe (throwSpar SparNoBodyInBrigResponse) pure $ responseBody resp
    either (const $ throwSpar SparCouldNotParseBrigResponse) pure $ eitherDecode' bdy

-- | Similar to 'Network.Wire.Client.API.Auth.tokenResponse', but easier: we just need to set the
-- cookie in the response, and the redirect will make the client negotiate a fresh auth token.
-- (This is the easiest way, since the login-request that we are in the middle of responding to here
-- is not from the wire client, but from a browser that is still processing a redirect from the
-- IdP.)
respToCookie :: (HasCallStack, MonadError SparError m) => Response (Maybe LBS) -> m SetCookie
respToCookie resp = do
  let crash = throwSpar SparCouldNotRetrieveCookie
  unless (statusCode resp == 200) crash
  maybe crash (pure . parseSetCookie) $ getHeader "Set-Cookie" resp


----------------------------------------------------------------------

class Monad m => MonadSparToBrig m where
  call :: (Request -> Request) -> m (Response (Maybe LBS))


-- | Create a user on brig.
createUser :: (HasCallStack, MonadError SparError m, MonadSparToBrig m) => SAML.UserRef -> UserId -> TeamId -> m UserId
createUser suid (Id buid) teamid = do
  let newUser :: NewUser
      newUser = NewUser
        { newUserName           = Name . cs . SAML.encodeElem $ suid ^. SAML.uidSubject
        , newUserUUID           = Just buid
        , newUserIdentity       = Just $ SSOIdentity (toUserSSOId suid) Nothing Nothing
        , newUserPict           = Nothing
        , newUserAssets         = []
        , newUserAccentId       = Nothing
        , newUserEmailCode      = Nothing
        , newUserPhoneCode      = Nothing
        , newUserOrigin         = Just . NewUserOriginTeamUser . NewTeamMemberSSO $ teamid
        , newUserLabel          = Nothing
        , newUserLocale         = Nothing
        , newUserPassword       = Nothing
        , newUserExpiresIn      = Nothing
        }

  resp :: Response (Maybe LBS) <- call
    $ method POST
    . path "/i/users"
    . json newUser
    . expect2xx
  parseResponse resp


getUser :: (HasCallStack, MonadError SparError m, MonadSparToBrig m) => UserId -> m (Maybe User)
getUser buid = do
  resp :: Response (Maybe LBS) <- call
    $ method GET
    . path "/self"
    . header "Z-User" (toByteString' buid)

  if statusCode resp /= 200
    then pure Nothing
    else Just . selfUser <$> parseResponse @SelfProfile resp


-- | Check that a user locally created on spar exists on brig and has a team id.
confirmUserId :: (HasCallStack, MonadError SparError m, MonadSparToBrig m) => UserId -> m (Maybe UserId)
confirmUserId buid = do
  usr <- getUser buid
  maybe (pure Nothing) (const . pure . Just $ buid) (userTeam =<< usr)


-- | If user is not in team, throw 'SparNotInTeam'; if user is in team but not owner, throw
-- 'SparNotTeamOwner'; otherwise, return.
assertIsTeamOwner :: (HasCallStack, MonadError SparError m, MonadSparToBrig m) => UserId -> TeamId -> m ()
assertIsTeamOwner buid tid = do
  self <- maybe (throwSpar SparNotInTeam) pure =<< getUser buid
  when (userTeam self /= Just tid) $ (throwSpar SparNotInTeam)
  resp :: Response (Maybe LBS) <- call
    $ method GET
    . paths ["i", "users", toByteString' buid, "is-team-owner", toByteString' tid]
  when (statusCode resp >= 400) $ throwSpar SparNotTeamOwner


-- | Get session token from brig and redirect user past login process.
forwardBrigLogin :: (HasCallStack, MonadError SparError m, SAML.HasConfig m, MonadSparToBrig m)
                 => UserId -> m (SetCookie, URI)
forwardBrigLogin buid = do
  resp :: Response (Maybe LBS) <- call
    $ method POST
    . path "/i/sso-login"
    . json (SsoLogin buid Nothing)
    . queryItem "persistent" "true"
    . expect2xx

  (,) <$> respToCookie resp <*> SAML.getLandingURI
