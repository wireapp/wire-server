{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

-- | Client functions for interacting with the Brig API.
module Spar.Intra.Brig
  ( MonadSparToBrig (..),
    getBrigUserAccount,
    getBrigUserByHandle,
    getBrigUserByEmail,
    getBrigUserRichInfo,
    setBrigUserName,
    setBrigUserHandle,
    setBrigUserManagedBy,
    setBrigUserVeid,
    setBrigUserRichInfo,
    checkHandleAvailable,
    deleteBrigUser,
    createBrigUserSAML,
    createBrigUserNoSAML,
    updateEmail,
    ensureReAuthorised,
    ssoLogin,
    getStatus,
    getStatusMaybe,
    setStatus,
  )
where

import Bilge
import Brig.Types.Intra
import Brig.Types.User
import Brig.Types.User.Auth (SsoLogin (..))
import Control.Monad.Except
import Data.ByteString.Conversion
import Data.Code as Code
import Data.Handle (Handle (fromHandle))
import Data.Id (Id (Id), TeamId, UserId)
import Data.Misc (PlainTextPassword)
import qualified Data.Text.Lazy as Lazy
import Imports
import Network.HTTP.Types.Method
import qualified Network.Wai.Utilities.Error as Wai
import qualified SAML2.WebSSO as SAML
import Spar.Error
import qualified System.Logger.Class as Log
import Web.Cookie
import Wire.API.User
import Wire.API.User.RichInfo as RichInfo
import Wire.API.User.Scim (ValidExternalId (..), runValidExternalId)

----------------------------------------------------------------------

-- | FUTUREWORK: this is redundantly defined in "Spar.Intra.BrigApp".
veidToUserSSOId :: ValidExternalId -> UserSSOId
veidToUserSSOId = runValidExternalId UserSSOId (UserScimExternalId . fromEmail)

-- | Similar to 'Network.Wire.Client.API.Auth.tokenResponse', but easier: we just need to set the
-- cookie in the response, and the redirect will make the client negotiate a fresh auth token.
-- (This is the easiest way, since the login-request that we are in the middle of responding to here
-- is not from the wire client, but from a browser that is still processing a redirect from the
-- IdP.)
respToCookie :: (HasCallStack, MonadError SparError m) => ResponseLBS -> m SetCookie
respToCookie resp = do
  let crash = throwSpar SparCouldNotRetrieveCookie
  unless (statusCode resp == 200) crash
  maybe crash (pure . parseSetCookie) $ getHeader "Set-Cookie" resp

----------------------------------------------------------------------

class (Log.MonadLogger m, MonadError SparError m) => MonadSparToBrig m where
  call :: (Request -> Request) -> m ResponseLBS

createBrigUserSAML ::
  (HasCallStack, MonadSparToBrig m) =>
  SAML.UserRef ->
  UserId ->
  TeamId ->
  -- | User name
  Name ->
  -- | Who should have control over the user
  ManagedBy ->
  m UserId
createBrigUserSAML uref (Id buid) teamid uname managedBy = do
  let newUser :: NewUser
      newUser =
        (emptyNewUser uname)
          { newUserUUID = Just buid,
            newUserIdentity = Just (SSOIdentity (UserSSOId uref) Nothing Nothing),
            newUserOrigin = Just (NewUserOriginTeamUser . NewTeamMemberSSO $ teamid),
            newUserManagedBy = Just managedBy
          }
  resp :: ResponseLBS <-
    call $
      method POST
        . path "/i/users"
        . json newUser
  if statusCode resp `elem` [200, 201]
    then userId . selfUser <$> parseResponse @SelfProfile "brig" resp
    else rethrow "brig" resp

createBrigUserNoSAML ::
  (HasCallStack, MonadSparToBrig m) =>
  Email ->
  TeamId ->
  -- | User name
  Name ->
  m UserId
createBrigUserNoSAML email teamid uname = do
  let newUser = NewUserScimInvitation teamid Nothing uname email
  resp :: ResponseLBS <-
    call $
      method POST
        . paths ["/i/teams", toByteString' teamid, "invitations"]
        . json newUser

  if statusCode resp `elem` [200, 201]
    then userId . accountUser <$> parseResponse @UserAccount "brig" resp
    else rethrow "brig" resp

updateEmail :: (HasCallStack, MonadSparToBrig m) => UserId -> Email -> m ()
updateEmail buid email = do
  resp <-
    call $
      method PUT
        . path "/i/self/email"
        . header "Z-User" (toByteString' buid)
        . query [("validate", Just "true")]
        . json (EmailUpdate email)
  case statusCode resp of
    204 -> pure ()
    202 -> pure ()
    _ -> rethrow "brig" resp

-- | Get a user; returns 'Nothing' if the user was not found or has been deleted.
getBrigUserAccount :: (HasCallStack, MonadSparToBrig m) => HavePendingInvitations -> UserId -> m (Maybe UserAccount)
getBrigUserAccount havePending buid = do
  resp :: ResponseLBS <-
    call $
      method GET
        . paths ["/i/users"]
        . query
          [ ("ids", Just $ toByteString' buid),
            ( "includePendingInvitations",
              Just . toByteString' $
                case havePending of
                  WithPendingInvitations -> True
                  NoPendingInvitations -> False
            )
          ]

  case statusCode resp of
    200 -> do
      parseResponse @[UserAccount] "brig" resp >>= \case
        [account] ->
          pure $
            if userDeleted $ accountUser account
              then Nothing
              else Just account
        _ -> pure Nothing
    404 -> pure Nothing
    _ -> rethrow "brig" resp

-- | Get a user; returns 'Nothing' if the user was not found.
--
-- TODO: currently this is not used, but it might be useful later when/if
-- @hscim@ stops doing checks during user creation.
getBrigUserByHandle :: (HasCallStack, MonadSparToBrig m) => Handle -> m (Maybe UserAccount)
getBrigUserByHandle handle = do
  resp :: ResponseLBS <-
    call $
      method GET
        . path "/i/users"
        . queryItem "handles" (toByteString' handle)
        . queryItem "includePendingInvitations" "true"
  case statusCode resp of
    200 -> listToMaybe <$> parseResponse @[UserAccount] "brig" resp
    404 -> pure Nothing
    _ -> rethrow "brig" resp

getBrigUserByEmail :: (HasCallStack, MonadSparToBrig m) => Email -> m (Maybe UserAccount)
getBrigUserByEmail email = do
  resp :: ResponseLBS <-
    call $
      method GET
        . path "/i/users"
        . queryItem "email" (toByteString' email)
        . queryItem "includePendingInvitations" "true"
  case statusCode resp of
    200 -> do
      macc <- listToMaybe <$> parseResponse @[UserAccount] "brig" resp
      case userEmail . accountUser =<< macc of
        Just email' | email' == email -> pure macc
        _ -> pure Nothing
    404 -> pure Nothing
    _ -> rethrow "brig" resp

-- | Set user' name.  Fails with status <500 if brig fails with <500, and with 500 if brig
-- fails with >= 500.
setBrigUserName :: (HasCallStack, MonadSparToBrig m) => UserId -> Name -> m ()
setBrigUserName buid (Name name) = do
  resp <-
    call $
      method PUT
        . paths ["/i/users", toByteString' buid, "name"]
        . json (NameUpdate name)
  let sCode = statusCode resp
  if
      | sCode < 300 ->
        pure ()
      | otherwise ->
        rethrow "brig" resp

-- | Set user's handle.  Fails with status <500 if brig fails with <500, and with 500 if brig fails
-- with >= 500.
--
-- NB: that this doesn't take a 'HandleUpdate', since we already construct a valid handle in
-- 'validateScimUser' to increase the odds that user creation doesn't fail half-way through
-- the many database write operations.
setBrigUserHandle :: (HasCallStack, MonadSparToBrig m) => UserId -> Handle {- not 'HandleUpdate'! -} -> m ()
setBrigUserHandle buid handle = do
  resp <-
    call $
      method PUT
        . paths ["/i/users", toByteString' buid, "handle"]
        . json (HandleUpdate (fromHandle handle))
  case (statusCode resp, Wai.label <$> responseJsonMaybe @Wai.Error resp) of
    (200, Nothing) -> do
      pure ()
    _ -> do
      rethrow "brig" resp

-- | Set user's managedBy. Fails with status <500 if brig fails with <500, and with 500 if
-- brig fails with >= 500.
setBrigUserManagedBy :: (HasCallStack, MonadSparToBrig m) => UserId -> ManagedBy -> m ()
setBrigUserManagedBy buid managedBy = do
  resp <-
    call $
      method PUT
        . paths ["/i/users", toByteString' buid, "managed-by"]
        . json (ManagedByUpdate managedBy)
  unless (statusCode resp == 200) $
    rethrow "brig" resp

-- | Set user's UserSSOId.
setBrigUserVeid :: (HasCallStack, MonadSparToBrig m) => UserId -> ValidExternalId -> m ()
setBrigUserVeid buid veid = do
  resp <-
    call $
      method PUT
        . paths ["i", "users", toByteString' buid, "sso-id"]
        . json (veidToUserSSOId veid)
  case statusCode resp of
    200 -> pure ()
    _ -> rethrow "brig" resp

-- | Set user's richInfo. Fails with status <500 if brig fails with <500, and with 500 if
-- brig fails with >= 500.
setBrigUserRichInfo :: (HasCallStack, MonadSparToBrig m) => UserId -> RichInfo -> m ()
setBrigUserRichInfo buid richInfo = do
  resp <-
    call $
      method PUT
        . paths ["i", "users", toByteString' buid, "rich-info"]
        . json (RichInfoUpdate $ unRichInfo richInfo)
  unless (statusCode resp == 200) $
    rethrow "brig" resp

getBrigUserRichInfo :: (HasCallStack, MonadSparToBrig m) => UserId -> m RichInfo
getBrigUserRichInfo buid = do
  resp <-
    call $
      method GET
        . paths ["/i/users", toByteString' buid, "rich-info"]
  case statusCode resp of
    200 -> parseResponse "brig" resp
    _ -> rethrow "brig" resp

checkHandleAvailable :: (HasCallStack, MonadSparToBrig m) => Handle -> m Bool
checkHandleAvailable hnd = do
  resp <-
    call $
      method HEAD
        . paths ["/i/users/handles", toByteString' hnd]
  let sCode = statusCode resp
  if
      | sCode == 200 -> -- handle exists
        pure False
      | sCode == 404 -> -- handle not found
        pure True
      | otherwise ->
        rethrow "brig" resp

-- | Call brig to delete a user
deleteBrigUser :: (HasCallStack, MonadSparToBrig m, MonadIO m) => UserId -> m ()
deleteBrigUser buid = do
  resp :: ResponseLBS <-
    call $
      method DELETE
        . paths ["/i/users", toByteString' buid]
  unless (statusCode resp == 202) $
    rethrow "brig" resp

-- | Verify user's password (needed for certain powerful operations).
ensureReAuthorised ::
  (HasCallStack, MonadSparToBrig m) =>
  Maybe UserId ->
  Maybe PlainTextPassword ->
  Maybe Code.Value ->
  Maybe VerificationAction ->
  m ()
ensureReAuthorised Nothing _ _ _ = throwSpar SparMissingZUsr
ensureReAuthorised (Just uid) secret mbCode mbAction = do
  resp <-
    call $
      method GET
        . paths ["/i/users", toByteString' uid, "reauthenticate"]
        . json (ReAuthUser secret mbCode mbAction)
  case (statusCode resp, errorLabel resp) of
    (200, _) -> pure ()
    (403, Just "code-authentication-required") -> throwSpar SparReAuthCodeAuthRequired
    (403, Just "code-authentication-failed") -> throwSpar SparReAuthCodeAuthFailed
    (403, _) -> throwSpar SparReAuthRequired
    (_, _) -> rethrow "brig" resp
  where
    errorLabel :: ResponseLBS -> Maybe Lazy.Text
    errorLabel = fmap Wai.label . responseJsonMaybe

-- | Get persistent cookie from brig and redirect user past login process.
--
-- If brig responds with status >=400;<500, return Nothing.  Otherwise, crash (500).
ssoLogin ::
  (HasCallStack, MonadSparToBrig m) =>
  UserId ->
  m SetCookie
ssoLogin buid = do
  resp :: ResponseLBS <-
    call $
      method POST
        . path "/i/sso-login"
        . json (SsoLogin buid Nothing)
        . queryItem "persist" "true"
  if (statusCode resp == 200)
    then respToCookie resp
    else rethrow "brig" resp

getStatus' :: (HasCallStack, MonadSparToBrig m) => UserId -> m ResponseLBS
getStatus' uid = call $ method GET . paths ["/i/users", toByteString' uid, "status"]

-- | FUTUREWORK: this is probably unnecessary, and we can get the status info from 'UserAccount'.
getStatus :: (HasCallStack, MonadSparToBrig m) => UserId -> m AccountStatus
getStatus uid = do
  resp <- getStatus' uid
  case statusCode resp of
    200 -> fromAccountStatusResp <$> parseResponse @AccountStatusResp "brig" resp
    _ -> rethrow "brig" resp

-- | FUTUREWORK: this is probably unnecessary, and we can get the status info from 'UserAccount'.
getStatusMaybe :: (HasCallStack, MonadSparToBrig m) => UserId -> m (Maybe AccountStatus)
getStatusMaybe uid = do
  resp <- getStatus' uid
  case statusCode resp of
    200 -> Just . fromAccountStatusResp <$> parseResponse @AccountStatusResp "brig" resp
    404 -> pure Nothing
    _ -> rethrow "brig" resp

setStatus :: (HasCallStack, MonadSparToBrig m) => UserId -> AccountStatus -> m ()
setStatus uid status = do
  resp <-
    call $
      method PUT
        . paths ["/i/users", toByteString' uid, "status"]
        . json (AccountStatusUpdate status)
  case statusCode resp of
    200 -> pure ()
    _ -> rethrow "brig" resp
