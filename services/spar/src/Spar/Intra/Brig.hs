{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

-- | Client functions for interacting with the Brig API.
module Spar.Intra.Brig
  ( toUserSSOId,
    fromUserSSOId,
    toExternalId,
    getBrigUser,
    getBrigUserTeam,
    getBrigUsers,
    getBrigUserByHandle,
    getBrigUserRichInfo,
    setBrigUserName,
    setBrigUserHandle,
    setBrigUserManagedBy,
    setBrigUserRichInfo,
    checkHandleAvailable,
    bindBrigUser,
    deleteBrigUser,
    createBrigUser,
    isTeamUser,
    getZUsrOwnedTeam,
    ensureReAuthorised,
    ssoLogin,
    parseResponse,
    MonadSparToBrig (..),
  )
where

-- TODO: when creating user, we need to be able to provide more
-- master data (first name, last name, ...)

import Bilge
import Brig.Types.Intra
import Brig.Types.User
import Brig.Types.User.Auth (SsoLogin (..))
import Control.Lens
import Control.Monad.Except
import Data.Aeson (FromJSON, eitherDecode')
import Data.ByteString.Conversion
import Data.Handle (Handle (fromHandle))
import Data.Id (Id (Id), TeamId, UserId)
import Data.Ix
import Data.Misc (PlainTextPassword)
import Data.Range
import Data.String.Conversions
import Imports
import Network.HTTP.Types.Method
import qualified SAML2.WebSSO as SAML
import Spar.Error
import Web.Cookie

----------------------------------------------------------------------

toUserSSOId :: SAML.UserRef -> UserSSOId
toUserSSOId (SAML.UserRef tenant subject) =
  UserSSOId (cs $ SAML.encodeElem tenant) (cs $ SAML.encodeElem subject)

fromUserSSOId :: MonadError String m => UserSSOId -> m SAML.UserRef
fromUserSSOId (UserSSOId (cs -> tenant) (cs -> subject)) =
  case (SAML.decodeElem tenant, SAML.decodeElem subject) of
    (Right t, Right s) -> pure $ SAML.UserRef t s
    (Left msg, _) -> throwError msg
    (_, Left msg) -> throwError msg

-- | Converts a brig User SSO Id into an external id
toExternalId :: MonadError SparError m => UserSSOId -> m Text
toExternalId ssoid = do
  uref <- either (throwSpar . SparCouldNotParseBrigResponse . cs) pure $ fromUserSSOId ssoid
  let subj = uref ^. SAML.uidSubject
  pure $ SAML.nameIDToST subj

parseResponse :: (FromJSON a, MonadError SparError m) => Response (Maybe LBS) -> m a
parseResponse resp = do
  bdy <- maybe (throwSpar SparNoBodyInBrigResponse) pure $ responseBody resp
  either (throwSpar . SparCouldNotParseBrigResponse . cs) pure $ eitherDecode' bdy

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

class MonadError SparError m => MonadSparToBrig m where
  call :: (Request -> Request) -> m (Response (Maybe LBS))

instance MonadSparToBrig m => MonadSparToBrig (ReaderT r m) where
  call = lift . call

-- | Create a user on brig.  User name is derived from 'SAML.UserRef'.
createBrigUser ::
  (HasCallStack, MonadSparToBrig m) =>
  -- | SSO identity
  SAML.UserRef ->
  UserId ->
  TeamId ->
  -- | User name (if 'Nothing', the subject ID will be used)
  Maybe Name ->
  -- | Who should have control over the user
  ManagedBy ->
  m UserId
createBrigUser suid (Id buid) teamid mbName managedBy = do
  uname :: Name <- case mbName of
    Just n -> pure n
    Nothing -> do
      -- 1. use 'SAML.unsafeShowNameID' to get a 'Name'.  rationale: it does not need to be
      --    unique.
      let subj = suid ^. SAML.uidSubject
          subjtxt = SAML.unsafeShowNameID subj
          muname = checked @ST @1 @128 subjtxt
          err = SparBadUserName $ "must have >= 1, <= 128 chars: " <> cs subjtxt
      case muname of
        Just uname -> pure . Name . fromRange $ uname
        Nothing -> throwSpar err
  let newUser :: NewUser
      newUser =
        NewUser
          { newUserDisplayName = uname,
            newUserUUID = Just buid,
            newUserIdentity = Just $ SSOIdentity (toUserSSOId suid) Nothing Nothing,
            newUserPict = Nothing,
            newUserAssets = [],
            newUserAccentId = Nothing,
            newUserEmailCode = Nothing,
            newUserPhoneCode = Nothing,
            newUserOrigin = Just . NewUserOriginTeamUser . NewTeamMemberSSO $ teamid,
            newUserLabel = Nothing,
            newUserLocale = Nothing,
            newUserPassword = Nothing,
            newUserExpiresIn = Nothing,
            newUserManagedBy = Just managedBy
          }
  resp :: Response (Maybe LBS) <-
    call $
      method POST
        . path "/i/users"
        . json newUser
  let sCode = statusCode resp
  if  | sCode < 300 ->
        userId . selfUser <$> parseResponse @SelfProfile resp
      | inRange (400, 499) sCode ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "create user failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "create user failed with status " <> show sCode

-- | Get a user; returns 'Nothing' if the user was not found or has been deleted.
getBrigUser :: (HasCallStack, MonadSparToBrig m) => UserId -> m (Maybe User)
getBrigUser buid = do
  resp :: Response (Maybe LBS) <-
    call $
      method GET
        . path "/self"
        . header "Z-User" (toByteString' buid)
  case statusCode resp of
    200 -> do
      user <- selfUser <$> parseResponse @SelfProfile resp
      pure $
        if (userDeleted user)
          then Nothing
          else Just user
    404 -> pure Nothing
    _ -> throwSpar (SparBrigError "Could not retrieve user")

-- | Get a list of users; returns a shorter list if some 'UserId's come up empty (no errors).
--
-- TODO: implement an internal end-point on brig that makes this possible with one request.
-- TODO(arianvp): This endpoint exists!
getBrigUsers :: (HasCallStack, MonadSparToBrig m) => [UserId] -> m [User]
getBrigUsers = fmap catMaybes . mapM getBrigUser

-- | Get a user; returns 'Nothing' if the user was not found.
--
-- TODO: currently this is not used, but it might be useful later when/if
-- @hscim@ stops doing checks during user creation.
getBrigUserByHandle :: (HasCallStack, MonadSparToBrig m) => Handle -> m (Maybe User)
getBrigUserByHandle handle = do
  resp :: Response (Maybe LBS) <-
    call $
      method GET
        . path "/i/users"
        . queryItem "handles" (toByteString' handle)
  -- This returns [UserAccount]
  case statusCode resp of
    200 -> parse <$> parseResponse @[UserAccount] resp
    404 -> pure Nothing
    _ -> throwSpar (SparBrigError "Could not retrieve user")
  where
    parse :: [UserAccount] -> Maybe User
    parse (x : []) = Just $ accountUser x
    parse _ = Nothing -- TODO: What if more accounts get returned?

-- | Set user' name.  Fails with status <500 if brig fails with <500, and with 500 if brig
-- fails with >= 500.
setBrigUserName :: (HasCallStack, MonadSparToBrig m) => UserId -> Name -> m ()
setBrigUserName buid name = do
  resp <-
    call $
      method PUT
        . path "/self"
        . header "Z-User" (toByteString' buid)
        . header "Z-Connection" ""
        . json
          UserUpdate
            { uupName = Just name,
              uupPict = Nothing,
              uupAssets = Nothing,
              uupAccentId = Nothing
            }
  let sCode = statusCode resp
  if  | sCode < 300 ->
        pure ()
      | inRange (400, 499) sCode ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "set name failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "set name failed with status " <> show sCode

-- | Set user's handle.  Fails with status <500 if brig fails with <500, and with 500 if brig fails
-- with >= 500.
setBrigUserHandle :: (HasCallStack, MonadSparToBrig m) => UserId -> Handle -> m ()
setBrigUserHandle buid handle = do
  resp <-
    call $
      method PUT
        . path "/self/handle"
        . header "Z-User" (toByteString' buid)
        . header "Z-Connection" ""
        . json (HandleUpdate (fromHandle handle))
  let sCode = statusCode resp
  if  | sCode < 300 ->
        pure ()
      | inRange (400, 499) sCode ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "set handle failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "set handle failed with status " <> show sCode

-- | Set user's managedBy. Fails with status <500 if brig fails with <500, and with 500 if
-- brig fails with >= 500.
setBrigUserManagedBy :: (HasCallStack, MonadSparToBrig m) => UserId -> ManagedBy -> m ()
setBrigUserManagedBy buid managedBy = do
  resp <-
    call $
      method PUT
        . paths ["i", "users", toByteString' buid, "managed-by"]
        . json (ManagedByUpdate managedBy)
  let sCode = statusCode resp
  if  | sCode < 300 ->
        pure ()
      | inRange (400, 499) sCode ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "set managedBy failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "set managedBy failed with status " <> show sCode

-- | Set user's richInfo. Fails with status <500 if brig fails with <500, and with 500 if
-- brig fails with >= 500.
setBrigUserRichInfo :: (HasCallStack, MonadSparToBrig m) => UserId -> RichInfo -> m ()
setBrigUserRichInfo buid richInfo = do
  resp <-
    call $
      method PUT
        . paths ["i", "users", toByteString' buid, "rich-info"]
        . json (RichInfoUpdate $ toRichInfoAssocList richInfo)
  let sCode = statusCode resp
  if  | sCode < 300 ->
        pure ()
      | inRange (400, 499) sCode ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "set richInfo failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "set richInfo failed with status " <> show sCode

-- TODO: We should add an internal endpoint for this instead
getBrigUserRichInfo :: (HasCallStack, MonadSparToBrig m) => UserId -> m RichInfo
getBrigUserRichInfo buid = do
  resp <-
    call $
      method GET
        . paths ["users", toByteString' buid, "rich-info"]
        . header "Z-User" (toByteString' buid)
        . header "Z-Connection" ""
  case statusCode resp of
    200 -> parseResponse @RichInfo resp
    _ -> throwSpar (SparBrigErrorWith (responseStatus resp) "Could not retrieve rich info")

-- | At the time of writing this, @HEAD /users/handles/:uid@ does not use the 'UserId' for
-- anything but authorization.
checkHandleAvailable :: (HasCallStack, MonadSparToBrig m) => Handle -> UserId -> m Bool
checkHandleAvailable hnd buid = do
  resp <-
    call $
      method HEAD
        . paths ["users", "handles", toByteString' hnd]
        . header "Z-User" (toByteString' buid)
        . header "Z-Connection" ""
  let sCode = statusCode resp
  if  | sCode == 200 -> -- handle exists
        pure False
      | sCode == 404 -> -- handle not found
        pure True
      | sCode < 500 ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "check handle failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "check handle failed with status " <> show sCode

-- | This works under the assumption that the user must exist on brig.  If it does not, brig
-- responds with 404 and this function returns 'False'.
bindBrigUser :: (HasCallStack, MonadSparToBrig m) => UserId -> SAML.UserRef -> m Bool
bindBrigUser uid (toUserSSOId -> ussoid) = do
  resp <-
    call $
      method PUT
        . paths ["/i/users", toByteString' uid, "sso-id"]
        . json ussoid
  pure $ Bilge.statusCode resp < 300

-- | Call brig to delete a user
deleteBrigUser :: (HasCallStack, MonadSparToBrig m, MonadIO m) => UserId -> m ()
deleteBrigUser buid = do
  resp :: Response (Maybe LBS) <-
    call $
      method DELETE
        . paths ["/i/users", toByteString' buid]
  let sCode = statusCode resp
  if  | sCode < 300 -> pure ()
      | inRange (400, 499) sCode ->
        throwSpar $ SparBrigErrorWith (responseStatus resp) "failed to delete user"
      | otherwise ->
        throwSpar $ SparBrigError ("delete user failed with status " <> cs (show sCode))

-- | Check that a user id exists on brig and has a team id.
isTeamUser :: (HasCallStack, MonadSparToBrig m) => UserId -> m Bool
isTeamUser buid = isJust <$> getBrigUserTeam buid

-- | Check that a user id exists on brig and has a team id.
getBrigUserTeam :: (HasCallStack, MonadSparToBrig m) => UserId -> m (Maybe TeamId)
getBrigUserTeam buid = do
  usr <- getBrigUser buid
  pure $ userTeam =<< usr

-- | If user is not in team, throw 'SparNotInTeam'; if user is in team but not owner, throw
-- 'SparNotTeamOwner'; otherwise, return.
assertIsTeamOwner :: (HasCallStack, MonadSparToBrig m) => UserId -> TeamId -> m ()
assertIsTeamOwner buid tid = do
  self <- maybe (throwSpar SparNotInTeam) pure =<< getBrigUser buid
  when (userTeam self /= Just tid) $ (throwSpar SparNotInTeam)
  resp :: Response (Maybe LBS) <-
    call $
      method GET
        . paths ["i", "users", toByteString' buid, "is-team-owner", toByteString' tid]
  when (statusCode resp >= 400) $ throwSpar SparNotTeamOwner

-- | Get the team that the user is an owner of.
--
-- Called by post handler, and by 'authorizeIdP'.
getZUsrOwnedTeam ::
  (HasCallStack, SAML.SP m, MonadSparToBrig m) =>
  Maybe UserId ->
  m TeamId
getZUsrOwnedTeam Nothing = throwSpar SparMissingZUsr
getZUsrOwnedTeam (Just uid) = do
  usr <- getBrigUser uid
  case userTeam =<< usr of
    Nothing -> throwSpar SparNotInTeam
    Just teamid -> teamid <$ assertIsTeamOwner uid teamid

-- | Verify user's password (needed for certain powerful operations).
ensureReAuthorised ::
  (HasCallStack, MonadSparToBrig m) =>
  Maybe UserId ->
  Maybe PlainTextPassword ->
  m ()
ensureReAuthorised Nothing _ = throwSpar SparMissingZUsr
ensureReAuthorised (Just uid) secret = do
  resp <-
    call $
      method GET
        . paths ["/i/users", toByteString' uid, "reauthenticate"]
        . json (ReAuthUser secret)
  let sCode = statusCode resp
  if  | sCode == 200 ->
        pure ()
      | sCode == 403 ->
        throwSpar SparReAuthRequired
      | inRange (400, 499) sCode ->
        throwSpar . SparBrigErrorWith (responseStatus resp) $ "reauthentication failed"
      | otherwise ->
        throwSpar . SparBrigError . cs $ "reauthentication failed with status " <> show sCode

-- | Get persistent cookie from brig and redirect user past login process.
--
-- If brig responds with status >=400;<500, return Nothing.  Otherwise, crash (500).
ssoLogin ::
  (HasCallStack, SAML.HasConfig m, MonadSparToBrig m) =>
  UserId ->
  m (Maybe SetCookie)
ssoLogin buid = do
  resp :: Response (Maybe LBS) <-
    call $
      method POST
        . path "/i/sso-login"
        . json (SsoLogin buid Nothing)
        . queryItem "persist" "true"
  let sCode = statusCode resp
  if  | sCode < 300 ->
        Just <$> respToCookie resp
      | inRange (400, 499) sCode ->
        pure Nothing
      | otherwise ->
        throwSpar . SparBrigError . cs $ "sso-login failed with status " <> show sCode
