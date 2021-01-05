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
  ( veidToUserSSOId,
    veidFromUserSSOId,
    urefToExternalId,
    urefToEmail,
    userToExternalId,
    veidFromBrigUser,
    mkUserName,
    renderValidExternalId,
    emailFromSAML,
    emailToSAML,
    emailToSAMLNameID,
    emailFromSAMLNameID,
    getBrigUserAccount,
    HavePendingInvitations (..),
    getBrigUser,
    getBrigUserTeam,
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
    getZUsrOwnedTeam,
    authorizeScimTokenManagement,
    ensureReAuthorised,
    ssoLogin,
    parseResponse,
    MonadSparToBrig (..),
    getStatus,
    getStatusMaybe,
    setStatus,
    giveDefaultHandle,
  )
where

import Bilge
import Brig.Types.Intra
import Brig.Types.User
import Brig.Types.User.Auth (SsoLogin (..))
import Control.Lens
import Control.Monad.Except
import Data.ByteString.Conversion
import Data.Handle (Handle (Handle, fromHandle))
import Data.Id (Id (Id), TeamId, UserId)
import Data.Misc (PlainTextPassword)
import Data.String.Conversions
import Galley.Types.Teams (HiddenPerm (CreateReadDeleteScimToken))
import Imports
import Network.HTTP.Types.Method
import qualified Network.Wai.Utilities.Error as Wai
import qualified SAML2.WebSSO as SAML
import Spar.Error
import Spar.Intra.Galley as Galley (MonadSparToGalley, assertHasPermission, assertIsTeamOwner)
import Spar.Scim.Types (ValidExternalId (..), runValidExternalId)
import qualified System.Logger.Class as Log
import qualified Text.Email.Parser
import Web.Cookie
import Wire.API.User
import Wire.API.User.RichInfo as RichInfo

----------------------------------------------------------------------

veidToUserSSOId :: ValidExternalId -> UserSSOId
veidToUserSSOId = runValidExternalId urefToUserSSOId (UserScimExternalId . fromEmail)

urefToUserSSOId :: SAML.UserRef -> UserSSOId
urefToUserSSOId (SAML.UserRef t s) = UserSSOId (cs $ SAML.encodeElem t) (cs $ SAML.encodeElem s)

veidFromUserSSOId :: MonadError String m => UserSSOId -> m ValidExternalId
veidFromUserSSOId = \case
  UserSSOId tenant subject ->
    case (SAML.decodeElem $ cs tenant, SAML.decodeElem $ cs subject) of
      (Right t, Right s) -> do
        let uref = SAML.UserRef t s
        case urefToEmail uref of
          Nothing -> pure $ UrefOnly uref
          Just email -> pure $ EmailAndUref email uref
      (Left msg, _) -> throwError msg
      (_, Left msg) -> throwError msg
  UserScimExternalId email ->
    maybe
      (throwError "externalId not an email and no issuer")
      (pure . EmailOnly)
      (parseEmail email)

urefToExternalId :: SAML.UserRef -> Maybe Text
urefToExternalId = SAML.shortShowNameID . view SAML.uidSubject

urefToEmail :: SAML.UserRef -> Maybe Email
urefToEmail uref = case uref ^. SAML.uidSubject . SAML.nameID of
  SAML.UNameIDEmail email -> Just $ emailFromSAML email
  _ -> Nothing

userToExternalId :: MonadError String m => User -> m Text
userToExternalId usr =
  case veidFromUserSSOId <$> userSSOId usr of
    Nothing -> throwError "brig user without sso_id"
    Just (Left err) -> throwError err
    Just (Right veid) ->
      runValidExternalId
        (\(SAML.UserRef _ subj) -> maybe (throwError "bad uref from brig") pure $ SAML.shortShowNameID subj)
        (pure . fromEmail)
        veid

-- | If the brig user has a 'UserSSOId', transform that into a 'ValidExternalId' (this is a
-- total function as long as brig obeys the api).  Otherwise, if the user has an email, we can
-- construct a return value from that (and an optional saml issuer).  If a user only has a
-- phone number, or no identity at all, throw an error.
--
-- Note: the saml issuer is only needed in the case where a user has been invited via team
-- settings and is now onboarded to saml/scim.  If this case can safely be ruled out, it's ok
-- to just set it to 'Nothing'.
veidFromBrigUser :: MonadError String m => User -> Maybe SAML.Issuer -> m ValidExternalId
veidFromBrigUser usr mIssuer = case (userSSOId usr, userEmail usr, mIssuer) of
  (Just ssoid, _, _) -> veidFromUserSSOId ssoid
  (Nothing, Just email, Just issuer) -> pure $ EmailAndUref email (SAML.UserRef issuer (emailToSAMLNameID email))
  (Nothing, Just email, Nothing) -> pure $ EmailOnly email
  (Nothing, Nothing, _) -> throwError "user has neither ssoIdentity nor userEmail"

-- | Take a maybe text, construct a 'Name' from what we have in a scim user.  If the text
-- isn't present, use an email address or a saml subject (usually also an email address).  If
-- both are 'Nothing', fail.
mkUserName :: Maybe Text -> ValidExternalId -> Either String Name
mkUserName (Just n) = const $ mkName n
mkUserName Nothing =
  runValidExternalId
    (\uref -> mkName (SAML.unsafeShowNameID $ uref ^. SAML.uidSubject))
    (\email -> mkName (fromEmail email))

renderValidExternalId :: ValidExternalId -> Maybe Text
renderValidExternalId = runValidExternalId urefToExternalId (Just . fromEmail)

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

emailFromSAML :: HasCallStack => SAML.Email -> Email
emailFromSAML =
  fromJust . parseEmail . cs
    . Text.Email.Parser.toByteString
    . SAML.fromEmail

emailToSAML :: HasCallStack => Email -> SAML.Email
emailToSAML brigEmail =
  SAML.Email $
    Text.Email.Parser.unsafeEmailAddress
      (cs $ emailLocal brigEmail)
      (cs $ emailDomain brigEmail)

-- | FUTUREWORK(fisx): if saml2-web-sso exported the 'NameID' constructor, we could make this
-- function total without all that praying and hoping.
emailToSAMLNameID :: HasCallStack => Email -> SAML.NameID
emailToSAMLNameID = fromRight (error "impossible") . SAML.emailNameID . fromEmail

emailFromSAMLNameID :: HasCallStack => SAML.NameID -> Maybe Email
emailFromSAMLNameID nid = case nid ^. SAML.nameID of
  SAML.UNameIDEmail email -> Just $ emailFromSAML email
  _ -> Nothing

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
            newUserIdentity = Just (SSOIdentity (urefToUserSSOId uref) Nothing Nothing),
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

getBrigUser :: (HasCallStack, MonadSparToBrig m) => HavePendingInvitations -> UserId -> m (Maybe User)
getBrigUser ifpend = (accountUser <$$>) . getBrigUserAccount ifpend

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

-- | Check that an id maps to an user on brig that is 'Active' (or optionally
-- 'PendingInvitation') and has a team id.
getBrigUserTeam :: (HasCallStack, MonadSparToBrig m) => HavePendingInvitations -> UserId -> m (Maybe TeamId)
getBrigUserTeam ifpend = fmap (userTeam =<<) . getBrigUser ifpend

-- | Get the team that the user is an owner of.  This is used for authorization.  It will fail
-- if the user is not in status 'Active'.
getZUsrOwnedTeam ::
  (HasCallStack, SAML.SP m, MonadSparToBrig m, MonadSparToGalley m) =>
  Maybe UserId ->
  m TeamId
getZUsrOwnedTeam Nothing = throwSpar SparMissingZUsr
getZUsrOwnedTeam (Just uid) = do
  getBrigUserTeam NoPendingInvitations uid
    >>= maybe
      (throwSpar SparNotInTeam)
      (\teamid -> teamid <$ Galley.assertIsTeamOwner teamid uid)

authorizeScimTokenManagement :: (HasCallStack, SAML.SP m, MonadSparToBrig m, MonadSparToGalley m) => Maybe UserId -> m TeamId
authorizeScimTokenManagement Nothing = throwSpar SparMissingZUsr
authorizeScimTokenManagement (Just uid) = do
  getBrigUserTeam NoPendingInvitations uid
    >>= maybe
      (throwSpar SparNotInTeam)
      (\teamid -> teamid <$ Galley.assertHasPermission teamid CreateReadDeleteScimToken uid)

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
  if
      | sCode == 200 ->
        pure ()
      | sCode == 403 ->
        throwSpar SparReAuthRequired
      | otherwise ->
        rethrow "brig" resp

-- | Get persistent cookie from brig and redirect user past login process.
--
-- If brig responds with status >=400;<500, return Nothing.  Otherwise, crash (500).
ssoLogin ::
  (HasCallStack, SAML.HasConfig m, MonadSparToBrig m) =>
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

-- | If the user has no 'Handle', set it to its 'UserId' and update the user in brig.
-- Return the handle the user now has (the old one if it existed, the newly created one
-- otherwise).
--
-- RATIONALE: Finding the handle can fail for users that have been created without scim, and
-- have stopped the onboarding process at the point where they are asked by the client to
-- enter a handle.
--
-- We make up a handle in this case, and the scim peer can find the user, see that the handle
-- is not the one it expects, and update it.
--
-- We cannot simply respond with 404 in this case, because the user exists.  404 would suggest
-- do the scim peer that it should post the user to create it, but that would create a new
-- user instead of finding the old that should be put under scim control.
giveDefaultHandle :: (HasCallStack, MonadSparToBrig m) => User -> m Handle
giveDefaultHandle usr = case userHandle usr of
  Just handle -> pure handle
  Nothing -> do
    let handle = Handle . cs . toByteString' $ uid
        uid = userId usr
    setBrigUserHandle uid handle
    pure handle
