{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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
module Spar.Intra.BrigApp
  ( veidToUserSSOId,
    urefToExternalId,
    urefToEmail,
    veidFromBrigUser,
    veidFromUserSSOId,
    mkUserName,
    renderValidExternalId,
    emailFromSAML,
    emailToSAML,
    emailToSAMLNameID,
    emailFromSAMLNameID,
    HavePendingInvitations (..),
    getBrigUser,
    getBrigUserTeam,
    getZUsrCheckPerm,
    authorizeScimTokenManagement,
    parseResponse,
    giveDefaultHandle,
  )
where

import Brig.Types.Intra
import Brig.Types.User
import Control.Lens
import Control.Monad.Except
import Data.ByteString.Conversion
import qualified Data.CaseInsensitive as CI
import Data.Handle (Handle (Handle))
import Data.Id (TeamId, UserId)
import Data.String.Conversions
import Galley.Types.Teams (HiddenPerm (CreateReadDeleteScimToken), IsPerm)
import Imports
import Polysemy
import Polysemy.Error
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Types.Email as SAMLEmail
import Spar.Error
import Spar.Sem.BrigAccess (BrigAccess)
import qualified Spar.Sem.BrigAccess as BrigAccess
import Spar.Sem.GalleyAccess (GalleyAccess)
import qualified Spar.Sem.GalleyAccess as GalleyAccess
import Wire.API.User
import Wire.API.User.Scim (ValidExternalId (..), runValidExternalId)

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
urefToExternalId = fmap CI.original . SAML.shortShowNameID . view SAML.uidSubject

urefToEmail :: SAML.UserRef -> Maybe Email
urefToEmail uref = case uref ^. SAML.uidSubject . SAML.nameID of
  SAML.UNameIDEmail email -> Just . emailFromSAML . CI.original $ email
  _ -> Nothing

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
    (\uref -> mkName (CI.original . SAML.unsafeShowNameID $ uref ^. SAML.uidSubject))
    (\email -> mkName (fromEmail email))

renderValidExternalId :: ValidExternalId -> Maybe Text
renderValidExternalId = runValidExternalId urefToExternalId (Just . fromEmail)

emailFromSAML :: HasCallStack => SAMLEmail.Email -> Email
emailFromSAML = fromJust . parseEmail . SAMLEmail.render

emailToSAML :: HasCallStack => Email -> SAMLEmail.Email
emailToSAML = CI.original . fromRight (error "emailToSAML") . SAMLEmail.validate . toByteString

-- | FUTUREWORK(fisx): if saml2-web-sso exported the 'NameID' constructor, we could make this
-- function total without all that praying and hoping.
emailToSAMLNameID :: HasCallStack => Email -> SAML.NameID
emailToSAMLNameID = fromRight (error "impossible") . SAML.emailNameID . fromEmail

emailFromSAMLNameID :: HasCallStack => SAML.NameID -> Maybe Email
emailFromSAMLNameID nid = case nid ^. SAML.nameID of
  SAML.UNameIDEmail email -> Just . emailFromSAML . CI.original $ email
  _ -> Nothing

----------------------------------------------------------------------

getBrigUser :: (HasCallStack, Member BrigAccess r) => HavePendingInvitations -> UserId -> Sem r (Maybe User)
getBrigUser ifpend = (accountUser <$$>) . BrigAccess.getAccount ifpend

-- | Check that an id maps to an user on brig that is 'Active' (or optionally
-- 'PendingInvitation') and has a team id.
getBrigUserTeam :: (HasCallStack, Member BrigAccess r) => HavePendingInvitations -> UserId -> Sem r (Maybe TeamId)
getBrigUserTeam ifpend = fmap (userTeam =<<) . getBrigUser ifpend

-- | Pull team id for z-user from brig.  Check permission in galley.  Return team id.  Fail if
-- permission check fails or the user is not in status 'Active'.
getZUsrCheckPerm ::
  forall r perm.
  (HasCallStack, Members '[BrigAccess, GalleyAccess, Error SparError] r, IsPerm perm, Show perm) =>
  Maybe UserId ->
  perm ->
  Sem r TeamId
getZUsrCheckPerm Nothing _ = throw $ SAML.CustomError SparMissingZUsr
getZUsrCheckPerm (Just uid) perm = do
  getBrigUserTeam NoPendingInvitations uid
    >>= maybe
      (throw $ SAML.CustomError SparNotInTeam)
      (\teamid -> teamid <$ GalleyAccess.assertHasPermission teamid perm uid)

authorizeScimTokenManagement ::
  forall r.
  (HasCallStack, Members '[BrigAccess, GalleyAccess, Error SparError] r) =>
  Maybe UserId ->
  Sem r TeamId
authorizeScimTokenManagement Nothing = throw $ SAML.CustomError SparMissingZUsr
authorizeScimTokenManagement (Just uid) = do
  getBrigUserTeam NoPendingInvitations uid
    >>= maybe
      (throw $ SAML.CustomError SparNotInTeam)
      (\teamid -> teamid <$ GalleyAccess.assertHasPermission teamid CreateReadDeleteScimToken uid)

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
giveDefaultHandle :: (HasCallStack, Member BrigAccess r) => User -> Sem r Handle
giveDefaultHandle usr = case userHandle usr of
  Just handle -> pure handle
  Nothing -> do
    let handle = Handle . cs . toByteString' $ uid
        uid = userId usr
    BrigAccess.setHandle uid handle
    pure handle
