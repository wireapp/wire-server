-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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
module Spar.Intra.BrigApp
  ( veidToUserSSOId,
    urefToExternalId,
    urefToEmail,
    veidFromBrigUser,
    veidFromUserSSOId,
    mkUserName,
    renderValidScimId,
    HavePendingInvitations (..),
    getBrigUser,
    getBrigUserTeam,
    getZUsrCheckPerm,
    authorizeScimTokenManagement,
    parseResponse,
    giveDefaultHandle,

    -- * re-exports, mostly for historical reasons and lazyness
    emailFromSAML,
    emailToSAMLNameID,
    emailFromSAMLNameID,
  )
where

import Brig.Types.Intra
import Brig.Types.User
import Control.Lens
import Control.Monad.Except
import Data.ByteString.Conversion
import qualified Data.CaseInsensitive as CI
import Data.Handle (Handle, parseHandle)
import Data.Id (TeamId, UserId)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Imports
import Polysemy
import Polysemy.Error
import qualified SAML2.WebSSO as SAML
import Spar.Error
import Spar.Sem.BrigAccess (BrigAccess)
import qualified Spar.Sem.BrigAccess as BrigAccess
import Spar.Sem.GalleyAccess (GalleyAccess)
import qualified Spar.Sem.GalleyAccess as GalleyAccess
import Wire.API.Team.Member (HiddenPerm (CreateReadDeleteScimToken), IsPerm)
import Wire.API.User
import Wire.API.User.Scim (ValidScimId (..), runValidScimIdEither)

----------------------------------------------------------------------

-- | If the brig user has a 'UserSSOId', transform that into a 'ValidScimId' (this is a
-- total function as long as brig obeys the api).  Otherwise, if the user has an email, we can
-- construct a return value from that (and an optional saml issuer).
--
-- Note: the saml issuer is only needed in the case where a user has been invited via team
-- settings and is now onboarded to saml/scim.  If this case can safely be ruled out, it's ok
-- to just set it to 'Nothing'.
veidFromBrigUser :: (MonadError String m) => User -> Maybe SAML.Issuer -> m ValidScimId
veidFromBrigUser usr mIssuer = case (userSSOId usr, userEmail usr, mIssuer) of
  (Just ssoid, _, _) -> veidFromUserSSOId ssoid
  (Nothing, Just email, Just issuer) -> pure $ EmailAndUref email (SAML.UserRef issuer (fromRight' $ emailToSAMLNameID email))
  (Nothing, Just email, Nothing) -> pure $ EmailOnly email
  (Nothing, Nothing, _) -> throwError "user has neither ssoIdentity nor userEmail"

-- | Take a maybe text, construct a 'Name' from what we have in a scim user.  If the text
-- isn't present, use an email address or a saml subject (usually also an email address).  If
-- both are 'Nothing', fail.
mkUserName :: Maybe Text -> ValidScimId -> Either String Name
mkUserName (Just n) = const $ mkName n
mkUserName Nothing =
  runValidScimIdEither
    (\uref -> mkName (CI.original . SAML.unsafeShowNameID $ uref ^. SAML.uidSubject))
    (mkName . fromEmail)

renderValidScimId :: ValidScimId -> Maybe Text
renderValidScimId = runValidScimIdEither urefToExternalId (Just . fromEmail)

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
  ( HasCallStack,
    ( Member BrigAccess r,
      Member GalleyAccess r,
      Member (Error SparError) r
    ),
    IsPerm perm,
    Show perm
  ) =>
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
  ( HasCallStack,
    ( Member BrigAccess r,
      Member GalleyAccess r,
      Member (Error SparError) r
    )
  ) =>
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
    let handle = fromJust . parseHandle . decodeUtf8With lenientDecode . toByteString' $ uid
        uid = userId usr
    BrigAccess.setHandle uid handle
    pure handle
