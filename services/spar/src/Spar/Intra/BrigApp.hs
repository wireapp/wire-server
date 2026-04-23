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
    oldVeidFromBrigUser,
    newVeidFromBrigUser,
    veidFromUserSSOId,
    mkUserName,
    HavePendingInvitations (..),
    getBrigUserTeam,
    getZUsrCheckPerm,
    authorizeScimTokenManagement,
    giveDefaultHandle,
    ensureReAuthorised,
    assertHasPermission,
    assertSSOEnabled,

    -- * re-exports, mostly for historical reasons and lazyness
    emailFromSAML,
  )
where

import Control.Lens
import Control.Monad.Except
import Data.ByteString.Conversion
import Data.CaseInsensitive (original)
import qualified Data.CaseInsensitive as CI
import Data.Code as Code
import Data.Handle (Handle, parseHandle)
import Data.HavePendingInvitations
import Data.Id (TeamId, UserId)
import Data.Misc (PlainTextPassword6)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import qualified Data.Text.Lazy as LText
import Data.These
import Data.These.Combinators
import Imports
import Polysemy
import Polysemy.Error
import qualified SAML2.WebSSO as SAML
import Spar.Error (SparCustomError (..), SparError)
import Wire.API.Error.Galley (AuthenticationError (..))
import Wire.API.Team.Feature
import Wire.API.Team.Member (HiddenPerm (CreateReadDeleteScimToken), IsPerm, TeamMember, hasPermission)
import Wire.API.User
import Wire.API.User.Auth.ReAuth (ReAuthUser (..))
import Wire.API.User.Scim (ValidScimId (..))
import Wire.BrigAPIAccess (BrigAPIAccess)
import qualified Wire.BrigAPIAccess as BrigAPIAccess
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import qualified Wire.GalleyAPIAccess as GalleyAPIAccess

----------------------------------------------------------------------

veidToUserSSOId :: ValidScimId -> UserSSOId
veidToUserSSOId (ValidScimId eid authInfo) = maybe (UserScimExternalId eid) UserSSOId (justThere authInfo)

veidFromUserSSOId ::
  (MonadError String m) =>
  UserSSOId ->
  -- | this is either the unvalidated email if exists, or otherwise the validated email.
  Maybe EmailAddress ->
  m ValidScimId
veidFromUserSSOId ssoId mEmail = case ssoId of
  UserSSOId uref -> do
    let eid = CI.original $ uref ^. SAML.uidSubject . to SAML.unsafeShowNameID
    pure $ case mEmail of
      Just email -> ValidScimId eid (These email uref)
      Nothing -> ValidScimId eid (That uref)
  UserScimExternalId veid -> do
    case mEmail of
      Just email ->
        pure $ ValidScimId veid (This email)
      Nothing ->
        -- If veid can be parsed as an email, we end up in the case above with email delivered separately.
        throwError "internal error: externalId is not an email and there is no SAML issuer"

-- | Turns ssoid and email* fields back into a `ValidScimId`.
oldVeidFromBrigUser :: User -> Maybe ValidScimId
oldVeidFromBrigUser usr =
  let mbEmail = userEmail usr <|> userEmailUnvalidated usr
   in fromRight (error "impossible") $ (`veidFromUserSSOId` mbEmail) `mapM` userSSOId usr

-- | Compute ValidScimId from updates.  Take both the old user (just
-- like `oldVeidFromBrigUser`) and updated idp issuer and unvalidated
-- email into consideration.
--
-- If updated values are `Nothing`, the corresponding data from brig
-- user will be ignored (this is how you delete an idp association).
--
-- `userSSOId usr` can be empty if the user has no SAML credentials
-- and is brought under scim management for the first time.  In that
-- case, the externalId is taken to be the email address.
newVeidFromBrigUser :: (MonadError String m) => User -> Maybe SAML.Issuer -> m ValidScimId
newVeidFromBrigUser usr mIssuer = case (userSSOId usr, userEmail usr <|> userEmailUnvalidated usr, mIssuer) of
  (Just ssoid, mbEmail, _) -> do
    -- this makes sure email encoded in ssoid is in synch with SCIM user.
    veidFromUserSSOId (updateSsoid ssoid) mbEmail
  (Nothing, Just email, Just issuer) -> pure $ ValidScimId (fromEmail email) (These email (SAML.UserRef issuer (fromRight' $ emailToSAMLNameID email)))
  (Nothing, Just email, Nothing) -> pure $ ValidScimId (fromEmail email) (This email)
  (Nothing, Nothing, _) -> throwError "user has neither ssoIdentity nor userEmail"
  where
    updateSsoid :: UserSSOId -> UserSSOId
    updateSsoid ssoid = case (ssoid, mIssuer) of
      (UserSSOId uref, Nothing) -> UserScimExternalId (uref ^. SAML.uidSubject . to SAML.nameIDToST . to original)
      (dontchange@(UserScimExternalId _), Nothing) -> dontchange
      (UserSSOId uref, Just issuer) -> UserSSOId (uref & SAML.uidTenant .~ issuer)
      (UserScimExternalId eid, Just issuer) ->
        let nameId :: SAML.NameID = SAML.emailNameID eid & fromRight (SAML.unspecifiedNameID eid)
         in UserSSOId (SAML.UserRef issuer nameId)

-- | Take a maybe text, construct a 'Name' from what we have in a scim user.  If the text
-- isn't present, use an email address or a saml subject (usually also an email address).  If
-- both are 'Nothing', fail.
mkUserName :: Maybe Text -> These EmailAddress SAML.UserRef -> Either String Name
mkUserName (Just n) = const $ mkName n
mkUserName Nothing =
  these
    (mkName . fromEmail)
    (\uref -> mkName (CI.original . SAML.unsafeShowNameID $ uref ^. SAML.uidSubject))
    (\_ uref -> mkName (CI.original . SAML.unsafeShowNameID $ uref ^. SAML.uidSubject))

----------------------------------------------------------------------

-- | Check that an id maps to an user on brig that is 'Active' (or optionally
-- 'PendingInvitation') and has a team id.
getBrigUserTeam :: (HasCallStack, Member BrigAPIAccess r) => HavePendingInvitations -> UserId -> Sem r (Maybe TeamId)
getBrigUserTeam ifpend = fmap (userTeam =<<) . BrigAPIAccess.getAccount ifpend

-- | Pull team id for z-user from brig.  Check permission in galley.  Return team id.  Fail if
-- permission check fails or the user is not in status 'Active'.
getZUsrCheckPerm ::
  forall r perm.
  ( HasCallStack,
    ( Member BrigAPIAccess r,
      Member GalleyAPIAccess r,
      Member (Error SparError) r
    ),
    IsPerm TeamMember perm,
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
      (\teamid -> teamid <$ assertHasPermission teamid perm uid)

authorizeScimTokenManagement ::
  forall r.
  ( HasCallStack,
    ( Member BrigAPIAccess r,
      Member GalleyAPIAccess r,
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
      (\teamid -> teamid <$ assertHasPermission teamid CreateReadDeleteScimToken uid)

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
giveDefaultHandle :: (HasCallStack, Member BrigAPIAccess r) => User -> Sem r Handle
giveDefaultHandle usr = case userHandle usr of
  Just handle -> pure handle
  Nothing -> do
    let handle = fromJust . parseHandle . decodeUtf8With lenientDecode . toByteString' $ uid
        uid = userId usr
    BrigAPIAccess.setHandle uid handle
    pure handle

-- | Verify user's password (needed for certain powerful operations).
ensureReAuthorised ::
  ( Member BrigAPIAccess r,
    Member (Error SparError) r
  ) =>
  Maybe UserId ->
  Maybe PlainTextPassword6 ->
  Maybe Code.Value ->
  Maybe VerificationAction ->
  Sem r ()
ensureReAuthorised Nothing _ _ _ = throw $ SAML.CustomError SparMissingZUsr
ensureReAuthorised (Just uid) mpwd mcode maction = do
  result <- BrigAPIAccess.reauthUser uid (ReAuthUser mpwd mcode maction)
  case result of
    Right () -> pure ()
    Left ReAuthFailed -> throw $ SAML.CustomError SparReAuthRequired
    Left VerificationCodeRequired -> throw $ SAML.CustomError SparReAuthCodeAuthRequired
    Left VerificationCodeAuthFailed -> throw $ SAML.CustomError SparReAuthCodeAuthFailed
    Left RateLimitExceeded -> throw $ SAML.CustomError SparReAuthRateLimitExceeded

-- | User is member of a given team and has a given permission there.
assertHasPermission ::
  ( Member GalleyAPIAccess r,
    Member (Error SparError) r,
    IsPerm TeamMember perm,
    Show perm
  ) =>
  TeamId ->
  perm ->
  UserId ->
  Sem r ()
assertHasPermission tid perm uid = do
  mbMember <- GalleyAPIAccess.getTeamMember uid tid
  case mbMember of
    Just member | hasPermission member perm -> pure ()
    _ -> throw $ SAML.CustomError (SparNoPermission (LText.pack $ show perm))

-- | Check that SSO is enabled for the given team.
assertSSOEnabled ::
  ( Member GalleyAPIAccess r,
    Member (Error SparError) r
  ) =>
  TeamId ->
  Sem r ()
assertSSOEnabled tid = do
  feat <- GalleyAPIAccess.getFeatureConfigForTeam @_ @SSOConfig tid
  unless (feat.status == FeatureStatusEnabled) $
    throw $
      SAML.CustomError SparSSODisabled
