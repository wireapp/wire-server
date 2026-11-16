{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

-- | SCIM is the only writer of the user attributes it owns: since we never push SCIM data
-- back to the IdP\/provisioning side, any change by somebody else would silently put wire
-- and the SCIM client out of sync.  So for a user with @managed_by == scim@, every field of
-- @Wire.API.User.User@ that SCIM can write (ie., that is derived from
-- @Wire.API.User.Scim.ValidScimUser@) must be rejected for all other actors.
--
-- Coverage map (@User@ field <- @ValidScimUser@ field: where it is tested):
--
-- * @userDisplayName@ <- @name@: 'testScimUserIsNotAllowedToChangeName' (self),
--   'Test.Spar.testScimUserIsNotAllowedToChangeNameOnRegistering' (registration),
--   'Test.Spar.testAllowUpdatesBySCIMWhenE2EIdEnabled' (self, e2eid teams).
-- * @userHandle@ <- @handle@: 'testScimUserIsNotAllowedToChangeName' (self),
--   'Test.Spar.testAllowUpdatesBySCIMWhenE2EIdEnabled' (self, e2eid teams).
-- * @userLocale@ <- @locale@: 'testScimManagedLocaleIsLockedForUser' (self).
-- * @userIdentity@ (email) <- @emails@: 'testScimManagedEmailIsLockedForUser' (self),
--   'testScimManagedEmailCannotBeRemoved' (self, deletion),
--   'testScimManagedEmailIsLockedForTeamAdmin' (team admin).
-- * @userIdentity@ (sso id) <- @externalId@: no non-SCIM write path exists (brig only
--   accepts sso ids on internal end-points used by spar);
--   'Test.Spar.testAllowUpdatesBySCIMWhenE2EIdEnabled' checks the SCIM write path.
-- * @userEmailUnvalidated@ <- (email validation triggered by @emails@):
--   'Test.Spar.testSparScimInvalidatesPendingEmail'.
-- * @userStatus@ <- @active@: no non-SCIM write path exists for team members (account
--   status is only settable on brig-internal end-points; a user can delete her own
--   account, which is intentional).
-- * @userManagedBy@: written by spar only, never derived from @ValidScimUser@.
--
-- The remaining @User@ fields (@userTextStatus@, @userPict@, @userAssets@, @userAccentId@,
-- @userSupportedProtocols@, @userSearchable@, ...) are not SCIM-managed and stay writable;
-- 'testNonScimManagedFieldsStayWritable' pins that down so the guards above don't
-- accidentally grow into a blanket lock.
module Test.Spar.SCIM where

import API.Brig
import API.BrigInternal (getUsersByEmail)
import API.Common (defPassword, randomEmail)
import API.Spar
import SetupHelpers
import Testlib.Prelude

-- | Team with a SCIM token, one SCIM-provisioned (and registered, hence password-owning)
-- member, and one ordinary wire-managed member.  Returns (owner, scim user, wire user).
setupScimTeam :: (HasCallStack) => App (Value, Value, Value)
setupScimTeam = do
  (owner, tid, [wireUser]) <- createTeam OwnDomain 2
  tok <- createScimToken owner def >>= getJSON 200 >>= (%. "token") >>= asString
  scimUser <- randomScimUser
  email <- scimUser %. "emails" >>= asList >>= assertOne >>= (%. "value") >>= asString
  void $ createScimUser OwnDomain tok scimUser >>= getJSON 201
  registerInvitedUser OwnDomain tid email
  user <- getUsersByEmail OwnDomain [email] >>= getJSON 200 >>= asList >>= assertOne
  user %. "managed_by" `shouldMatch` "scim"
  pure (owner, user, wireUser)

-- | @userHandle@ and @userDisplayName@ are SCIM-managed, so the user may not change them
-- herself.
testScimUserIsNotAllowedToChangeName :: (HasCallStack) => App ()
testScimUserIsNotAllowedToChangeName = do
  (_owner, scimUser, _wireUser) <- setupScimTeam
  putHandle scimUser "foo" `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "managed-by-scim"
  putSelf scimUser def {name = Just "foo"} `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "managed-by-scim"

-- | @userLocale@ is SCIM-managed, so the user may not change it herself.  (The same is
-- checked for e2eid teams in 'Test.Spar.testAllowUpdatesBySCIMWhenE2EIdEnabled'; here the
-- team has no e2eid, so the lock can only come from @managed_by == scim@.)
testScimManagedLocaleIsLockedForUser :: (HasCallStack) => App ()
testScimManagedLocaleIsLockedForUser = do
  (_owner, scimUser, wireUser) <- setupScimTeam
  localeBefore <- scimUser %. "locale"

  putSelfLocale scimUser "de" `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "managed-by-scim"
  getSelf scimUser `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "locale" `shouldMatch` localeBefore

  -- control: the same request is fine for a wire-managed team member
  putSelfLocale wireUser "de" >>= assertStatus 200

-- | @userIdentity@ (the email part) is SCIM-managed, so the user may not change it herself.
testScimManagedEmailIsLockedForUser :: (HasCallStack) => App ()
testScimManagedEmailIsLockedForUser = do
  (_owner, scimUser, _wireUser) <- setupScimTeam
  emailBefore <- scimUser %. "email" >>= asString
  (cookie, token) <- loginWithCookieAndToken OwnDomain emailBefore defPassword

  newEmail <- randomEmail
  updateEmail scimUser newEmail cookie token `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "managed-by-scim"
  getSelf scimUser `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "email" `shouldMatch` emailBefore
    lookupField resp.json "email_unvalidated" `shouldMatch` (Nothing :: Maybe Value)

-- | Removing a SCIM-managed email must be rejected without changing the identity.
testScimManagedEmailCannotBeRemoved :: (HasCallStack) => App ()
testScimManagedEmailCannotBeRemoved = do
  (_owner, scimUser, _wireUser) <- setupScimTeam
  emailBefore <- scimUser %. "email"

  deleteSelfEmail scimUser `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "managed-by-scim"
  getSelf scimUser `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "email" `shouldMatch` emailBefore

-- | ... and neither may a team admin, who can change the email address of any other
-- wire-managed member of her team (@PUT \/users\/:uid\/email@).
testScimManagedEmailIsLockedForTeamAdmin :: (HasCallStack) => App ()
testScimManagedEmailIsLockedForTeamAdmin = do
  (owner, scimUser, wireUser) <- setupScimTeam
  emailBefore <- scimUser %. "email"

  newEmail <- randomEmail
  putUserEmail owner scimUser newEmail `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "managed-by-scim"
  getSelf scimUser `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "email" `shouldMatch` emailBefore
    lookupField resp.json "email_unvalidated" `shouldMatch` (Nothing :: Maybe Value)

  -- control: the same request is fine for a wire-managed team member
  otherEmail <- randomEmail
  putUserEmail owner wireUser otherEmail >>= assertStatus 200

-- | Fields that SCIM does not manage must stay writable by the user, even if she is
-- @managed_by == scim@.
testNonScimManagedFieldsStayWritable :: (HasCallStack) => App ()
testNonScimManagedFieldsStayWritable = do
  (_owner, scimUser, _wireUser) <- setupScimTeam
  putSelf scimUser def {accent = Just 5} >>= assertStatus 200
  getSelf scimUser `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "accent_id" `shouldMatchInt` 5
