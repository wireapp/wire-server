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
-- The remaining @User@ fields (@userTextStatus@, @userPict@, @userAssets@, @userAccentId@,
-- @userSupportedProtocols@, @userSearchable@, ...) are not SCIM-managed and stay writable;
-- 'testNonScimManagedFieldsStayWritable' pins that down so the guards above don't
-- accidentally grow into a blanket lock.
module Test.Spar.SCIM where

import API.Brig
import API.BrigInternal (getUsersByEmail)
import API.Common (defPassword, randomEmail)
import API.Galley qualified as Galley
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

-- | A team owner must not overwrite a SCIM-provisioned role.
testScimManagedRoleIsLockedForTeamAdmin :: (HasCallStack) => App ()
testScimManagedRoleIsLockedForTeamAdmin = do
  (owner, scimUser, wireUser) <- setupScimTeam
  tid <- scimUser %. "team" >>= asString
  tok <- createScimToken owner def >>= getJSON 200 >>= (%. "token") >>= asString
  uid <- scimUser %. "id" >>= asString
  before <- getScimUser OwnDomain tok uid >>= getJSON 200 >>= (%. "roles")
  Galley.updateTeamMember tid owner scimUser Galley.Admin >>= assertStatus 403
  after <- getScimUser OwnDomain tok uid >>= getJSON 200 >>= (%. "roles")
  after `shouldMatch` before
  Galley.updateTeamMember tid owner wireUser Galley.Admin >>= assertSuccess

-- | Each route gets its own test so one missing guard cannot hide the others.
-- The actor is deliberately wire-managed: ownership belongs to the target group.
checkScimGroupIsLocked :: (HasCallStack) => (Value -> String -> String -> String -> App Response) -> App ()
checkScimGroupIsLocked mutate = do
  (owner, scimUser, wireUser) <- setupScimTeam
  tok <- createScimToken owner def >>= getJSON 200 >>= (%. "token") >>= asString
  uid <- scimUser %. "id" >>= asString
  other <- wireUser %. "id" >>= asString
  let group = object ["schemas" .= ["urn:ietf:params:scim:schemas:core:2.0:Group" :: String], "displayName" .= ("original" :: String), "members" .= [object ["value" .= uid]]]
  gid <- createScimUserGroup OwnDomain tok group >>= getJSON 201 >>= (%. "id") >>= asString
  before <- getScimUserGroup OwnDomain tok gid >>= getJSON 200
  mutate owner gid uid other >>= assertStatus 403
  after <- getScimUserGroup OwnDomain tok gid >>= getJSON 200
  after `shouldMatch` before

  -- Control: the identical operation remains available on a wire-managed group.
  wireGid <- createUserGroup owner (object ["name" .= ("wire group" :: String), "members" .= [uid]]) >>= getJSON 200 >>= (%. "id") >>= asString
  mutate owner wireGid uid other >>= assertSuccess

-- These regressions intentionally fail until public group writes check managedBy.
testScimGroupCannotBeRenamed :: (HasCallStack) => App ()
testScimGroupCannotBeRenamed = checkScimGroupIsLocked $ \owner gid _ _ -> updateUserGroup owner gid (object ["name" .= ("changed" :: String)])

testScimGroupCannotBeDeleted :: (HasCallStack) => App ()
testScimGroupCannotBeDeleted = checkScimGroupIsLocked $ \owner gid _ _ -> deleteUserGroup owner gid

testScimGroupMemberCannotBeAdded :: (HasCallStack) => App ()
testScimGroupMemberCannotBeAdded = checkScimGroupIsLocked $ \owner gid _ other -> addUserToGroup owner gid other

testScimGroupMembersCannotBeAdded :: (HasCallStack) => App ()
testScimGroupMembersCannotBeAdded = checkScimGroupIsLocked $ \owner gid _ other -> addUsersToGroup owner gid [other]

testScimGroupMemberCannotBeRemoved :: (HasCallStack) => App ()
testScimGroupMemberCannotBeRemoved = checkScimGroupIsLocked $ \owner gid uid _ -> removeUserFromGroup owner gid uid

testScimGroupMembersCannotBeReplaced :: (HasCallStack) => App ()
testScimGroupMembersCannotBeReplaced = checkScimGroupIsLocked $ \owner gid _ other -> updateUserGroupUsers owner gid [other]

-- | Non-provisioned profile fields must not acquire a blanket SCIM lock.
testNonScimManagedFieldsStayWritable :: (HasCallStack) => App ()
testNonScimManagedFieldsStayWritable = do
  (_owner, scimUser, _wireUser) <- setupScimTeam
  putSelf scimUser def {accent = Just 5} >>= assertStatus 200
  getSelf scimUser `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "accent_id" `shouldMatchInt` 5
