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

module Wire.ScimSubsystem.Interpreter
  ( module Wire.ScimSubsystem.Error,
    module Wire.ScimSubsystem.Interpreter,
  )
where

import Data.Default
import Data.Id
import Data.Json.Util
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as V
import Imports
import Network.HTTP.Types.Status (notFound404)
import Network.URI (parseURI)
import Network.Wai.Utilities.Error qualified as Error
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Web.Scim.Class.Group qualified as SCG
import Web.Scim.Filter qualified as Scim
import Web.Scim.Schema.Common qualified as Common
import Web.Scim.Schema.ListResponse qualified as Scim
import Web.Scim.Schema.Meta qualified as Meta
import Web.Scim.Schema.ResourceType qualified as RT
import Web.Scim.Schema.Schema qualified as Scim
import Wire.API.Routes.Internal.Brig
import Wire.API.User
import Wire.API.User.Scim (SparTag)
import Wire.API.UserGroup
import Wire.API.UserGroup.Pagination
import Wire.BrigAPIAccess (BrigAPIAccess)
import Wire.BrigAPIAccess qualified as BrigAPI
import Wire.ScimSubsystem
import Wire.ScimSubsystem.Error

data ScimSubsystemConfig = ScimSubsystemConfig
  { scimBaseUri :: Common.URI
  }

interpretScimSubsystem ::
  ( Member (Input ScimSubsystemConfig) r,
    Member (Error ScimSubsystemError) r,
    Member BrigAPIAccess r
  ) =>
  InterpreterFor ScimSubsystem r
interpretScimSubsystem = interpret $ \case
  ScimCreateUserGroup teamId scimGroup -> createScimGroupImpl teamId scimGroup
  ScimGetUserGroup tid gid -> scimGetUserGroupImpl tid gid
  ScimGetUserGroups tid mbFilter startIndex mbCount -> scimGetUserGroupsImpl tid mbFilter startIndex mbCount
  ScimUpdateUserGroup teamId userGroupId scimGroup -> scimUpdateUserGroupImpl teamId userGroupId scimGroup
  ScimDeleteUserGroup teamId groupId -> deleteScimGroupImpl teamId groupId

createScimGroupImpl ::
  forall r.
  ( Member (Input ScimSubsystemConfig) r,
    Member (Error ScimSubsystemError) r,
    Member BrigAPIAccess r
  ) =>
  TeamId ->
  SCG.Group ->
  Sem r (SCG.StoredGroup SparTag)
createScimGroupImpl teamId grp = do
  membersNotFound <- do
    uids :: [UserId] <- parseMember `mapM` grp.members
    users <- BrigAPI.getAccountsBy def {getByUserId = uids}
    pure $
      users
        & filter (\u -> u.userTeam /= Just teamId || u.userManagedBy /= ManagedByScim)
        & fmap userId
  unless (null membersNotFound) do
    throw (ScimSubsystemGroupMembersNotFound membersNotFound)

  ugName <-
    userGroupNameFromText grp.displayName
      & either (throw . ScimSubsystemBadGroupName) pure
  ugMemberIds <-
    let go :: SCG.Member -> Sem r UserId
        go m =
          parseIdFromText m.value
            & either (throw . ScimSubsystemInvalidGroupMemberId . Text.pack) pure
     in go `mapM` grp.members

  let newGroup = NewUserGroup {name = ugName, members = V.fromList ugMemberIds}
  BrigAPI.createGroupInternal ManagedByScim teamId Nothing newGroup >>= \case
    Right ug -> do
      ScimSubsystemConfig scimBaseUri <- input
      pure $ toStoredGroup scimBaseUri ug
    Left err -> do
      throw (ScimSubsystemInternal err)

scimGetUserGroupImpl ::
  forall r.
  ( Member (Input ScimSubsystemConfig) r,
    Member (Error ScimSubsystemError) r,
    Member BrigAPIAccess r
  ) =>
  TeamId ->
  UserGroupId ->
  Sem r (SCG.StoredGroup SparTag)
scimGetUserGroupImpl tid gid = do
  let includeChannels = False -- SCIM has no notion of channels.
  maybe groupNotFound returnStoredGroup =<< BrigAPI.getGroupInternal tid gid includeChannels
  where
    groupNotFound = throw (ScimSubsystemGroupNotFound gid)
    returnStoredGroup g = do
      ScimSubsystemConfig scimBaseUri <- input
      pure $ toStoredGroup scimBaseUri g

scimGetUserGroupsImpl ::
  forall r.
  ( Member (Input ScimSubsystemConfig) r,
    Member BrigAPIAccess r
  ) =>
  TeamId ->
  Maybe Scim.Filter ->
  Maybe Int ->
  Maybe Int ->
  Sem r (Scim.ListResponse (SCG.StoredGroup SparTag))
scimGetUserGroupsImpl tid mbFilter mbStartIndex mbCount = do
  UserGroupPage {page, total} :: UserGroupPageWithMembers <- BrigAPI.getGroupsInternal tid mbFilter mbStartIndex mbCount
  ScimSubsystemConfig scimBaseUri <- input
  let page' = map (toStoredGroup scimBaseUri) page
  pure $
    Scim.ListResponse
      { schemas = [Scim.ListResponse20],
        totalResults = total,
        itemsPerPage = length page',
        startIndex = fromMaybe 1 mbStartIndex,
        resources = page'
      }

scimUpdateUserGroupImpl ::
  forall r.
  ( Member (Input ScimSubsystemConfig) r,
    Member (Error ScimSubsystemError) r,
    Member BrigAPIAccess r
  ) =>
  TeamId ->
  UserGroupId ->
  SCG.Group ->
  Sem r (SCG.StoredGroup SparTag)
scimUpdateUserGroupImpl teamId gid grp = do
  let includeChannels = False
  ug <-
    BrigAPI.getGroupInternal teamId gid includeChannels
      >>= note (ScimSubsystemGroupNotFound gid)
  when (ug.managedBy /= ManagedByScim) do
    throw (ScimSubsystemGroupNotFound gid)

  ugName <- either (throw . ScimSubsystemBadGroupName) pure $ userGroupNameFromText grp.displayName
  reqMemberIds <- for grp.members parseMember

  let currentSet = Set.fromList (toList (runIdentity ug.members))
      requestedSet = Set.fromList reqMemberIds
      toAdd = requestedSet `Set.difference` currentSet

  unless (null toAdd) do
    accounts <- BrigAPI.getUsers (Set.toList toAdd)
    let notInTeamOrNotScim = [userId u | u <- accounts, u.userManagedBy /= ManagedByScim || u.userTeam /= Just teamId]
        found = Set.fromList (userId <$> accounts)
        missing = Set.toList (toAdd `Set.difference` found)
    unless (null notInTeamOrNotScim) do
      throw (ScimSubsystemGroupMembersNotFound notInTeamOrNotScim)
    case missing of
      [] -> pure ()
      (u : _) -> throw (ScimSubsystemUserNotFound u)

  -- replace the members of the user group; propagate Brig errors
  BrigAPI.updateGroup (UpdateGroupInternalRequest teamId gid (Just ugName) (Just reqMemberIds)) >>= \case
    Right () -> pure ()
    Left err ->
      if err.code == notFound404
        then throw (ScimSubsystemGroupNotFound gid)
        else throw (ScimSubsystemInternal err)

  ScimSubsystemConfig scimBaseUri <- input
  maybe (throw $ ScimSubsystemGroupNotFound gid) (pure . toStoredGroup scimBaseUri)
    =<< BrigAPI.getGroupInternal teamId gid includeChannels

deleteScimGroupImpl ::
  forall r.
  ( Member (Error ScimSubsystemError) r,
    Member BrigAPIAccess r
  ) =>
  TeamId ->
  UserGroupId ->
  Sem r ()
deleteScimGroupImpl teamId groupId = do
  eResult <- BrigAPI.deleteGroupInternal ManagedByScim teamId groupId
  case eResult of
    Right () -> pure ()
    Left BrigAPI.DeleteGroupManagedManagedByMismatch -> throw (ScimSubsystemForbidden groupId)

toStoredGroup :: Common.URI -> UserGroup -> SCG.StoredGroup SparTag
toStoredGroup scimBaseUri ug = Meta.WithMeta meta (Common.WithId ug.id_ sg)
  where
    mkLocation :: String -> Common.URI
    mkLocation pathSuffix =
      let uri = Common.uriToString scimBaseUri <> pathSuffix
       in maybe (error "invalid SCIM group location URI") Common.URI (parseURI uri)

    meta =
      Meta.Meta
        { Meta.resourceType = RT.GroupResource,
          Meta.created = fromUTCTimeMillis ug.createdAt,
          Meta.lastModified = fromUTCTimeMillis ug.createdAt,
          Meta.version = Meta.Weak "v1",
          Meta.location = mkLocation $ "/Groups/" <> Text.unpack (idToText ug.id_)
        }

    sg =
      SCG.Group
        { schemas = ["urn:ietf:params:scim:schemas:core:2.0:Group"],
          displayName = userGroupNameToText ug.name,
          members =
            [ SCG.Member
                { value = idToText uid,
                  typ = "User",
                  ref = Common.uriToText . mkLocation $ "/Users/" <> idToString uid
                }
              | uid <- toList (runIdentity ug.members)
            ]
        }

parseMember ::
  (Member (Error ScimSubsystemError) r) =>
  SCG.Member ->
  Sem r UserId
parseMember m =
  parseIdFromText m.value
    & either (throw . ScimSubsystemInvalidGroupMemberId . Text.pack) pure
