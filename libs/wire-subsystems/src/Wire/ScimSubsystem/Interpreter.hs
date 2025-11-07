module Wire.ScimSubsystem.Interpreter where

import Data.UUID qualified as UUID
import Data.Default
import Data.Id
import Data.Json.Util
import Data.Text qualified as Text
import Data.Vector qualified as V
import Imports
import Network.URI (parseURI)
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Web.Scim.Class.Group qualified as SCG
import Web.Scim.Schema.Common qualified as Common
import Web.Scim.Schema.Error
import Web.Scim.Schema.Meta qualified as Meta
import Web.Scim.Schema.ResourceType qualified as RT
import Wire.API.User
import Wire.API.User.Scim (SparTag)
import Wire.API.UserGroup
import Wire.BrigAPIAccess (BrigAPIAccess)
import Wire.BrigAPIAccess qualified as BrigAPI
import Wire.ScimSubsystem
import Wire.UserGroupSubsystem
import Wire.UserGroupStore qualified as UGStore
import Wire.UserSubsystem

data ScimSubsystemConfig = ScimSubsystemConfig
  { scimBaseUri :: Common.URI
  }

interpretScimSubsystem ::
  ( Member UserGroupSubsystem r,
    Member UGStore.UserGroupStore r,
    Member (Input ScimSubsystemConfig) r,
    Member (Error ScimSubsystemError) r,
    Member BrigAPIAccess r
  ) =>
  InterpreterFor ScimSubsystem r
interpretScimSubsystem = interpret $ \case
  ScimCreateUserGroup teamId scimGroup -> createScimGroupImpl teamId scimGroup
  ScimGetUserGroup tid gid -> scimGetUserGroupImpl tid gid

data ScimSubsystemError
  = ScimSubsystemError ScimError
  | ScimSubsystemInvalidGroupMemberId Text
  | ScimSubsystemScimGroupWithNonScimMembers [UserId]
  deriving (Show, Eq)

scimThrow :: (Member (Error ScimSubsystemError) r) => ScimError -> Sem r a
scimThrow = throw . ScimSubsystemError

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
  membersNotManagedByScim <- do
    let uidsAsText = (.value) <$> grp.members
    uids :: [UserId] <-
      let thrw = throw . ScimSubsystemInvalidGroupMemberId
       in forM uidsAsText $ either (thrw . Text.pack) pure . parseIdFromText
    users <- BrigAPI.getAccountsBy def {getByUserId = uids}
    pure $
      users
        & filter (\u -> u.userManagedBy /= ManagedByScim)
        & fmap userId
  unless (null membersNotManagedByScim) do
    throw (ScimSubsystemScimGroupWithNonScimMembers membersNotManagedByScim)

  ugName <-
    userGroupNameFromText grp.displayName
      & either (scimThrow . badRequest InvalidValue . Just) pure
  ugMemberIds <-
    let go :: SCG.Member -> Sem r UserId
        go m =
          parseIdFromText m.value
            & either (scimThrow . badRequest InvalidValue . Just . Text.pack) pure
     in go `mapM` grp.members

  let newGroup = NewUserGroup {name = ugName, members = V.fromList ugMemberIds}
  ug <- BrigAPI.createGroupFull ManagedByScim teamId Nothing newGroup
  ScimSubsystemConfig scimBaseUri <- input
  pure $ toStoredGroup scimBaseUri ug

scimGetUserGroupImpl ::
  forall r.
  ( Member (Input ScimSubsystemConfig) r,
    Member (Error ScimSubsystemError) r,
    Member UGStore.UserGroupStore r,
    () ~ ()
  ) =>
  TeamId ->
  UserGroupId ->
  Sem r (SCG.StoredGroup SparTag)
scimGetUserGroupImpl tid gid = do
  let includeChannels = False -- SCIM has no notion of channels.
  maybe groupNotFound returnStoredGroup =<< UGStore.getUserGroup tid gid includeChannels
  where
    groupNotFound = scimThrow $ notFound "Group" $ UUID.toText $ toUUID gid
    returnStoredGroup g = do
      ScimSubsystemConfig scimBaseUri <- input
      return $ toStoredGroup scimBaseUri g

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
