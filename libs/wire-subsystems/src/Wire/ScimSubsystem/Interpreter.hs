module Wire.ScimSubsystem.Interpreter where

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
import Wire.Error
import Wire.ScimSubsystem
import Wire.UserGroupSubsystem

data ScimSubsystemConfig = ScimSubsystemConfig
  { scimBaseUri :: Common.URI
  }

instance Default ScimSubsystemConfig where
  def =
    ScimSubsystemConfig
      { scimBaseUri = todo -- "/scim/v2"
      }

interpretScimSubsystem ::
  ( Member UserGroupSubsystem r,
    Member (Input ScimSubsystemConfig) r,
    Member (Error ScimSubsystemError) r
  ) =>
  InterpreterFor ScimSubsystem r
interpretScimSubsystem = interpret $ \case
  CreateScimGroup teamId scimGroup -> createScimGroupImpl teamId scimGroup

data ScimSubsystemError = ScimSubsystemError ScimError
  deriving (Show, Eq)

scimThrow :: (Member (Error ScimSubsystemError) r) => ScimError -> Sem r a
scimThrow = throw . ScimSubsystemError

scimSubsystemErrorToHttpError :: ScimSubsystemError -> HttpError
scimSubsystemErrorToHttpError =
  StdError . \case
    ScimSubsystemError _err -> undefined -- _ scimToServerError

createScimGroupImpl ::
  forall r.
  ( Member UserGroupSubsystem r,
    Member (Input ScimSubsystemConfig) r,
    Member (Error ScimSubsystemError) r
  ) =>
  TeamId ->
  SCG.Group ->
  Sem r (SCG.StoredGroup SparTag)
createScimGroupImpl teamId grp = do
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
  ug <- createGroupFull ManagedByScim teamId Nothing newGroup
  scimBaseUri <- (.scimBaseUri) <$> input
  pure $ toStoredGroup scimBaseUri ug

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
