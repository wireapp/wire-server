{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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

-- | This module contains the analog of some of the team-level roles & permissions types in
-- "Wire.API.Team".
module Wire.API.Conversation.Role
  ( -- * Role
    ConversationRole,
    wireConvRoles,
    convRoleWireAdmin,
    convRoleWireMember,
    ConversationRolesList (..),

    -- * RoleName
    RoleName,
    fromRoleName,
    parseRoleName,
    wireConvRoleNames,
    roleNameWireAdmin,
    roleNameWireMember,

    -- * Action
    Action (..),
    Actions (..),

    -- * helpers
    isValidRoleName,
    roleActions,
    toConvRole,

    -- * Swagger
    modelConversationRole,
    modelConversationRolesList,
    typeConversationRoleAction,
  )
where

import Cassandra.CQL hiding (Set)
import Control.Applicative (optional)
import Control.Lens (at, (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Data.Hashable
import Data.Proxy (Proxy (..))
import Data.Range (fromRange, genRangeText)
import Data.Schema
import qualified Data.Set as Set
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import qualified Deriving.Swagger as S
import Imports
import qualified Test.QuickCheck as QC
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- Role

-- | A conversation role is associated to a user in the scope of a conversation and implies
-- with a set of 'Action's.  Conversation-level analog to what 'Role' is on the team-level.
--
-- Do not expose the constructors directly,
-- use smart constructors instead to ensure that all validation is performed.
data ConversationRole
  = ConvRoleWireAdmin
  | ConvRoleWireMember
  | ConvRoleCustom RoleName Actions
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationRole)

modelConversationRole :: Doc.Model
modelConversationRole = Doc.defineModel "ConversationRole" $ do
  Doc.description "Conversation role"
  Doc.property "conversation_role" Doc.string' $
    Doc.description
      "role name, between 2 and 128 chars, 'wire_' prefix \
      \is reserved for roles designed by Wire (i.e., no \
      \custom roles can have the same prefix)"
  Doc.property "actions" (Doc.array typeConversationRoleAction) $
    Doc.description "The set of actions allowed for this role"

instance S.ToSchema ConversationRole where
  declareNamedSchema _ = do
    conversationRoleSchema <-
      S.declareSchemaRef (Proxy @RoleName)
    let convRoleSchema :: S.Schema =
          mempty
            & S.properties . at "conversation_role" ?~ conversationRoleSchema
            & S.properties . at "actions"
              ?~ S.Inline
                ( S.toSchema (Proxy @[Action])
                    & description ?~ "The set of actions allowed for this role"
                )
    pure (S.NamedSchema (Just "ConversationRole") convRoleSchema)

instance ToJSON ConversationRole where
  toJSON cr =
    A.object
      [ "conversation_role" A..= roleToRoleName cr,
        "actions" A..= roleActions cr
      ]

roleActions :: ConversationRole -> Set Action
roleActions ConvRoleWireAdmin = allowedActions allActions
roleActions ConvRoleWireMember =
  Set.fromList
    [ LeaveConversation
    ]
roleActions (ConvRoleCustom _ (Actions actions)) = actions

roleToRoleName :: ConversationRole -> RoleName
roleToRoleName ConvRoleWireAdmin = roleNameWireAdmin
roleToRoleName ConvRoleWireMember = roleNameWireMember
roleToRoleName (ConvRoleCustom l _) = l

instance FromJSON ConversationRole where
  parseJSON = A.withObject "conversationRole" $ \o -> do
    role <- o A..: "conversation_role"
    actions <- o A..: "actions"
    case (toConvRole role (Just $ Actions actions)) of
      Just cr -> return cr
      Nothing -> fail ("Failed to parse: " ++ show o)

toConvRole :: RoleName -> Maybe Actions -> Maybe ConversationRole
toConvRole (RoleName "wire_admin") _ = Just ConvRoleWireAdmin
toConvRole (RoleName "wire_member") _ = Just ConvRoleWireMember
toConvRole x (Just as) = Just (ConvRoleCustom x as)
toConvRole _ _ = Nothing

wireConvRoles :: [ConversationRole]
wireConvRoles = [ConvRoleWireAdmin, ConvRoleWireMember]

convRoleWireAdmin :: ConversationRole
convRoleWireAdmin = ConvRoleWireAdmin

convRoleWireMember :: ConversationRole
convRoleWireMember = ConvRoleWireMember

data ConversationRolesList = ConversationRolesList
  { convRolesList :: [ConversationRole]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationRolesList)
  deriving (S.ToSchema) via (S.CustomSwagger '[S.FieldLabelModifier (S.LabelMappings '["convRolesList" 'S.:-> "conversation_roles"])] ConversationRolesList)

modelConversationRolesList :: Doc.Model
modelConversationRolesList = Doc.defineModel "ConversationRolesList" $ do
  Doc.description "list of roles allowed in the given conversation"
  Doc.property "conversation_roles" (Doc.unique $ Doc.array (Doc.ref modelConversationRole)) $
    Doc.description "the array of conversation roles"

instance ToJSON ConversationRolesList where
  toJSON (ConversationRolesList r) =
    A.object
      [ "conversation_roles" A..= r
      ]

instance FromJSON ConversationRolesList where
  parseJSON = A.withObject "ConversationRolesList" $ \o ->
    ConversationRolesList
      <$> o A..: "conversation_roles"

--------------------------------------------------------------------------------
-- RoleName

-- RoleNames with `wire_` prefix are reserved
-- and cannot be created by externals. Therefore, never
-- expose this constructor outside of this module.
newtype RoleName = RoleName {fromRoleName :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToByteString, Hashable)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema RoleName

instance ToSchema RoleName where
  schema =
    (S.schema . description ?~ desc) $
      RoleName <$> fromRoleName .= text "RoleName"
    where
      desc =
        "Role name, between 2 and 128 chars, 'wire_' prefix \
        \is reserved for roles designed by Wire (i.e., no \
        \custom roles can have the same prefix)"

instance FromByteString RoleName where
  parser = parser >>= maybe (fail "Invalid RoleName") return . parseRoleName

deriving instance Cql RoleName

instance Arbitrary RoleName where
  arbitrary =
    RoleName . fromRange
      <$> genRangeText @2 @128 genChar
    where
      genChar = QC.elements $ ['a' .. 'z'] <> ['0' .. '9'] <> ['_']

wireConvRoleNames :: [RoleName]
wireConvRoleNames = [roleNameWireAdmin, roleNameWireMember]

roleNameWireAdmin :: RoleName
roleNameWireAdmin = RoleName "wire_admin"

roleNameWireMember :: RoleName
roleNameWireMember = RoleName "wire_member"

parseRoleName :: Text -> Maybe RoleName
parseRoleName t
  | isValidRoleName t = Just (RoleName t)
  | otherwise = Nothing

-- All RoleNames should have 2-128 chars
isValidRoleName :: Text -> Bool
isValidRoleName =
  either (const False) (const True)
    . parseOnly customRoleName
  where
    customRoleName =
      count 2 (satisfy chars)
        *> count 126 (optional (satisfy chars))
        *> endOfInput
    chars = inClass "a-z0-9_"

--------------------------------------------------------------------------------
-- Action

newtype Actions = Actions
  { allowedActions :: Set Action
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

allActions :: Actions
allActions = Actions $ Set.fromList [minBound .. maxBound]

-- | These conversation-level permissions.  Analogous to the team-level permissions called
-- 'Perm' (or 'Permissions').
data Action
  = AddConversationMember
  | RemoveConversationMember
  | ModifyConversationName
  | ModifyConversationMessageTimer
  | ModifyConversationReceiptMode
  | ModifyConversationAccess
  | ModifyOtherConversationMember
  | LeaveConversation
  | DeleteConversation
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform Action)
  deriving (S.ToSchema) via (S.CustomSwagger '[S.ConstructorTagModifier S.CamelToSnake] Action)

typeConversationRoleAction :: Doc.DataType
typeConversationRoleAction =
  Doc.string $
    Doc.enum
      [ "add_conversation_member",
        "remove_conversation_member",
        "modify_conversation_name",
        "modify_conversation_message_timer",
        "modify_conversation_receipt_mode",
        "modify_conversation_access",
        "modify_other_conversation_member",
        "leave_conversation",
        "delete_conversation"
      ]

A.deriveJSON A.defaultOptions {A.constructorTagModifier = A.camelTo2 '_'} ''Action
