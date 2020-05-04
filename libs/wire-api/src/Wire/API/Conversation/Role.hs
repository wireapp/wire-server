{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  ( -- * Action
    Action (..),
    Actions (..),

    -- * Role
    ConversationRole,
    wireConvRoles,
    convRoleWireAdmin,
    convRoleWireMember,
    ConversationRolesList (..),

    -- * RoleName
    RoleName,
    wireConvRoleNames,
    roleNameWireAdmin,
    roleNameWireMember,

    -- * Swagger
    modelConversationRole,
    modelConversationRolesList,
  )
where

import Cassandra.CQL hiding (Set)
import Control.Applicative (optional)
import Data.Aeson
import Data.Aeson.TH
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import Data.Hashable
import qualified Data.Set as Set
import qualified Data.Swagger.Build.Api as Doc
import Imports

--------------------------------------------------------------------------------
-- Action

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
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

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

deriveJSON defaultOptions {constructorTagModifier = camelTo2 '_'} ''Action

newtype Actions = Actions
  { allowedActions :: Set Action
  }
  deriving (Eq, Ord, Show, Generic)

allActions :: Actions
allActions = Actions $ Set.fromList [minBound .. maxBound]

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
  deriving (Eq, Show)

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

instance ToJSON ConversationRole where
  toJSON cr =
    object
      [ "conversation_role" .= roleToRoleName cr,
        "actions" .= roleActions cr
      ]

instance FromJSON ConversationRole where
  parseJSON = withObject "conversationRole" $ \o -> do
    role <- o .: "conversation_role"
    actions <- o .: "actions"
    case (toConvRole role (Just $ Actions actions)) of
      Just cr -> return cr
      Nothing -> fail ("Failed to parse: " ++ show o)

wireConvRoles :: [ConversationRole]
wireConvRoles = [ConvRoleWireAdmin, ConvRoleWireMember]

convRoleWireAdmin :: ConversationRole
convRoleWireAdmin = ConvRoleWireAdmin

convRoleWireMember :: ConversationRole
convRoleWireMember = ConvRoleWireMember

data ConversationRolesList = ConversationRolesList
  { convRolesList :: [ConversationRole]
  }
  deriving (Eq, Show)

modelConversationRolesList :: Doc.Model
modelConversationRolesList = Doc.defineModel "ConversationRolesList" $ do
  Doc.description "list of roles allowed in the given conversation"
  Doc.property "conversation_roles" (Doc.unique $ Doc.array (Doc.ref modelConversationRole)) $
    Doc.description "the array of conversation roles"

instance ToJSON ConversationRolesList where
  toJSON (ConversationRolesList r) =
    object
      [ "conversation_roles" .= r
      ]

--------------------------------------------------------------------------------
-- RoleName

-- RoleNames with `wire_` prefix are reserved
-- and cannot be created by externals. Therefore, never
-- expose this constructor outside of this module.
newtype RoleName = RoleName {fromRoleName :: Text}
  deriving (Eq, Show, ToJSON, ToByteString, Hashable, Generic)

instance FromByteString RoleName where
  parser = parser >>= maybe (fail "Invalid RoleName") return . parseRoleName

instance FromJSON RoleName where
  parseJSON =
    withText "RoleName" $
      maybe (fail "Invalid RoleName") pure . parseRoleName

deriving instance Cql RoleName

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
-- helpers (used in JSON instances)

roleToRoleName :: ConversationRole -> RoleName
roleToRoleName ConvRoleWireAdmin = roleNameWireAdmin
roleToRoleName ConvRoleWireMember = roleNameWireMember
roleToRoleName (ConvRoleCustom l _) = l

toConvRole :: RoleName -> Maybe Actions -> Maybe ConversationRole
toConvRole (RoleName "wire_admin") _ = Just ConvRoleWireAdmin
toConvRole (RoleName "wire_member") _ = Just ConvRoleWireMember
toConvRole x (Just as) = Just (ConvRoleCustom x as)
toConvRole _ _ = Nothing

roleActions :: ConversationRole -> Set Action
roleActions ConvRoleWireAdmin = allowedActions allActions
roleActions ConvRoleWireMember =
  Set.fromList
    [ LeaveConversation
    ]
roleActions (ConvRoleCustom _ (Actions actions)) = actions
