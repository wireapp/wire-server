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
module Galley.Types.Conversations.Roles
  ( ConversationRole,
    convRoleWireAdmin,
    convRoleWireMember,
    wireConvRoles,
    RoleName,
    roleNameWireAdmin,
    roleNameWireMember,
    wireConvRoleNames,
    Action (..),
    Actions (..),
    ConversationRolesList (..),
    isActionAllowed,
    roleNameToActions,
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
import qualified Data.Text as T
import Imports

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

deriveJSON defaultOptions {constructorTagModifier = camelTo2 '_'} ''Action

newtype Actions = Actions
  { allowedActions :: Set Action
  }
  deriving (Eq, Ord, Show, Generic)

-- | A conversation role is associated to a user in the scope of a conversation and implies
-- with a set of 'Action's.  Conversation-level analog to what 'Role' is on the team-level.
--
-- Do not expose the constructors directly, use smart
-- constructors instead to ensure that all validation
-- is performed
data ConversationRole
  = ConvRoleWireAdmin
  | ConvRoleWireMember
  | ConvRoleCustom RoleName Actions
  deriving (Eq, Show)

-- Given an action and a RoleName, three possible outcomes:
-- Just True:  Yes, the action is allowed
-- Just False: No, the action is not allowed
-- Nothing:    Not enough information, this is a custom role
isActionAllowed :: Action -> RoleName -> Maybe Bool
isActionAllowed action rn
  | isCustomRoleName rn = Nothing
  | otherwise = pure $ maybe False (action `elem`) (roleNameToActions rn)

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

data ConversationRolesList = ConversationRolesList
  { convRolesList :: [ConversationRole]
  }
  deriving (Eq, Show)

instance ToJSON ConversationRolesList where
  toJSON (ConversationRolesList r) =
    object
      [ "conversation_roles" .= r
      ]

-- RoleNames with `wire_` prefix are reserved
-- and cannot be created by externals. Therefore, never
-- expose this constructor outside of this module.
newtype RoleName = RoleName {fromRoleName :: Text}
  deriving (Eq, Show, ToJSON, ToByteString, Hashable, Generic)

deriving instance Cql RoleName

instance FromByteString RoleName where
  parser = parser >>= maybe (fail "Invalid RoleName") return . parseRoleName

instance FromJSON RoleName where
  parseJSON =
    withText "RoleName" $
      maybe (fail "Invalid RoleName") pure . parseRoleName

wireConvRoles :: [ConversationRole]
wireConvRoles =
  [ ConvRoleWireAdmin,
    ConvRoleWireMember
  ]

wireConvRoleNames :: [RoleName]
wireConvRoleNames = [roleNameWireAdmin, roleNameWireMember]

roleNameWireAdmin :: RoleName
roleNameWireAdmin = RoleName "wire_admin"

roleNameWireMember :: RoleName
roleNameWireMember = RoleName "wire_member"

convRoleWireAdmin :: ConversationRole
convRoleWireAdmin = ConvRoleWireAdmin

convRoleWireMember :: ConversationRole
convRoleWireMember = ConvRoleWireMember

-- | This is how the definition of the convRoleCustom constructor
--   should look like. We comment this out due to the fact that we
--   do not want to use this yet and want to avoid `Defined but not
--   used warnings`
-- convRoleCustom :: RoleName -> Actions -> Maybe ConversationRole
-- convRoleCustom r a
--     | isCustomRoleName r = Just (ConvRoleCustom r a)
--     | otherwise          = Nothing
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

--  * Custom RoleNames _must not_ start with `wire_`
isCustomRoleName :: RoleName -> Bool
isCustomRoleName (RoleName r) = isValidRoleName r && (not $ "wire_" `T.isPrefixOf` r)

roleToRoleName :: ConversationRole -> RoleName
roleToRoleName ConvRoleWireAdmin = roleNameWireAdmin
roleToRoleName ConvRoleWireMember = roleNameWireMember
roleToRoleName (ConvRoleCustom l _) = l

toConvRole :: RoleName -> Maybe Actions -> Maybe ConversationRole
toConvRole (RoleName "wire_admin") _ = Just ConvRoleWireAdmin
toConvRole (RoleName "wire_member") _ = Just ConvRoleWireMember
toConvRole x (Just as) = Just (ConvRoleCustom x as)
toConvRole _ _ = Nothing

roleNameToActions :: RoleName -> Maybe (Set Action)
roleNameToActions r = roleActions <$> toConvRole r Nothing

allActions :: Actions
allActions = Actions $ Set.fromList [minBound .. maxBound]

roleActions :: ConversationRole -> Set Action
roleActions ConvRoleWireAdmin = allowedActions allActions
roleActions ConvRoleWireMember =
  Set.fromList
    [ LeaveConversation
    ]
roleActions (ConvRoleCustom _ (Actions actions)) = actions
