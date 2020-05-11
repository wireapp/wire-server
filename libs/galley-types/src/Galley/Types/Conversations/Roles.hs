{-# LANGUAGE OverloadedStrings #-}

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
-- "Galley.Types.Teams".
module Galley.Types.Conversations.Roles
  ( isActionAllowed,
    roleNameToActions,

    -- * re-exports
    ConversationRole,
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
  )
where

import qualified Data.Text as T
import Imports
import Wire.API.Conversation.Role

-- | Given an action and a RoleName, three possible outcomes:
-- Just True:  Yes, the action is allowed
-- Just False: No, the action is not allowed
-- Nothing:    Not enough information, this is a custom role
isActionAllowed :: Action -> RoleName -> Maybe Bool
isActionAllowed action rn
  | isCustomRoleName rn = Nothing
  | otherwise = pure $ maybe False (action `elem`) (roleNameToActions rn)

-- | Custom RoleNames _must not_ start with `wire_`
isCustomRoleName :: RoleName -> Bool
isCustomRoleName (fromRoleName -> r) = isValidRoleName r && (not $ "wire_" `T.isPrefixOf` r)

roleNameToActions :: RoleName -> Maybe (Set Action)
roleNameToActions r = roleActions <$> toConvRole r Nothing
