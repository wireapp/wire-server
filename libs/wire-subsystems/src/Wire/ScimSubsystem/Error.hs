{-# LANGUAGE RecordWildCards #-}

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

module Wire.ScimSubsystem.Error where

import Data.Aeson
import Data.ByteString.Lazy as LBS
import Data.Id
import Data.Text as T
import Data.Text.Encoding as T
import Imports
import Network.Wai.Utilities.Error qualified as Wai
import Web.Scim.Schema.Error

data ScimSubsystemError
  = ScimSubsystemBadGroupName Text
  | ScimSubsystemGroupNotFound UserGroupId
  | ScimSubsystemUserNotFound UserId
  | ScimSubsystemInvalidGroupMemberId Text
  | ScimSubsystemGroupMembersNotFound [UserId]
  | ScimSubsystemForbidden UserGroupId
  | ScimSubsystemInternal Wai.Error
  deriving (Show, Eq)

scimSubsystemErrorToScimError :: ScimSubsystemError -> ScimError
scimSubsystemErrorToScimError = \case
  ScimSubsystemBadGroupName bad -> badRequest InvalidValue (Just bad)
  ScimSubsystemGroupNotFound bad -> notFound "Group" (idToText bad)
  ScimSubsystemUserNotFound bad -> notFound "User" (idToText bad)
  ScimSubsystemInvalidGroupMemberId bad -> badRequest InvalidValue (Just $ "Invalid group member ID: " <> bad)
  ScimSubsystemGroupMembersNotFound bads -> badRequest InvalidValue (Just $ "These users do not exist, are not in your team, or not \"managed_by\" = \"scim\": " <> T.intercalate ", " (idToText <$> bads))
  ScimSubsystemForbidden bad -> forbidden ("Group is not managed by SCIM: " <> idToText bad)
  ScimSubsystemInternal waiErr -> serverError (T.decodeUtf8 $ LBS.toStrict $ encode waiErr)
