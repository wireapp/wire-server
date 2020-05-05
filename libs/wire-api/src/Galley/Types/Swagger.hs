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

module Galley.Types.Swagger where

import qualified Data.Swagger.Build.Api as Doc
import Imports
import qualified Wire.Swagger as Swagger

-- FUTUREWORK: unused? OtherMemberUpdateData doesn't exist.
modelOtherMemberUpdateData :: Doc.Model
modelOtherMemberUpdateData = Doc.defineModel "OtherMemberUpdateData" $ do
  Doc.description "Event data on other member updates"
  Doc.property "target" Doc.bytes' $ do
    Doc.description "Target ID of the user that the action was performed on"
    Doc.optional
  Doc.property "conversation_role" Doc.string' $ do
    Doc.description "Name of the conversation role to update to"
    Doc.optional

modelErrorObj :: Doc.Model
modelErrorObj = Swagger.errorModel
