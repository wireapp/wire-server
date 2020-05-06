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

module Brig.Types.Swagger where

import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.Swagger (errorProperties)

brigModels :: [Doc.Model]
brigModels =
  [ -- Login / Authentication
    modelPendingLoginError -- TODO: couldn't find a corresponding type
  ]

-------------------------------------------------------------------------------
-- Login / Authentication Models

modelPendingLoginError :: Doc.Model
modelPendingLoginError = Doc.defineModel "PendingLoginError" $ do
  Doc.description "A login code is still pending."
  errorProperties
  Doc.property "expires_in" Doc.int32' $
    Doc.description "Number of seconds before the pending login code expires."
