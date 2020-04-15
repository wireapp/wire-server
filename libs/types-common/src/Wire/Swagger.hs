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

module Wire.Swagger where

import Data.Swagger.Build.Api
import Imports hiding (max, min)

errorModel :: Model
errorModel = defineModel "Error" $ do
  description "Basic error information"
  errorProperties

errorProperties :: ModelBuilder
errorProperties = do
  property "code" int32' $
    description "HTTP status code"
  property "label" string' $
    description "Textual classifier for programmatic consumption."
  property "message" string' $
    description "More detailed error description."

int32Between :: Int32 -> Int32 -> DataType
int32Between m n = int32 (min m . max n)
