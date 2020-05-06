{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

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

module Wire.API.User.Handle
  ( UserHandleInfo (..),
    CheckHandles (..),

    -- * Swagger
    modelUserHandleInfo,
    modelCheckHandles,
  )
where

import Data.Aeson
import Data.Id (UserId)
import Data.Range
import qualified Data.Swagger.Build.Api as Doc
import Imports

--------------------------------------------------------------------------------
-- UserHandleInfo

newtype UserHandleInfo = UserHandleInfo {userHandleId :: UserId}
  deriving (Eq, Show, Generic)

modelUserHandleInfo :: Doc.Model
modelUserHandleInfo = Doc.defineModel "UserHandleInfo" $ do
  Doc.description "User handle info"
  Doc.property "user" Doc.string' $
    Doc.description "ID of the user owning the handle"

instance ToJSON UserHandleInfo where
  toJSON (UserHandleInfo u) =
    object
      ["user" .= u]

instance FromJSON UserHandleInfo where
  parseJSON = withObject "UserHandleInfo" $ \o ->
    UserHandleInfo <$> o .: "user"

--------------------------------------------------------------------------------
-- CheckHandles

-- | Check the availability of user handles.
data CheckHandles = CheckHandles
  { -- | Handles to check for availability, in ascending order of preference.
    checkHandlesList :: Range 1 50 [Text],
    -- | Number of free handles to return. Default 1.
    checkHandlesNum :: Range 1 10 Word
  }
  deriving (Eq, Show, Generic)

modelCheckHandles :: Doc.Model
modelCheckHandles = Doc.defineModel "CheckHandles" $ do
  Doc.description "Check availability of user handles."
  Doc.property "handles" (Doc.array Doc.string') $
    Doc.description "The prioritised list of handles to check (up to 50)"
  Doc.property "return" Doc.int32' $ do
    Doc.description "Desired number of free handles to return (1 - 10). Default 1."
    Doc.optional

instance ToJSON CheckHandles where
  toJSON (CheckHandles l n) =
    object
      [ "handles" .= l,
        "return" .= n
      ]

instance FromJSON CheckHandles where
  parseJSON = withObject "CheckHandles" $ \o ->
    CheckHandles <$> o .: "handles"
      <*> o .:? "return" .!= unsafeRange 1
