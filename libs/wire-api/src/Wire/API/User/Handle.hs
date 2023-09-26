{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
  )
where

import Control.Applicative
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Id (UserId)
import Data.OpenApi qualified as S
import Data.Qualified (Qualified (..), deprecatedSchema)
import Data.Range
import Data.Schema
import Imports
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- UserHandleInfo

newtype UserHandleInfo = UserHandleInfo {userHandleId :: Qualified UserId}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema UserHandleInfo

instance ToSchema UserHandleInfo where
  schema =
    object "UserHandleInfo" $
      UserHandleInfo
        <$> userHandleId .= field "qualified_user" schema
        <* (qUnqualified . userHandleId)
          .= optional (field "user" (deprecatedSchema "qualified_user" schema))

--------------------------------------------------------------------------------
-- CheckHandles

-- | Check the availability of user handles.
data CheckHandles = CheckHandles
  { -- | Handles to check for availability, in ascending order of preference.
    checkHandlesList :: Range 1 50 [Text],
    -- | Number of free handles to return. Default 1.
    checkHandlesNum :: Range 1 10 Word
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CheckHandles)
  deriving (S.ToSchema) via Schema CheckHandles

instance ToJSON CheckHandles where
  toJSON (CheckHandles l n) =
    A.object
      [ "handles" A..= l,
        "return" A..= n
      ]

instance FromJSON CheckHandles where
  parseJSON = A.withObject "CheckHandles" $ \o ->
    CheckHandles
      <$> o A..: "handles"
      <*> o A..:? "return" A..!= unsafeRange 1

instance ToSchema CheckHandles where
  schema =
    object "CheckHandles" $
      CheckHandles
        <$> checkHandlesList .= field "handles" (fromRange .= rangedSchema (array schema))
        <*> checkHandlesNum .= field "return" schema
