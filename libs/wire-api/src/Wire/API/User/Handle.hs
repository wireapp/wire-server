{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}

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

import Control.Applicative
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import Data.Id (UserId)
import Data.Qualified (Qualified (..), deprecatedSchema)
import Data.Range
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))
import Data.Singletons

--------------------------------------------------------------------------------
-- UserHandleInfo

newtype UserHandleInfo = UserHandleInfo {userHandleId :: Qualified UserId}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema UserHandleInfo

modelUserHandleInfo :: Doc.Model
modelUserHandleInfo = Doc.defineModel "UserHandleInfo" $ do
  Doc.description "User handle info"
  Doc.property "user" Doc.string' $
    Doc.description "ID of the user owning the handle"

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

modelCheckHandles :: Doc.Model
modelCheckHandles = Doc.defineModel "CheckHandles" $ do
  Doc.description "Check availability of user handles."
  Doc.property "handles" (Doc.array Doc.string') $
    Doc.description "The prioritised list of handles to check (up to 50)"
  Doc.property "return" Doc.int32' $ do
    Doc.description "Desired number of free handles to return (1 - 10). Default 1."
    Doc.optional

instance ToSchema CheckHandles where
  schema =
    object "CheckHandles" $
      CheckHandles
        <$> (checkHandlesList .= field "handles" (fromRange .= rangedSchema sing sing (array schema)))
        <*> (checkHandlesNum .= field "return" schema)

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
