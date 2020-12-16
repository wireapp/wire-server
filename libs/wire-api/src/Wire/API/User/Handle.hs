{-# LANGUAGE DerivingVia #-}
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

import Control.Lens ((.~), (?~))
import Data.Aeson
import Data.Id (UserId)
import Data.Proxy (Proxy (..))
import Data.Qualified (Qualified (..), deprecatedUnqualifiedSchemaRef)
import Data.Range
import Data.Swagger (NamedSchema (..), SwaggerType (..), ToSchema (..), declareSchemaRef, properties, type_)
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- UserHandleInfo

newtype UserHandleInfo = UserHandleInfo {userHandleId :: Qualified UserId}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

modelUserHandleInfo :: Doc.Model
modelUserHandleInfo = Doc.defineModel "UserHandleInfo" $ do
  Doc.description "User handle info"
  Doc.property "user" Doc.string' $
    Doc.description "ID of the user owning the handle"

instance ToSchema UserHandleInfo where
  declareNamedSchema _ = do
    qualifiedIdSchema <- declareSchemaRef (Proxy @(Qualified UserId))
    unqualifiedIdSchema <- deprecatedUnqualifiedSchemaRef (Proxy @UserId) "qualified_user"
    pure $
      NamedSchema
        (Just "UserHandleInfo")
        $ mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ [ ("user", unqualifiedIdSchema),
                 ("qualified_user", qualifiedIdSchema)
               ]

instance ToJSON UserHandleInfo where
  toJSON (UserHandleInfo u) =
    object
      [ "user" .= qUnqualified u, -- For backwards compatibility
        "qualified_user" .= u
      ]

instance FromJSON UserHandleInfo where
  parseJSON = withObject "UserHandleInfo" $ \o ->
    UserHandleInfo <$> o .: "qualified_user"

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

instance ToJSON CheckHandles where
  toJSON (CheckHandles l n) =
    object
      [ "handles" .= l,
        "return" .= n
      ]

instance FromJSON CheckHandles where
  parseJSON = withObject "CheckHandles" $ \o ->
    CheckHandles
      <$> o .: "handles"
      <*> o .:? "return" .!= unsafeRange 1
