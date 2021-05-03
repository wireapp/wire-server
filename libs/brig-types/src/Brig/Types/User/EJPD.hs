{-# LANGUAGE DerivingVia #-}

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

-- | Identify users for law enforcement.  (Wire has legal requirements to cooperate with the
-- authorities.  The wire backend operations team uses this to answer identification requests
-- manually.)
module Brig.Types.User.EJPD
  ( EJPDRequestBody (EJPDRequestBody, ejpdRequestBody),
    EJPDResponseBody (EJPDResponseBody, ejpdResponseBody),
    EJPDResponseItem (EJPDResponseItem, ejpdResponseHandle, ejpdResponsePushTokens, ejpdResponseContacts),
  )
where

import Data.Aeson hiding (json)
import Data.Handle (Handle)
import Data.Id (TeamId, UserId)
import Data.Swagger (ToSchema)
import Deriving.Swagger (CamelToSnake, CustomSwagger (..), FieldLabelModifier, StripSuffix)
import Imports hiding (head)
import Test.QuickCheck (Arbitrary)
import Wire.API.Arbitrary (GenericUniform (..))
import Wire.API.Connection (Relation)
import Wire.API.Team.Member (NewListType)
import Wire.API.User.Identity (Email, Phone)
import Wire.API.User.Profile (Name)

newtype EJPDRequestBody = EJPDRequestBody {ejpdRequestBody :: [Handle]}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDRequestBody)
  deriving (ToSchema) via CustomSwagger '[FieldLabelModifier (CamelToSnake, StripSuffix "_body")] EJPDRequestBody

newtype EJPDResponseBody = EJPDResponseBody {ejpdResponseBody :: [EJPDResponseItem]}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDResponseBody)
  deriving (ToSchema) via CustomSwagger '[FieldLabelModifier (CamelToSnake, StripSuffix "_body")] EJPDResponseBody

data EJPDResponseItem = EJPDResponseItem
  { ejpdResponseUserId :: UserId,
    ejpdResponseTeamId :: Maybe TeamId,
    ejpdResponseName :: Name,
    ejpdResponseHandle :: Maybe Handle,
    ejpdResponseEmail :: Maybe Email,
    ejpdResponsePhone :: Maybe Phone,
    ejpdResponsePushTokens :: Set Text, -- 'Wire.API.Push.V2.Token.Token', but that would produce an orphan instance.
    ejpdResponseContacts :: Maybe (Set (Relation, EJPDResponseItem)),
    ejpdResponseTeamContacts :: Maybe (Set EJPDResponseItem, NewListType)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDResponseItem)
  deriving (ToSchema) via CustomSwagger '[FieldLabelModifier CamelToSnake] EJPDResponseItem

instance ToJSON EJPDRequestBody where
  toJSON (EJPDRequestBody hs) = object ["ejpd_request" .= hs]

instance FromJSON EJPDRequestBody where
  parseJSON = withObject "EJPDRequestBody" $ EJPDRequestBody <$$> (.: "ejpd_request")

instance ToJSON EJPDResponseBody where
  toJSON (EJPDResponseBody is) = object ["ejpd_response" .= is]

instance FromJSON EJPDResponseBody where
  parseJSON = withObject "EJPDResponseBody" $ EJPDResponseBody <$$> (.: "ejpd_response")

instance ToJSON EJPDResponseItem where
  toJSON rspi =
    object
      [ "ejpd_response_user_id" .= ejpdResponseUserId rspi,
        "ejpd_response_team_id" .= ejpdResponseTeamId rspi,
        "ejpd_response_name" .= ejpdResponseName rspi,
        "ejpd_response_handle" .= ejpdResponseHandle rspi,
        "ejpd_response_email" .= ejpdResponseEmail rspi,
        "ejpd_response_phone" .= ejpdResponsePhone rspi,
        "ejpd_response_push_tokens" .= ejpdResponsePushTokens rspi,
        "ejpd_response_contacts" .= ejpdResponseContacts rspi,
        "ejpd_response_team_contacts" .= ejpdResponseTeamContacts rspi
      ]

instance FromJSON EJPDResponseItem where
  parseJSON = withObject "EJPDResponseItem" $ \obj ->
    EJPDResponseItem
      <$> obj .: "ejpd_response_user_id"
      <*> obj .:? "ejpd_response_team_id"
      <*> obj .: "ejpd_response_name"
      <*> obj .:? "ejpd_response_handle"
      <*> obj .:? "ejpd_response_email"
      <*> obj .:? "ejpd_response_phone"
      <*> obj .: "ejpd_response_push_tokens"
      <*> obj .:? "ejpd_response_contacts"
      <*> obj .:? "ejpd_response_team_contacts"
