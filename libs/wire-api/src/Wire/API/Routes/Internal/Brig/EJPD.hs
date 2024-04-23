{-# LANGUAGE TemplateHaskell #-}

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

-- | Identify users for law enforcement.  (Wire has legal requirements to cooperate with the
-- authorities.  The wire backend operations team uses this to answer identification requests
-- manually.)
module Wire.API.Routes.Internal.Brig.EJPD
  ( EJPDRequestBody (EJPDRequestBody, ejpdRequestBody),
    EJPDResponseBody (EJPDResponseBody, ejpdResponseBody),
    EJPDResponseItem
      ( EJPDResponseItem,
        ejpdResponseUserId,
        ejpdResponseTeamId,
        ejpdResponseName,
        ejpdResponseHandle,
        ejpdResponseEmail,
        ejpdResponsePhone,
        ejpdResponsePushTokens,
        ejpdResponseContacts,
        ejpdResponseTeamContacts,
        ejpdResponseConversations,
        ejpdResponseAssets
      ),
    EJPDConvInfo (..),
    EJPDContact,
  )
where

import Control.Lens (makePrisms)
import Data.Aeson hiding (json)
import Data.Handle (Handle)
import Data.Id (ConvId, TeamId, UserId)
import Data.OpenApi (ToSchema)
import Data.Qualified
import Data.Schema qualified as S
import Deriving.Swagger (CamelToSnake, CustomSwagger (..), FieldLabelModifier, StripSuffix)
import Imports hiding (head)
import Test.QuickCheck (Arbitrary)
import Wire.API.Connection (Relation)
import Wire.API.Team.Member (NewListType)
import Wire.API.User.Identity (Email, Phone)
import Wire.API.User.Profile (Name)
import Wire.Arbitrary (GenericUniform (..))

newtype EJPDRequestBody = EJPDRequestBody {ejpdRequestBody :: [Handle]}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDRequestBody)
  deriving (ToSchema) via CustomSwagger '[FieldLabelModifier (CamelToSnake, StripSuffix "_body")] EJPDRequestBody

newtype EJPDResponseBody = EJPDResponseBody {ejpdResponseBody :: [EJPDResponseItem]}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDResponseBody)

data EJPDResponseItem = EJPDResponseItem
  { ejpdResponseUserId :: Qualified UserId,
    ejpdResponseTeamId :: Maybe TeamId,
    ejpdResponseName :: Name,
    ejpdResponseHandle :: Maybe Handle,
    ejpdResponseEmail :: Maybe Email,
    ejpdResponsePhone :: Maybe Phone,
    ejpdResponsePushTokens :: Set Text, -- 'Wire.API.Push.V2.Token.Token', but that would produce an orphan instance.
    ejpdResponseContacts :: Maybe (Set EJPDContact),
    ejpdResponseTeamContacts :: Maybe (Set EJPDResponseItem, NewListType),
    ejpdResponseConversations :: Maybe (Set EJPDConvInfo),
    ejpdResponseAssets :: Maybe (Set Text) -- urls pointing to s3 resources
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDResponseItem)

data EJPDConvInfo = EJPDConvInfo {ejpdConvName :: Text, ejpdConvId :: Qualified ConvId}
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDConvInfo)
  deriving (ToJSON, FromJSON, ToSchema) via S.Schema EJPDConvInfo

instance S.ToSchema EJPDConvInfo where
  schema =
    S.object "EJPDConvInfo" $
      EJPDConvInfo
        <$> ejpdConvName S..= S.field "conv_name" S.schema
        <*> ejpdConvId S..= S.field "conv_id" S.schema

data EJPDContact
  = -- | looking up the remote profile page containing this uid failed with FederationError
    EJPDContactRemoteError
      { -- ejpdContactError :: FederationError,
        -- (^ we could do that, but we don't want to import wire-federation-api, or mess
        -- around with type arguments all over the place.)
        ejpdContactErrorUid :: Qualified UserId
      }
  | -- | local or remote contact with relation
    EJPDContactFound
      { ejpdContactRelation :: Relation,
        ejpdContactFound :: EJPDResponseItem
      }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDContact)

$(makePrisms ''EJPDContact)

instance S.ToSchema EJPDContact where
  schema =
    -- do it as in taggedEventDataSchema
    undefined

deriving via S.Schema EJPDContact instance ToJSON EJPDContact

deriving via S.Schema EJPDContact instance FromJSON EJPDContact

deriving via S.Schema EJPDContact instance ToSchema EJPDContact

deriving via CustomSwagger '[FieldLabelModifier CamelToSnake] EJPDResponseItem instance ToSchema EJPDResponseItem

deriving via CustomSwagger '[FieldLabelModifier (CamelToSnake, StripSuffix "_body")] EJPDResponseBody instance ToSchema EJPDResponseBody

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
        "ejpd_response_team_contacts" .= ejpdResponseTeamContacts rspi,
        "ejpd_response_conversations" .= ejpdResponseConversations rspi,
        "ejpd_response_assets" .= ejpdResponseAssets rspi
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
      <*> obj .:? "ejpd_response_conversations"
      <*> obj .:? "ejpd_response_assets"

instance ToJSON EJPDRequestBody where
  toJSON (EJPDRequestBody hs) = object ["ejpd_request" .= hs]

instance FromJSON EJPDRequestBody where
  parseJSON = withObject "EJPDRequestBody" $ EJPDRequestBody <$$> (.: "ejpd_request")

instance ToJSON EJPDResponseBody where
  toJSON (EJPDResponseBody is) = object ["ejpd_response" .= is]

instance FromJSON EJPDResponseBody where
  parseJSON = withObject "EJPDResponseBody" $ EJPDResponseBody <$$> (.: "ejpd_response")
