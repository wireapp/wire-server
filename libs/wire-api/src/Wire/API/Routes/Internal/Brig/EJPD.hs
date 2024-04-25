-- see https://gitlab.haskell.org/ghc/ghc/-/issues/24710
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

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
    EJPDResponseItem (..),
    EJPDConvInfo (..),
    EJPDContact (..),
  )
where

import Control.Lens (makePrisms)
import Data.Aeson qualified as Aeson
import Data.Handle (Handle)
import Data.Id (ConvId, TeamId, UserId)
import Data.OpenApi qualified as OpenAPI
import Data.Qualified
import Data.Schema
import Deriving.Swagger qualified as S (CamelToSnake, CustomSwagger (..), FieldLabelModifier, StripSuffix)
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
  deriving (OpenAPI.ToSchema) via S.CustomSwagger '[S.FieldLabelModifier (S.CamelToSnake, S.StripSuffix "_body")] EJPDRequestBody

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
  deriving (Aeson.ToJSON, Aeson.FromJSON, OpenAPI.ToSchema) via Schema EJPDConvInfo

instance ToSchema EJPDConvInfo where
  schema =
    object "EJPDConvInfo" $
      EJPDConvInfo
        <$> ejpdConvName .= field "conv_name" schema
        <*> ejpdConvId .= field "conv_id" schema

data EJPDContact
  = -- | looking up the remote profile page containing this uid failed with FederationError
    EJPDContactRemoteError
      { -- ejpdContactError :: FederationError,
        -- (^ we could include that, but we don't want to import wire-federation-api, or mess
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

instance ToSchema EJPDContact where
  schema = named "EJDPContact" do
    tag _EJPDContactRemoteError remoteErrorSchema
      <> tag _EJPDContactFound contactFoundSchema
    where
      remoteErrorSchema :: ValueSchema SwaggerDoc (Qualified UserId)
      remoteErrorSchema = unnamed schema

      contactFoundSchema :: ValueSchema SwaggerDoc (Relation, EJPDResponseItem)
      contactFoundSchema = unnamed $ object "Object" do
        (,) <$> fst .= field "contact_relation" schema <*> snd .= field "contact_item" schema

deriving via Schema EJPDContact instance Aeson.ToJSON EJPDContact

deriving via Schema EJPDContact instance Aeson.FromJSON EJPDContact

deriving via Schema EJPDContact instance OpenAPI.ToSchema EJPDContact

deriving via
  S.CustomSwagger '[S.FieldLabelModifier S.CamelToSnake] EJPDResponseItem
  instance
    OpenAPI.ToSchema EJPDResponseItem

deriving via
  S.CustomSwagger '[S.FieldLabelModifier (S.CamelToSnake, S.StripSuffix "_body")] EJPDResponseBody
  instance
    OpenAPI.ToSchema EJPDResponseBody

-- FUTUREWORK(mangoiv): we probably want to write a proper schema for this; the issue here was that
-- (a, b) was serialized/ deserialized as Array with Aeson; schema-profunctor lacks support for this
-- but it should be added eventually (similaly to object, there should be a list / array combinator
-- that just wraps into Array or uses the Aeson parser to deserialize from a list)
instance ToSchema EJPDResponseItem where
  schema = mkSchema (swaggerDoc @EJPDResponseItem) (Aeson.parseJSON) (Just . Aeson.toJSON)

instance Aeson.ToJSON EJPDResponseItem where
  toJSON rspi =
    Aeson.object
      [ "ejpd_response_user_id" Aeson..= ejpdResponseUserId rspi,
        "ejpd_response_team_id" Aeson..= ejpdResponseTeamId rspi,
        "ejpd_response_name" Aeson..= ejpdResponseName rspi,
        "ejpd_response_handle" Aeson..= ejpdResponseHandle rspi,
        "ejpd_response_email" Aeson..= ejpdResponseEmail rspi,
        "ejpd_response_phone" Aeson..= ejpdResponsePhone rspi,
        "ejpd_response_push_tokens" Aeson..= ejpdResponsePushTokens rspi,
        "ejpd_response_contacts" Aeson..= ejpdResponseContacts rspi,
        "ejpd_response_team_contacts" Aeson..= ejpdResponseTeamContacts rspi,
        "ejpd_response_conversations" Aeson..= ejpdResponseConversations rspi,
        "ejpd_response_assets" Aeson..= ejpdResponseAssets rspi
      ]

instance Aeson.FromJSON EJPDResponseItem where
  parseJSON = Aeson.withObject "EJPDResponseItem" $ \obj ->
    EJPDResponseItem
      <$> obj Aeson..: "ejpd_response_user_id"
      <*> obj Aeson..:? "ejpd_response_team_id"
      <*> obj Aeson..: "ejpd_response_name"
      <*> obj Aeson..:? "ejpd_response_handle"
      <*> obj Aeson..:? "ejpd_response_email"
      <*> obj Aeson..:? "ejpd_response_phone"
      <*> obj Aeson..: "ejpd_response_push_tokens"
      <*> obj Aeson..:? "ejpd_response_contacts"
      <*> obj Aeson..:? "ejpd_response_team_contacts"
      <*> obj Aeson..:? "ejpd_response_conversations"
      <*> obj Aeson..:? "ejpd_response_assets"

instance Aeson.ToJSON EJPDRequestBody where
  toJSON (EJPDRequestBody hs) = Aeson.object ["ejpd_request" Aeson..= hs]

instance Aeson.FromJSON EJPDRequestBody where
  parseJSON = Aeson.withObject "EJPDRequestBody" $ EJPDRequestBody <$$> (Aeson..: "ejpd_request")

instance Aeson.ToJSON EJPDResponseBody where
  toJSON (EJPDResponseBody is) = Aeson.object ["ejpd_response" Aeson..= is]

instance Aeson.FromJSON EJPDResponseBody where
  parseJSON = Aeson.withObject "EJPDResponseBody" $ EJPDResponseBody <$$> (Aeson..: "ejpd_response")
