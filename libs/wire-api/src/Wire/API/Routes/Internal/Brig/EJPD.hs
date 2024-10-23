{-# LANGUAGE RecordWildCards #-}

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
    EJPDResponseItemRoot (..),
    EJPDResponseItemLeaf (..),
    EJPDConvInfo (..),
    EJPDContact (..),
    EJPDTeamContacts (..),
    toEJPDResponseItemLeaf,
  )
where

import Data.Aeson qualified as Aeson
import Data.Handle (Handle)
import Data.Id (ConvId, TeamId, UserId)
import Data.OpenApi qualified as OpenAPI
import Data.Qualified
import Data.Schema
import Data.Set as Set
import Imports hiding (head)
import Test.QuickCheck (Arbitrary)
import Wire.API.Connection (Relation)
import Wire.API.Team.Member (NewListType)
import Wire.API.User.Identity (EmailAddress, Phone)
import Wire.API.User.Profile (Name)
import Wire.Arbitrary (GenericUniform (..))

newtype EJPDRequestBody = EJPDRequestBody {ejpdRequestBody :: [Handle]}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDRequestBody)
  deriving (Aeson.ToJSON, Aeson.FromJSON, OpenAPI.ToSchema) via (Schema EJPDRequestBody)

newtype EJPDResponseBody = EJPDResponseBody {ejpdResponseBody :: [EJPDResponseItemRoot]}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDResponseBody)
  deriving (Aeson.ToJSON, Aeson.FromJSON, OpenAPI.ToSchema) via (Schema EJPDResponseBody)

data EJPDResponseItemRoot = EJPDResponseItemRoot
  { ejpdResponseRootUserId :: Qualified UserId,
    ejpdResponseRootTeamId :: Maybe TeamId,
    ejpdResponseRootName :: Name,
    ejpdResponseRootHandle :: Maybe Handle,
    ejpdResponseRootEmail :: Maybe EmailAddress,
    ejpdResponseRootPhone :: Maybe Phone,
    ejpdResponseRootPushTokens :: Set Text, -- 'Wire.API.Push.V2.Token.Token', but that would produce an orphan instance.
    ejpdResponseRootContacts :: Maybe (Set EJPDContact),
    ejpdResponseRootTeamContacts :: Maybe EJPDTeamContacts,
    ejpdResponseRootConversations :: Maybe (Set EJPDConvInfo),
    ejpdResponseRootAssets :: Maybe (Set Text) -- urls pointing to s3 resources
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDResponseItemRoot)

data EJPDResponseItemLeaf = EJPDResponseItemLeaf
  { ejpdResponseLeafUserId :: Qualified UserId,
    ejpdResponseLeafTeamId :: Maybe TeamId,
    ejpdResponseLeafName :: Name,
    ejpdResponseLeafHandle :: Maybe Handle,
    ejpdResponseLeafEmail :: Maybe EmailAddress,
    ejpdResponseLeafPhone :: Maybe Phone,
    ejpdResponseLeafPushTokens :: Set Text, -- 'Wire.API.Push.V2.Token.Token', but that would produce an orphan instance.
    ejpdResponseLeafConversations :: Maybe (Set EJPDConvInfo),
    ejpdResponseLeafAssets :: Maybe (Set Text) -- urls pointing to s3 resources
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDResponseItemLeaf)

data EJPDContact
  = -- | local or remote contact with relation
    EJPDContactFound
    { ejpdContactRelation :: Relation,
      ejpdContactFound :: EJPDResponseItemLeaf
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDContact)
  deriving (Aeson.ToJSON, Aeson.FromJSON, OpenAPI.ToSchema) via Schema EJPDContact

data EJPDTeamContacts = EJPDTeamContacts
  { ejpdTeamContacts :: Set EJPDResponseItemLeaf,
    ejpdTeamContactsListType :: NewListType
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDTeamContacts)

data EJPDConvInfo = EJPDConvInfo {ejpdConvName :: Text, ejpdConvId :: Qualified ConvId}
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EJPDConvInfo)
  deriving (Aeson.ToJSON, Aeson.FromJSON, OpenAPI.ToSchema) via Schema EJPDConvInfo

----------------------------------------------------------------------

toEJPDResponseItemLeaf :: EJPDResponseItemRoot -> EJPDResponseItemLeaf
toEJPDResponseItemLeaf EJPDResponseItemRoot {..} =
  EJPDResponseItemLeaf
    { ejpdResponseLeafUserId = ejpdResponseRootUserId,
      ejpdResponseLeafTeamId = ejpdResponseRootTeamId,
      ejpdResponseLeafName = ejpdResponseRootName,
      ejpdResponseLeafHandle = ejpdResponseRootHandle,
      ejpdResponseLeafEmail = ejpdResponseRootEmail,
      ejpdResponseLeafPhone = ejpdResponseRootPhone,
      ejpdResponseLeafPushTokens = ejpdResponseRootPushTokens,
      ejpdResponseLeafConversations = ejpdResponseRootConversations,
      ejpdResponseLeafAssets = ejpdResponseRootAssets
    }

----------------------------------------------------------------------

instance ToSchema EJPDRequestBody where
  schema = object "EJPDRequestBody" do
    EJPDRequestBody <$> ejpdRequestBody .= field "EJPDRequest" (array schema)

instance ToSchema EJPDResponseBody where
  schema = object "EJPDResponseBody" do
    EJPDResponseBody <$> ejpdResponseBody .= field "EJPDResponse" (array schema)

instance ToSchema EJPDResponseItemRoot where
  schema = object "EJPDResponseItemRoot" do
    EJPDResponseItemRoot
      <$> ejpdResponseRootUserId .= field "UserId" schema
      <*> ejpdResponseRootTeamId .= maybe_ (optField "TeamId" schema)
      <*> ejpdResponseRootName .= field "Name" schema
      <*> ejpdResponseRootHandle .= maybe_ (optField "Handle" schema)
      <*> ejpdResponseRootEmail .= maybe_ (optField "Email" schema)
      <*> ejpdResponseRootPhone .= maybe_ (optField "Phone" schema)
      <*> (Set.toList . ejpdResponseRootPushTokens) .= (Set.fromList <$> field "PushTokens" (array schema))
      <*> (fmap Set.toList . ejpdResponseRootContacts) .= (Set.fromList <$$> maybe_ (optField "Contacts" (array schema)))
      <*> ejpdResponseRootTeamContacts .= maybe_ (optField "TeamContacts" schema)
      <*> (fmap Set.toList . ejpdResponseRootConversations) .= (Set.fromList <$$> maybe_ (optField "Conversations" (array schema)))
      <*> (fmap Set.toList . ejpdResponseRootAssets) .= (Set.fromList <$$> maybe_ (optField "Assets" (array schema)))

instance ToSchema EJPDResponseItemLeaf where
  schema = object "EJPDResponseItemLeaf" do
    EJPDResponseItemLeaf
      <$> ejpdResponseLeafUserId .= field "UserId" schema
      <*> ejpdResponseLeafTeamId .= maybe_ (optField "TeamId" schema)
      <*> ejpdResponseLeafName .= field "Name" schema
      <*> ejpdResponseLeafHandle .= maybe_ (optField "Handle" schema)
      <*> ejpdResponseLeafEmail .= maybe_ (optField "Email" schema)
      <*> ejpdResponseLeafPhone .= maybe_ (optField "Phone" schema)
      <*> (Set.toList . ejpdResponseLeafPushTokens) .= (Set.fromList <$> field "PushTokens" (array schema))
      <*> (fmap Set.toList . ejpdResponseLeafConversations) .= (Set.fromList <$$> maybe_ (optField "Conversations" (array schema)))
      <*> (fmap Set.toList . ejpdResponseLeafAssets) .= (Set.fromList <$$> maybe_ (optField "Assets" (array schema)))

instance ToSchema EJPDContact where
  schema =
    object "EJDPContact" do
      EJPDContactFound
        <$> ejpdContactRelation .= field "contact_relation" schema
        <*> ejpdContactFound .= field "contact_item" schema

instance ToSchema EJPDTeamContacts where
  schema = object "EJPDTeamContacts" do
    EJPDTeamContacts
      <$> (Set.toList . ejpdTeamContacts) .= (Set.fromList <$> field "TeamContacts" (array schema))
      <*> ejpdTeamContactsListType .= field "ListType" schema

instance ToSchema EJPDConvInfo where
  schema =
    object "EJPDConvInfo" $
      EJPDConvInfo
        <$> ejpdConvName .= field "conv_name" schema
        <*> ejpdConvId .= field "conv_id" schema
