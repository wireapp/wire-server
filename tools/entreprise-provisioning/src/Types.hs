-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Types where

import Data.Aeson
import Data.Id
import Data.Range
import GHC.Generics
import Imports

newtype Token = Token {fromToken :: Text}
  deriving stock (Eq, Show, Generic)

newtype ApiUrl = ApiUrl {fromApiUrl :: String}
  deriving stock (Eq, Generic)
  deriving newtype (Show)

newtype ChannelName = ChannelName {fromChannelName :: Range 1 256 Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype UserGroupChannelsProvisionningSpec = UserGroupChannelsProvisionningSpec
  { byGroup :: Map UserGroupId [ChannelName]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype UserGroupChannelsProvisionningResult = UserGroupChannelsProvisionningResult
  { results :: Map UserGroupId UserGroupResult
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data UserGroupResult = UserGroupResult
  { channel :: [ChannelResult],
    association :: AssociationResult
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Generically UserGroupResult)

data ChannelResult
  = ChannelSuccess ChannelName ConvId
  | ChannelFailure ChannelName ErrorDetail
  deriving stock (Eq, Show, Generic)

channelResultName :: ChannelResult -> ChannelName
channelResultName (ChannelSuccess n _) = n
channelResultName (ChannelFailure n _) = n

instance ToJSON ChannelResult where
  toJSON (ChannelSuccess n cid) =
    object
      [ "name" .= n,
        "id" .= cid
      ]
  toJSON (ChannelFailure n f) =
    object
      [ "name" .= n,
        "failure" .= f
      ]

instance FromJSON ChannelResult where
  parseJSON = withObject "ChannelResult" $ \o -> do
    name <- o .: "name"
    mId <- o .:? "id"
    mFailure <- o .:? "failure"
    case (mId, mFailure) of
      (Just cid, Nothing) -> pure $ ChannelSuccess name cid
      (Nothing, Just f) -> pure $ ChannelFailure name f
      _ -> fail "ChannelResult must have either 'id' or 'failure' field"

data AssociationResult
  = AssociationSuccess
  | AssociationFailureResult ErrorDetail
  deriving stock (Eq, Show, Generic)

instance ToJSON AssociationResult where
  toJSON AssociationSuccess =
    object ["success" .= True]
  toJSON (AssociationFailureResult det) =
    object
      [ "success" .= False,
        "detail" .= det
      ]

instance FromJSON AssociationResult where
  parseJSON = withObject "AssociationResult" $ \o -> do
    success <- o .: "success"
    if success
      then pure AssociationSuccess
      else AssociationFailureResult <$> o .: "detail"

data ErrorDetail = ErrorDetail
  { status :: Int,
    response :: Value
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via (Generically ErrorDetail)
