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

module Wire.API.FederationStatus
  ( FederationStatus (..),
    RemoteDomains (..),
  )
where

import Control.Applicative
import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Domain
import Data.OpenApi qualified as S
import Data.Qualified
import Data.Schema
import Data.Set qualified as Set
import Imports
import Wire.Arbitrary

newtype RemoteDomains = RemoteDomains
  { rdDomains :: Set (Remote ())
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RemoteDomains)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema RemoteDomains

instance ToSchema RemoteDomains where
  schema =
    objectWithDocModifier "RemoteDomains" (description ?~ "A set of remote domains") $
      RemoteDomains
        <$> (Set.map tDomain . rdDomains)
          .= field "domains" (Set.map (flip toRemoteUnsafe ()) <$> set schema)

-- | This value expresses if the requested remote domains are fully connected or not.
-- If not the response will contain the first pair of domains found
-- which do not federate with each other.
data FederationStatus
  = FullyConnected
  | NotConnectedDomains Domain Domain
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform FederationStatus)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema FederationStatus

instance ToSchema FederationStatus where
  schema = mkSchema descr toFederationStatus fromFederationStatus
    where
      toFederationStatus :: A.Value -> A.Parser FederationStatus
      toFederationStatus = A.withObject "FederationStatus" $ \o -> do
        status :: Text <- o .: "status"
        case status of
          "fully-connected" -> pure FullyConnected
          "non-fully-connected" -> do
            domains <- o .: "not_connected"
            case domains of
              [a, b] -> pure $ NotConnectedDomains a b
              _ -> A.parseFail "Expected exactly two domains in field not_connected."
          _ -> A.parseFail "Expected field status to be 'fully-connected' or 'non-fully-connected'."

      fromFederationStatus :: FederationStatus -> Maybe A.Value
      fromFederationStatus = \case
        FullyConnected -> Just $ A.object ["status" A..= ("fully-connected" :: Text)]
        NotConnectedDomains a b -> Just $ A.object ["status" A..= ("non-fully-connected" :: Text), "not_connected" A..= [a, b]]

      descr :: NamedSwaggerDoc
      descr =
        swaggerDoc @Text
          & S.schema . S.description
            ?~ "This value expresses if the requested remote domains are fully connected or not. \
               \If not, it contains exactly two remote domains which do not federate with each other."
          & S.schema . S.example ?~ "{ \"status\": \"fully-connected\", \"not_connected\": [] }"
          & S.schema . S.example ?~ "{ \"status\": \"non-fully-connected\", \"not_connected\": [\"d.example.com\", \"e.example.com\"] }"
