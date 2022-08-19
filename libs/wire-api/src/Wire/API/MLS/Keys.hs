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

module Wire.API.MLS.Keys
  ( MLSKeys (..),
    MLSPublicKeys (..),
    mlsKeysToPublic,
  )
where

import Crypto.PubKey.Ed25519
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteArray
import Data.Json.Util
import qualified Data.Map as Map
import Data.Schema
import qualified Data.Swagger as S
import Imports
import Wire.API.MLS.Credential

data MLSKeys = MLSKeys
  { mlsKeyPair_ed25519 :: Maybe (SecretKey, PublicKey)
  }

newtype MLSPublicKeys = MLSPublicKeys
  { unMLSPublicKeys :: Map SignatureSchemeTag ByteString
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema MLSPublicKeys

instance ToSchema MLSPublicKeys where
  schema =
    named "MLSKeys" $
      MLSPublicKeys <$> unMLSPublicKeys .= map_ base64Schema

mlsKeysToPublic :: MLSKeys -> MLSPublicKeys
mlsKeysToPublic (MLSKeys mEd25519key) =
  MLSPublicKeys $ fold $ Map.singleton Ed25519 . convert . snd <$> mEd25519key
