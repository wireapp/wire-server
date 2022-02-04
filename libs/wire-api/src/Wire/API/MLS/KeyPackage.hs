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

module Wire.API.MLS.KeyPackage where

import Data.Aeson (FromJSON)
import Data.Binary
import Data.Json.Util
import Data.Schema
import qualified Data.Swagger as S
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.Serialisation

data KeyPackageUpload = KeyPackageUpload
  {kpuKeyPackages :: [KeyPackageData]}
  deriving (FromJSON, S.ToSchema) via Schema KeyPackageUpload

instance ToSchema KeyPackageUpload where
  schema =
    object "KeyPackageUpload" $
      KeyPackageUpload
        <$> kpuKeyPackages .= field "key_packages" (array schema)

newtype KeyPackageData = KeyPackageData {kpData :: Base64ByteString}

instance ToSchema KeyPackageData where
  schema =
    KeyPackageData <$> kpData
      .= named "KeyPackage" base64Schema

--------------------------------------------------------------------------------

data ProtocolVersion = ProtocolReserved | ProtocolMLS
  deriving stock (Enum)
  deriving (Binary) via EnumBinary Word8 ProtocolVersion

data Extension = Extension
  { extType :: Word16,
    extData :: ByteString
  }
  deriving (Generic)

instance Binary Extension

data KeyPackageTBS = KeyPackageTBS
  { kpProtocolVersion :: ProtocolVersion,
    kpCipherSuite :: CipherSuite,
    kpInitKey :: ByteString,
    kpCredential :: Credential,
    kpExtensions :: [Extension]
  }
  deriving (Generic)

instance Binary KeyPackageTBS

data KeyPackage = KeyPackage
  { kpTBS :: KeyPackageTBS,
    kpSignature :: ByteString
  }
  deriving (Generic)
  deriving (ParseMLS) via BinaryMLS KeyPackage

instance Binary KeyPackage
