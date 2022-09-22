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

module Wire.API.MLS.GroupInfoBundle where

import Data.Binary.Put
import qualified Data.Swagger as S
import Imports
import Servant.API.ContentTypes
import Test.QuickCheck
import Wire.API.MLS.PublicGroupState
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Servant
import Wire.Arbitrary

data GroupInfoEncryption = UnencryptedGroupInfo | JweEncryptedGroupInfo
  deriving stock (Eq, Show, Generic, Bounded, Enum)
  deriving (Arbitrary) via (GenericUniform GroupInfoEncryption)

data GroupInfoTreeType = TreeFull | TreeDelta | TreeByRef
  deriving stock (Eq, Show, Generic, Bounded, Enum)
  deriving (Arbitrary) via (GenericUniform GroupInfoTreeType)

data GroupInfoBundle = GroupInfoBundle
  { gipEncryptionType :: GroupInfoEncryption,
    gipTreeType :: GroupInfoTreeType,
    gipGroupState :: PublicGroupState
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform GroupInfoBundle

instance ParseMLS GroupInfoBundle where
  parseMLS =
    GroupInfoBundle
      <$> parseMLSEnum @Word8 "GroupInfoEncryptionEnum"
      <*> parseMLSEnum @Word8 "RatchetTreeEnum"
      <*> parseMLS

instance SerialiseMLS GroupInfoBundle where
  serialiseMLS (GroupInfoBundle e t pgs) = do
    serialiseMLSEnum @Word8 e
    serialiseMLSEnum @Word8 t
    serialiseMLS pgs

instance S.ToSchema GroupInfoBundle where
  declareNamedSchema _ = pure (mlsSwagger "GroupInfoBundle")

instance MimeRender MLS GroupInfoBundle where
  mimeRender _ = runPut . serialiseMLS
