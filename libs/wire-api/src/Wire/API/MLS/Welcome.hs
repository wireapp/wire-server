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

module Wire.API.MLS.Welcome where

import qualified Data.Swagger as S
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

data Welcome = Welcome
  { welCipherSuite :: CipherSuite,
    welSecrets :: [GroupSecrets],
    welGroupInfo :: ByteString
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform Welcome)

instance S.ToSchema Welcome where
  declareNamedSchema _ = pure (mlsSwagger "Welcome")

instance ParseMLS Welcome where
  parseMLS =
    Welcome
      <$> parseMLS
      <*> parseMLSVector @VarInt parseMLS
      <*> parseMLSBytes @VarInt

instance SerialiseMLS Welcome where
  serialiseMLS (Welcome cs ss gi) = do
    serialiseMLS cs
    serialiseMLSVector @VarInt serialiseMLS ss
    serialiseMLSBytes @VarInt gi

data GroupSecrets = GroupSecrets
  { gsNewMember :: KeyPackageRef,
    gsSecrets :: HPKECiphertext
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform GroupSecrets)

instance ParseMLS GroupSecrets where
  parseMLS = GroupSecrets <$> parseMLS <*> parseMLS

instance SerialiseMLS GroupSecrets where
  serialiseMLS (GroupSecrets kp sec) = do
    serialiseMLS kp
    serialiseMLS sec
