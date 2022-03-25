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

import Control.Lens
import qualified Data.Swagger as S
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Extension
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation

data Welcome = Welcome
  { welCipherSuite :: CipherSuite,
    welSecrets :: [GroupSecrets],
    welGroupInfo :: ByteString
  }

instance S.ToSchema Welcome where
  declareNamedSchema _ =
    pure . S.NamedSchema (Just "Welcome") $
      mempty
        & S.description
          ?~ "This object can only be parsed in TLS format. \
             \Please refer to the MLS specification for details."

instance ParseMLS Welcome where
  parseMLS =
    Welcome
      -- Note: the extra protocol version at the beginning of the welcome
      -- message is present in openmls-0.4.0-pre, but is not part of the spec
      <$> (parseMLS @ProtocolVersion *> parseMLS)
      <*> parseMLSVector @Word32 parseMLS
      <*> parseMLSBytes @Word32

data GroupSecrets = GroupSecrets
  { gsNewMember :: KeyPackageRef,
    gsSecrets :: HPKECiphertext
  }

instance ParseMLS GroupSecrets where
  parseMLS = GroupSecrets <$> parseMLS <*> parseMLS
