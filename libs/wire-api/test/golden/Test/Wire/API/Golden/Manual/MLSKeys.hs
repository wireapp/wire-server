-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.MLSKeys where

import Data.Json.Util
import Wire.API.MLS.Keys

testObject_MLSPublicKeys1 :: MLSPublicKeys
testObject_MLSPublicKeys1 =
  MLSKeys
    { ed25519 =
        MLSPublicKey
          (fromBase64TextLenient "7C8PpP91rzMnD4VHuWTI3yNuInfbzIk937uF0Cg/Piw="),
      ecdsa_secp256r1_sha256 =
        MLSPublicKey
          (fromBase64TextLenient "ArUTSywmqya1wAGwrK+pJuA7KSpKm06y3eZq8Py2NMM="),
      ecdsa_secp384r1_sha384 =
        MLSPublicKey
          (fromBase64TextLenient "7pKiTLf72OfpQIeVeXF0mJKfWsBnhTtMUy0zuKasYjlTQUW5fGtcyAFXinM3FahV"),
      ecdsa_secp521r1_sha512 =
        MLSPublicKey
          (fromBase64TextLenient "9twvhZ57ytiujWXFtSmxd8I5r9iZjgdCtGtReJT3yQL2BCGZ80Vzq/MrmV+O0i7lZEI1gqbr8vL1xKk+2h2LyQ==")
    }

testObject_MLSKeysByPurpose1 :: MLSKeysByPurpose MLSPublicKeys
testObject_MLSKeysByPurpose1 =
  MLSKeysByPurpose
    { removal = testObject_MLSPublicKeys1
    }
