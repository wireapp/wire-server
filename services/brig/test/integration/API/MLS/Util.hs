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

module API.MLS.Util where

import Bilge
import Bilge.Assert
import Data.Aeson (object, toJSON, (.=))
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import Data.Json.Util
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Text as T
import Imports
import System.Process
import Util
import Wire.API.MLS.Credential
import Wire.API.User.Client

data SetKey = SetKey | DontSetKey
  deriving (Eq)

uploadKeyPackages :: Brig -> FilePath -> SetKey -> Qualified UserId -> ClientId -> Int -> Http ()
uploadKeyPackages brig store sk u c n = do
  let cmd0 = ["crypto-cli", "--store", store, "--enc-key", "test"]
      clientId =
        show (qUnqualified u)
          <> ":"
          <> T.unpack (client c)
          <> "@"
          <> T.unpack (domainText (qDomain u))
  kps <-
    replicateM n . liftIO . spawn . shell . unwords $
      cmd0 <> ["key-package", clientId]
  when (sk == SetKey) $
    do
      pk <-
        liftIO . spawn . shell . unwords $
          cmd0 <> ["public-key", clientId]
      put
        ( brig
            . paths ["clients", toByteString' c]
            . zUser (qUnqualified u)
            . json defUpdateClient {updateClientMLSPublicKeys = Map.fromList [(Ed25519, pk)]}
        )
      !!! const 200 === statusCode
  let upload = object ["key_packages" .= toJSON (map Base64ByteString kps)]
  post
    ( brig
        . paths ["mls", "key-packages", "self", toByteString' c]
        . zUser (qUnqualified u)
        . json upload
    )
    !!! const (case sk of SetKey -> 201; DontSetKey -> 400) === statusCode
