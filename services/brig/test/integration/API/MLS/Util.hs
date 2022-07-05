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
import System.FilePath
import System.Process
import Util
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.User.Client

data SetKey = SetKey | DontSetKey
  deriving (Eq)

uploadKeyPackages ::
  HasCallStack =>
  Brig ->
  FilePath ->
  SetKey ->
  Qualified UserId ->
  ClientId ->
  Int ->
  Http ()
uploadKeyPackages brig tmp sk u c n = do
  let cmd0 = ["mls-test-cli", "--store", tmp </> (clientId <> ".db")]
      clientId =
        show (qUnqualified u)
          <> ":"
          <> T.unpack (client c)
          <> "@"
          <> T.unpack (domainText (qDomain u))
  void . liftIO . flip spawn Nothing . shell . unwords $
    cmd0 <> ["init", clientId]
  kps <-
    replicateM n . liftIO . flip spawn Nothing . shell . unwords $
      cmd0 <> ["key-package", "create"]
  when (sk == SetKey) $
    do
      pk <-
        liftIO . flip spawn Nothing . shell . unwords $
          cmd0 <> ["public-key"]
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

getKeyPackageCount :: HasCallStack => Brig -> Qualified UserId -> ClientId -> Http KeyPackageCount
getKeyPackageCount brig u c =
  responseJsonError
    =<< get
      ( brig . paths ["mls", "key-packages", "self", toByteString' c, "count"]
          . zUser (qUnqualified u)
      )
    <!! const 200 === statusCode
