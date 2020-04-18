-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module CargoHold.API.Legacy
  ( download,
    downloadOtr,
  )
where

import CargoHold.App
import qualified CargoHold.S3 as S3
import CargoHold.Util
import Data.Id
import Imports
import URI.ByteString

download :: UserId -> ConvId -> AssetId -> Handler (Maybe URI)
download _ _ ast = S3.getMetadata ast >>= maybe notFound found
  where
    notFound = return Nothing
    found public =
      if not public
        then return Nothing
        else do
          url <- genSignedURL (S3.plainKey ast)
          return $! Just $! url

downloadOtr :: UserId -> ConvId -> AssetId -> Handler (Maybe URI)
downloadOtr _ cnv ast = S3.getOtrMetadata cnv ast >>= maybe notFound found
  where
    notFound = return Nothing
    found _ = do
      url <- genSignedURL (S3.otrKey cnv ast)
      return $! Just $! url
