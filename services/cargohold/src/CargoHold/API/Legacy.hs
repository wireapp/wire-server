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

-- Legacy:
module CargoHold.API.Legacy
  ( download,
    downloadOtr,
  )
where

import CargoHold.App
import qualified CargoHold.S3 as S3
import CargoHold.Types.V3 (Principal (UserPrincipal))
import CargoHold.Util (genSignedURL)
import Data.Id
import Data.Qualified (Local, QualifiedWithTag (tUntagged))
import Imports
import URI.ByteString

download :: Local UserId -> ConvId -> AssetId -> Handler (Maybe URI)
download luid _cnv ast = S3.getMetadata ast >>= maybe notFound found
  where
    notFound = pure Nothing
    found isPublic =
      if not isPublic
        then pure Nothing
        else do
          let principal = UserPrincipal <$> tUntagged luid
          url <- genSignedURL (Just principal) Nothing (S3.plainKey ast) Nothing
          pure (Just url)

downloadOtr :: Local UserId -> ConvId -> AssetId -> Handler (Maybe URI)
downloadOtr luid cnv ast = S3.getOtrMetadata cnv ast >>= maybe notFound found
  where
    notFound = pure Nothing
    found _ = do
      let principal = UserPrincipal <$> tUntagged luid
      url <- genSignedURL (Just principal) Nothing (S3.otrKey cnv ast) Nothing
      pure (Just url)
