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

module CargoHold.Util where

import CargoHold.AWS
import CargoHold.App
import qualified CargoHold.CloudFront as CloudFront
import qualified CargoHold.S3 as S3
import Control.Lens
import Data.ByteString.Conversion
import Imports
import URI.ByteString hiding (urlEncode)

genSignedURL :: (ToByteString p) => p -> Handler URI
genSignedURL path = do
  uri <-
    view (aws . cloudFront) >>= \case
      Nothing -> S3.signedURL path
      Just cf -> CloudFront.signedURL cf path
  return $! uri
