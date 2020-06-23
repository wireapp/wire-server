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

module API.RichInfo.Util where

import Bilge
import Brig.Types
import Data.ByteString.Conversion
import Data.Id
import Imports
import Util
import Wire.API.User.RichInfo

getRichInfo ::
  HasCallStack =>
  Brig ->
  -- | The user who is performing the query
  UserId ->
  -- | The users whose rich info is being queried
  UserId ->
  Http (Either Int RichInfoAssocList)
getRichInfo brig self uid = do
  r <-
    get
      ( brig
          . paths ["users", toByteString' uid, "rich-info"]
          . zUser self
      )
  if
      | statusCode r == 200 -> Right <$> responseJsonError r
      | statusCode r `elem` [403, 404] -> pure . Left . statusCode $ r
      | otherwise ->
        error $
          "expected status code 200, 403, or 404, got: " <> show (statusCode r)

-- | This contacts an internal end-point.  Note the asymmetry between this and the external
-- GET end-point in the body: here we need to wrap the 'RichInfo' in a 'RichInfoUpdate'.
putRichInfo ::
  HasCallStack =>
  Brig ->
  -- | The user whose rich info is being updated
  UserId ->
  RichInfoAssocList ->
  Http ResponseLBS
putRichInfo brig uid rinfo = do
  put
    ( brig
        . paths ["i", "users", toByteString' uid, "rich-info"]
        . json (RichInfoUpdate rinfo)
    )
