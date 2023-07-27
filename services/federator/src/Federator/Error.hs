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

module Federator.Error
  ( AsWai (..),
    errorResponse,
  )
where

import Data.Aeson qualified as A
import Imports
import Network.HTTP.Types.Header
import Network.Wai qualified as Wai
import Network.Wai.Utilities.Error qualified as Wai

class AsWai e where
  toWai :: e -> Wai.Error
  waiErrorDescription :: e -> Text

errorResponse :: [Header] -> Wai.Error -> Wai.Response
errorResponse hdrs e = Wai.responseLBS (Wai.code e) hdrs (A.encode e)
