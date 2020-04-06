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

module Federator.Util
  ( wireJsonOptions,
  )
where

import Data.Aeson as Aeson
import Imports

dropPrefix :: String -> String -> Maybe String
dropPrefix pfx str =
  if length pfx > length str
    then Nothing
    else case splitAt (length pfx) str of
      (pfx', sfx) ->
        if pfx' /= pfx
          then Nothing
          else Just sfx

-- | This is a partial function; totality of all calls must be verified by roundtrip tests on
-- the aeson instances involved.
wireJsonOptions :: String -> Options
wireJsonOptions pfx = defaultOptions {fieldLabelModifier = fromJust . dropPrefix pfx . fmap toLower}
