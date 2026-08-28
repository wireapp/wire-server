-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Network.Wai.Utilities.Exception where

import Control.Exception
import Imports

-- | `displayException` with empty `ExceptionContext`
--
-- Starting with GHC 9.10, exceptions carry a context that contains backtraces.
-- Displaying these  is not always desired; e.g. for HTTP response bodies.
displayExceptionNoBacktrace :: (Exception e) => e -> String
displayExceptionNoBacktrace = trim . displayException . toException
  where
    trim = (dropWhileEnd isSpace) . (dropWhile isSpace)
