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

module Main (main) where

import Brig.Index.Eval
import Brig.Index.Options
import Imports
import Options.Applicative
import System.Exit
import qualified System.Logger.Class as Log

main :: IO ()
main = do
  cmd <- execParser (info (helper <*> commandParser) desc)
  lgr <- initLogger
  runCommand lgr cmd
  -- TODO: dump metrics in a suitable format (NOT json)
  exitSuccess
  where
    desc =
      header "brig-index"
        <> progDesc "Brig Search Index Utilities"
        <> fullDesc
    initLogger =
      Log.new -- TODO: use mkLogger'?
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings
