{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

module Bonanza.Metrics
  ( Stats (..),
    dumpStderr,
    formatStderr,
  )
where

-- FUTUREWORK: In case bonanza is to be used in the future (big 'if'!)
-- and there is a wish to have metrics in bonanza (another 'if'),
-- then you may wish to implement prometheus-style metrics in this module.
-- This library used to support collectd metrics,
-- which were removed as part of https://github.com/wireapp/wire-server/pull/940

import Data.Time
import Imports
import System.IO

data Stats
  = Stats
      { sBytesIn :: !Int64,
        sBytesOut :: !Int64,
        sCPUTime :: !DiffTime,
        sWallTime :: !NominalDiffTime,
        sEventsIn :: !Int64
      }
  deriving (Eq, Show)

dumpStderr :: Stats -> IO ()
dumpStderr = hPutStrLn stderr . ('\n' :) . formatStderr

formatStderr :: Stats -> String
formatStderr Stats {..} =
  unlines . map (intercalate "\t") $
    [ ["Events parsed:", show sEventsIn],
      ["Bytes read:", show sBytesIn],
      ["Bytes written:", show sBytesOut],
      ["CPU time:", show sCPUTime],
      ["Wall time:", show sWallTime]
    ]
