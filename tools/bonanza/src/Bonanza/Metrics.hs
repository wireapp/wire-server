{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
