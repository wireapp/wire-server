-- | Tinylog convenience things.
module System.Logger.Extended
    ( mkLogger
    , mkLogger'
    ) where

import Imports

import qualified System.Logger as Log

mkLogger :: Log.Level -> Bool -> IO Log.Logger
mkLogger lvl netstr = Log.new'
    . Log.setOutput Log.StdOut
    . Log.setFormat Nothing
    $ Log.simpleSettings (Just lvl) (Just netstr)

-- | Work where there are no options; Use Log.new which reads in LOG_* env variables.
--
-- TODO: DEPRECATED!  Use 'mkLogger' instead and get all settings from config files, not from
-- environment!
mkLogger' :: IO Log.Logger
mkLogger' = Log.new
    . Log.setOutput Log.StdOut
    . Log.setFormat Nothing
    $ Log.defSettings
