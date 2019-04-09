{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
-- | Tinylog convenience things.
module System.Logger.Extended
    ( module Log
    , mkLogger
    , mkLogger'
    , LoggerT(..)
    , runWithLogger
    ) where

import Imports
import Control.Monad.Catch
import Database.CQL.IO

import System.Logger as Log
import qualified System.Logger.Class as LC

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

-- | It's a bit odd that we mention 'MonadClient' from the cql-io package here, but it's the
-- easiest way to get things done.  Alternatively, we could introduce 'LoggerT' in the gundeck
-- integration tests, which is the only place in the world where it is currently used, but we
-- may need it elsewhere in the future and here it's easier to find.
newtype LoggerT m a = LoggerT {runLoggerT :: ReaderT Log.Logger m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadClient
        )

instance (MonadIO m) => LC.MonadLogger (LoggerT m) where
    log :: LC.Level -> (LC.Msg -> LC.Msg) -> LoggerT m ()
    log l m = LoggerT $ do
        logger <- ask
        Log.log logger l m

runWithLogger :: Log.Logger -> LoggerT m a -> m a
runWithLogger logger = flip runReaderT logger . runLoggerT
