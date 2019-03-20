{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Tinylog convenience things.
module System.Logger.Extended
    ( mkLogger
    , mkLogger'
    , LoggerT(..)
    ) where

import Imports

import qualified System.Logger as Log
import qualified System.Logger.Class as LC
import Database.CQL.IO
import Control.Monad.Catch

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

newtype LoggerT m a = LoggerT {runLoggerT :: ReaderT Log.Logger m a}
    deriving newtype
        (Functor
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
