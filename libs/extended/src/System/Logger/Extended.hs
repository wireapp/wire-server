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

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.Builder as B
import qualified System.Logger.Class as LC

mkLogger :: Log.Level -> Bool -> IO Log.Logger
mkLogger lvl netstr = Log.new
    . Log.setReadEnvironment False
    . Log.setOutput Log.StdOut
    . Log.setFormat Nothing
    $ simpleSettings lvl netstr

-- | Variant of Log.defSettings:
--
--   * change log level according to first arg (iff isJust).
--
--   * pick renderNetstr or renderDefault according to 2nd arg (iff isJust).
--
--   * use 'canonicalizeWhitespace'.
--
simpleSettings :: Log.Level -> Bool -> Log.Settings
simpleSettings lvl netstr
  = Log.setLogLevel lvl
  . Log.setRenderer (canonicalizeWhitespace rndr)
  $ Log.defSettings
  where
    rndr = case netstr of
      True  -> \_ _ _ -> Log.renderNetstr
      False -> \s _ _ -> Log.renderDefault s

-- | Replace all whitespace characters in the output of a renderer by @' '@.
-- Log output must be ASCII encoding.
--
-- (Many logging processors handle newlines poorly.  Instead of hunting down all
-- places and situations in your code and your dependencies that inject newlines
-- into your log messages, you can choose to call 'canonicalizeWhitespace' on
-- your renderer.)
canonicalizeWhitespace :: Log.Renderer -> Log.Renderer
canonicalizeWhitespace rndrRaw delim df lvl
  = B.lazyByteString . nl2sp . B.toLazyByteString . rndrRaw delim df lvl
  where
    nl2sp :: L.ByteString -> L.ByteString
    nl2sp = L.concatMap $
      \c -> if isSpace c
            then " "
            else L.singleton c

-- | Like 'mkLogger', but uses Log.new which reads in LOG_* env variables.
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
