-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Small layer on top of @fast-logger@ which adds log-levels and
-- timestamp support and not much more.
module System.Logger
    ( -- * Settings
      Settings
    , defSettings
    , logLevel
    , setLogLevel
    , logLevelOf
    , setLogLevelOf
    , output
    , setOutput
    , format
    , setFormat
    , delimiter
    , readEnvironment
    , setDelimiter
    , setNetStrings
    , setReadEnvironment
    , setRendererNetstr
    , setRendererDefault
    , bufSize
    , setBufSize
    , name
    , setName
    , setRenderer
    , renderer

      -- * Type definitions
    , Logger
    , Level      (..)
    , Output     (..)
    , DateFormat (..)
    , Renderer
    , iso8601UTC

      -- * Core API
    , new
    , create
    , level
    , flush
    , close
    , clone
    , settings

      -- ** Logging
    , log
    , trace
    , debug
    , info
    , warn
    , err
    , fatal

    , module M
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.UnixTime
import System.Environment (lookupEnv)
import System.Logger.Message as M
import System.Logger.Settings
import Prelude hiding (log)

import qualified Data.Map.Strict       as Map
import qualified System.Log.FastLogger as FL

data Logger = Logger
    { logger    :: FL.LoggerSet
    , settings  :: Settings
    , getDate   :: IO (Msg -> Msg)
    }

-- | Create a new 'Logger' with the given 'Settings'.
-- Please note that the 'logLevel' can be dynamically adjusted by setting
-- the environment variable @LOG_LEVEL@ accordingly. Likewise the buffer
-- size can be dynamically set via @LOG_BUFFER@ and netstrings encoding
-- can be enabled with @LOG_NETSTR=True@.  **NOTE: If you do this any custom
-- renderers you may have passed with the settings will be overwritten!**
--
-- Since version 0.11 one can also use @LOG_LEVEL_MAP@ to specify log
-- levels per (named) logger. The syntax uses standard haskell syntax for
-- association lists of type @[(Text, Level)]@. For example:
--
-- If you want to ignore environment variables, call @setReadEnvironment False@ on the
-- 'Settings'.
--
-- @
-- $ LOG_LEVEL=Info LOG_LEVEL_MAP='[("foo", Warn), ("bar", Trace)]' cabal repl
-- > g1 <- new defSettings
-- > let g2 = clone (Just "foo") g1
-- > let g3 = clone (Just "bar") g1
-- > let g4 = clone (Just "xxx") g1
-- > logLevel (settings g1)
-- Info
-- > logLevel (settings g2)
-- Warn
-- > logLevel (settings g3)
-- Trace
-- > logLevel (settings g4)
-- Info
-- @
new :: MonadIO m => Settings -> m Logger
new s = liftIO $ do
    !n <- fmap (readNote "Invalid LOG_BUFFER") <$> maybeLookupEnv "LOG_BUFFER"
    !l <- fmap (readNote "Invalid LOG_LEVEL")  <$> maybeLookupEnv "LOG_LEVEL"
    !e <- fmap (readNote "Invalid LOG_NETSTR") <$> maybeLookupEnv "LOG_NETSTR"
    !m <- fromMaybe "[]" <$> maybeLookupEnv "LOG_LEVEL_MAP"
    let !k  = logLevelMap s `mergeWith` m
    let !s' = setLogLevel (fromMaybe (logLevel s) l)
            . maybe id (bool id setRendererNetstr) e
            . setLogLevelMap k
            $ s
    g <- fn (output s) (fromMaybe (bufSize s) n)
    Logger g s' <$> mkGetDate (format s)
  where
    maybeLookupEnv :: String -> IO (Maybe String)
    maybeLookupEnv key =
        if readEnvironment s
            then lookupEnv key
            else pure Nothing

    fn StdOut   = FL.newStdoutLoggerSet
    fn StdErr   = FL.newStderrLoggerSet
    fn (Path p) = flip FL.newFileLoggerSet p

    mkGetDate Nothing  = return (return id)
    mkGetDate (Just f) = return (msg . (display f) <$> getUnixTime)

    mergeWith m e = Map.fromList (readNote "Invalid LOG_LEVEL_MAP" e) `Map.union` m

-- | Invokes 'new' with default settings and the given output as log sink.
create :: MonadIO m => Output -> m Logger
create o = new $ setOutput o defSettings

readNote :: Read a => String -> String -> a
readNote m s = case reads s of
    [(a, "")] -> a
    _         -> error m

-- | Logs a message with the given level if greater or equal to the
-- logger's threshold.
log :: MonadIO m => Logger -> Level -> (Msg -> Msg) -> m ()
log g l m = unless (level g > l) $ putMsg g l m
{-# INLINE log #-}

-- | Abbreviation of 'log' using the corresponding log level.
trace, debug, info, warn, err, fatal :: MonadIO m => Logger -> (Msg -> Msg) -> m ()
trace g = log g Trace
debug g = log g Debug
info  g = log g Info
warn  g = log g Warn
err   g = log g Error
fatal g = log g Fatal
{-# INLINE trace #-}
{-# INLINE debug #-}
{-# INLINE info  #-}
{-# INLINE warn  #-}
{-# INLINE err   #-}
{-# INLINE fatal #-}

-- | Clone the given logger and optionally give it a name
-- (use @Nothing@ to clear).
--
-- If 'logLevelOf' returns a custom 'Level' for this name
-- then the cloned logger will use it for its log messages.
clone :: Maybe Text -> Logger -> Logger
clone Nothing  g = g { settings = setName Nothing (settings g) }
clone (Just n) g =
    let s = settings g
        l = fromMaybe (logLevel s) $ logLevelOf n s
    in g { settings = setName (Just n) . setLogLevel l $ s }

-- | Force buffered bytes to output sink.
flush :: MonadIO m => Logger -> m ()
flush = liftIO . FL.flushLogStr . logger

-- | Closes the logger.
close :: MonadIO m => Logger -> m ()
close g = liftIO $ FL.rmLoggerSet (logger g)

-- | Inspect this logger's threshold.
level :: Logger -> Level
level = logLevel . settings
{-# INLINE level #-}

putMsg :: MonadIO m => Logger -> Level -> (Msg -> Msg) -> m ()
putMsg g l f = liftIO $ do
    d <- getDate g
    let r = renderer  $ settings g
    let x = delimiter $ settings g
    let s = nameMsg   $ settings g
    let df = fromMaybe iso8601UTC . format $ settings g
    let ll = logLevel $ settings g
    let m = render (r x df ll) (d . lmsg l . s . f)
    FL.pushLogStr (logger g) (FL.toLogStr m)

lmsg :: Level -> (Msg -> Msg)
lmsg Trace = msg (val "T")
lmsg Debug = msg (val "D")
lmsg Info  = msg (val "I")
lmsg Warn  = msg (val "W")
lmsg Error = msg (val "E")
lmsg Fatal = msg (val "F")
{-# INLINE lmsg #-}
