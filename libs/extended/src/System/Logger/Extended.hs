{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Tinylog convenience things.
module System.Logger.Extended
    ( module Log
    , LogFormat(..)
    , mkLogger
    , mkLogger'
    , LoggerT(..)
    , runWithLogger
    , netStringsToLogFormat
    ) where

import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Encoding (pair, list, text)
import Database.CQL.IO
import GHC.Generics
import Imports
import System.Logger as Log
import Data.String.Conversions (cs)
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified System.Logger.Class as LC


deriving instance Generic LC.Level
instance FromJSON LC.Level
instance ToJSON LC.Level

-- | The log formats supported
data LogFormat = JSON | Plain | Netstring 
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | We use this as an intermediate structure to ease the implementation of the
-- ToJSON instance but we could just inline everything. I think this has
-- negligible impact and makes the code a bit more readable. Let me know
data Element' = Element' Series [Builder]

elementToEncoding :: Element' -> Encoding
elementToEncoding (Element' fields msgs) = pairs $ fields <> msgsToSeries msgs
    where
      msgsToSeries :: [Builder] -> Series
      msgsToSeries  = pair "msgs" . list (text . cs . eval)

collect :: [Element] -> Element'
collect = foldr go (Element' mempty [])
  where
    go :: Element -> Element' -> Element'
    go (Bytes b) (Element' f m) =
      Element' f (b : m)
    go (Field k v) (Element' f m) = 
      Element' (f <> pair (cs . eval $ k) (text . cs . eval $ v)) m

jsonRenderer :: Renderer
jsonRenderer _sep _dateFormat _logLevel = fromEncoding  . elementToEncoding . collect

-- | Here for backwards-compatibility reasons
netStringsToLogFormat :: Bool -> LogFormat
netStringsToLogFormat True = Netstring
netStringsToLogFormat False = Plain

-- | Creates a logger given a log format Also takes an useNetstrings argument
-- which is there because we cannot immediatelly deprecate the old interface.
-- Old configs only provide the useNetstrings argument and not the logFormat
-- argument, and in that case implement the old behaviour of either enabling
-- plain text logging or netstring logging.  If both arguments are set,
-- logFormat takes presedence over useNetstrings
--
-- FUTUREWORK: Once we get rid of the useNetstrings in our config files, we can
-- remove this function and rename 'mkLoggerNew' to 'mkLogger'
mkLogger :: Log.Level -> Maybe (Last Bool) -> Maybe (Last LogFormat) -> IO  Log.Logger
mkLogger lvl useNetstrings logFormat = do
  mkLoggerNew lvl $
    case (fmap netStringsToLogFormat <$> useNetstrings) <> logFormat of
      Just x -> getLast x
      Nothing -> Plain
   
-- | Version of mkLogger that doesn't support the deprecated useNetstrings option
mkLoggerNew :: Log.Level -> LogFormat -> IO Log.Logger
mkLoggerNew lvl logFormat = Log.new
    . Log.setReadEnvironment False
    . Log.setOutput Log.StdOut
    . Log.setFormat Nothing
    $ simpleSettings lvl logFormat

-- | Variant of Log.defSettings:
--
--   * change log level according to first arg (iff isJust).
--
--   * pick renderNetstr or renderDefault according to 2nd arg (iff isJust).
--
--   * use 'canonicalizeWhitespace'.
--
simpleSettings :: Log.Level -> LogFormat -> Log.Settings
simpleSettings lvl logFormat
  = Log.setLogLevel lvl
  . Log.setRenderer (canonicalizeWhitespace rndr)
  $ Log.defSettings
  where
    rndr = case logFormat of
      Netstring  -> \_separator _dateFormat _level -> Log.renderNetstr
      Plain -> \ separator _dateFormat _level -> Log.renderDefault separator
      JSON -> jsonRenderer

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
