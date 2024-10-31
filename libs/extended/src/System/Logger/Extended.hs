{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

-- | Tinylog convenience things.
module System.Logger.Extended
  ( module Log,
    LogFormat (..),
    mkLogger,
    mkLogger',
    LoggerT (..),
    runWithLogger,
    netStringsToLogFormat,
    structuredJSONRenderer,
  )
where

import Cassandra (MonadClient)
import Control.Monad.Catch
import Data.Aeson as Aeson
import Data.Aeson.Encoding (list, pair, text)
import Data.Aeson.Key qualified as Key
import Data.ByteString (toStrict)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Map.Lazy qualified as Map
import Data.Schema qualified as S
import Data.Text.Encoding
import Data.Text.Encoding.Error
import GHC.Generics
import Imports
import System.Logger as Log
import System.Logger.Class qualified as LC

deriving instance Generic LC.Level

deriving via (S.Schema LC.Level) instance Aeson.FromJSON LC.Level

deriving via (S.Schema LC.Level) instance Aeson.ToJSON LC.Level

instance S.ToSchema LC.Level where
  schema =
    S.enum @Text "Level" $
      mconcat
        [ S.element "Trace" Trace,
          S.element "Debug" Debug,
          S.element "Info" Info,
          S.element "Warn" Warn,
          S.element "Error" LC.Error,
          S.element "Fatal" Fatal
        ]

-- | The log formats supported
data LogFormat = JSON | Plain | Netstring | StructuredJSON
  deriving stock (Eq, Show, Bounded, Enum, Generic)
  deriving (ToJSON, FromJSON) via (S.Schema LogFormat)

instance S.ToSchema LogFormat where
  schema =
    S.enum @Text "LogFormat" $
      mconcat
        [ S.element "JSON" JSON,
          S.element "Plain" Plain,
          S.element "Netstring" Netstring,
          S.element "StructuredJSON" StructuredJSON
        ]

-- | We use this as an intermediate structure to ease the implementation of the
-- ToJSON instance but we could just inline everything. I think this has
-- negligible impact and makes the code a bit more readable. Let me know
data Element' = Element' Series [Builder]

elementToEncoding :: Element' -> Encoding
elementToEncoding (Element' fields msgs) = pairs $ fields <> msgsToSeries msgs
  where
    msgsToSeries :: [Builder] -> Series
    msgsToSeries =
      pair "msgs"
        . list
          ( text
              . decodeUtf8With lenientDecode
              . toStrict
              . eval
          )

collect :: [Element] -> Element'
collect = foldr go (Element' mempty [])
  where
    go :: Element -> Element' -> Element'
    go (Bytes b) (Element' f m) =
      Element' f (b : m)
    go (Field k v) (Element' f m) =
      Element'
        ( f
            <> pair
              (Key.fromText . dec . toStrict . eval $ k)
              (text . dec . toStrict . eval $ v)
        )
        m
    dec = decodeUtf8With lenientDecode

jsonRenderer :: Renderer
jsonRenderer _sep _dateFormat _logLevel = fromEncoding . elementToEncoding . collect

data StructuredJSONOutput = StructuredJSONOutput {lvl :: Maybe Level, msgs :: [Text], fields :: Map Key [Text]}

-- | Displays all the 'Bytes' segments in a list under key @msgs@ and 'Field'
-- segments as key-value pair in a JSON
--
-- >>> logElems = [Bytes "W", Bytes "The message", Field "field1" "val1", Field "field2" "val2", Field "field1" "val1.1"]
-- >>> B.toLazyByteString $ structuredJSONRenderer "," iso8601UTC Info logElems
-- "{\"msgs\":[\"The message\"],\"field1\":[\"val1\",\"val1.1\"],\"field2\":\"val2\",\"level\":\"Warn\"}"
structuredJSONRenderer :: Renderer
structuredJSONRenderer _sep _dateFmt _lvlThreshold logElems =
  let structuredJSON = toStructuredJSONOutput logElems
   in fromEncoding . toEncoding $
        object
          ( [ "level" Aeson..= lvl structuredJSON,
              "msgs" Aeson..= msgs structuredJSON
            ]
              <> Map.foldMapWithKey (\k v -> [k Aeson..= renderTextList v]) (fields structuredJSON)
          )
  where
    -- Renders List of Text as a String, if it only contains one element. This
    -- should be most (if not all) of the cases
    renderTextList :: [Text] -> Value
    renderTextList [t] = String t
    renderTextList xs = toJSON xs

    builderToText :: Builder -> Text
    builderToText = decodeUtf8With lenientDecode . toStrict . eval

    -- We need to do this to work around https://gitlab.com/twittner/tinylog/-/issues/5
    parseLevel :: Text -> Maybe Level
    parseLevel = \case
      "T" -> Just Trace
      "D" -> Just Debug
      "I" -> Just Info
      "W" -> Just Warn
      "E" -> Just Log.Error
      "F" -> Just Fatal
      _ -> Nothing

    toStructuredJSONOutput :: [Element] -> StructuredJSONOutput
    toStructuredJSONOutput =
      foldr
        ( \e o -> case e of
            Bytes b ->
              let buildMsg = builderToText b
               in case parseLevel buildMsg of
                    Nothing -> o {msgs = builderToText b : msgs o}
                    Just lvl -> o {lvl = Just lvl}
            Field k v -> o {fields = Map.insertWith (<>) (Key.fromText $ builderToText k) ([builderToText v]) (fields o)}
        )
        (StructuredJSONOutput Nothing [] mempty)

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
mkLogger :: Log.Level -> Maybe (Last Bool) -> Maybe (Last LogFormat) -> IO Log.Logger
mkLogger lvl useNetstrings logFormat = do
  mkLoggerNew lvl $
    maybe
      Plain
      getLast
      ((fmap netStringsToLogFormat <$> useNetstrings) <> logFormat)

-- | Version of mkLogger that doesn't support the deprecated useNetstrings option
mkLoggerNew :: Log.Level -> LogFormat -> IO Log.Logger
mkLoggerNew lvl logFormat =
  Log.new
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
simpleSettings :: Log.Level -> LogFormat -> Log.Settings
simpleSettings lvl logFormat =
  Log.setLogLevel lvl
    . Log.setRenderer (canonicalizeWhitespace rndr)
    $ Log.defSettings
  where
    rndr = case logFormat of
      Netstring -> \_separator _dateFormat _level -> Log.renderNetstr
      Plain -> \separator _dateFormat _level -> Log.renderDefault separator
      JSON -> jsonRenderer
      StructuredJSON -> structuredJSONRenderer

-- | Replace all whitespace characters in the output of a renderer by @' '@.
-- Log output must be ASCII encoding.
--
-- (Many logging processors handle newlines poorly.  Instead of hunting down all
-- places and situations in your code and your dependencies that inject newlines
-- into your log messages, you can choose to call 'canonicalizeWhitespace' on
-- your renderer.)
canonicalizeWhitespace :: Log.Renderer -> Log.Renderer
canonicalizeWhitespace rndrRaw delim df lvl =
  B.lazyByteString . nl2sp . B.toLazyByteString . rndrRaw delim df lvl
  where
    nl2sp :: L.ByteString -> L.ByteString
    nl2sp = L.concatMap $
      \c ->
        if isSpace c
          then " "
          else L.singleton c

-- | Like 'mkLogger', but uses Log.new which reads in LOG_* env variables.
--
-- TODO: DEPRECATED!  Use 'mkLogger' instead and get all settings from config files, not from
-- environment!
mkLogger' :: IO Log.Logger
mkLogger' =
  Log.new
    . Log.setOutput Log.StdOut
    . Log.setFormat Nothing
    $ Log.defSettings

-- | It's a bit odd that we mention 'MonadClient' from the cql-io package here, but it's the
-- easiest way to get things done.  Alternatively, we could introduce 'LoggerT' in the gundeck
-- integration tests, which is the only place in the world where it is currently used, but we
-- may need it elsewhere in the future and here it's easier to find.
newtype LoggerT m a = LoggerT {runLoggerT :: ReaderT Log.Logger m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadClient
    )

instance (MonadIO m) => LC.MonadLogger (LoggerT m) where
  log :: LC.Level -> (LC.Msg -> LC.Msg) -> LoggerT m ()
  log l m = LoggerT $ do
    logger <- ask
    Log.log logger l m

runWithLogger :: Log.Logger -> LoggerT m a -> m a
runWithLogger logger = flip runReaderT logger . runLoggerT
