{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Collectd.Collectd
    ( Identifier (..)
    , ValueList  (..)
    , Option     (..)
    , serialise
    , putVal
    , putErr
    ) where

import Data.List (intersperse)
import Data.Monoid
import Data.Scientific
import Data.Text (Text)
import Data.Text.Lazy.Builder.Scientific
import Data.Time.Clock.POSIX
import System.IO

import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy             as L
import qualified Data.Text.Lazy.Builder     as L
import qualified Data.Text.Lazy.Builder.Int as L

-- host "/" plugin ["-" plugin instance] "/" type ["-" type instance]
data Identifier = Identifier
    { host       :: !Text
    , plugin     :: !Text
    , pluginInst :: Maybe Text
    , typ        :: !Text
    , typInst    :: Maybe Text
    }

data ValueList = ValueList
    { time   :: POSIXTime
    , values :: [Scientific]
    }

data Option = Interval Int

serialise :: Identifier -> [Option] -> ValueList -> L.Builder
serialise i o v = mconcat . intersperse " " $
    (serialiseIdent i : serialiseOpts o) ++ [serialiseVals v]

serialiseIdent :: Identifier -> L.Builder
serialiseIdent i = L.fromText (host i)
    <> "/"
    <> L.fromText (plugin i)
    <> maybe "" (("-" <>) . L.fromText) (pluginInst i)
    <> "/"
    <> L.fromText (typ i)
    <> maybe "" (("-" <>) . L.fromText) (typInst i)

serialiseOpts :: [Option] -> [L.Builder]
serialiseOpts = map fromOption
  where
    fromOption (Interval i) = "interval=" <> L.decimal i

serialiseVals :: ValueList -> L.Builder
serialiseVals v =
    let t = round (time v) :: Int in
    mconcat . intersperse ":" $ L.decimal t : map fromScientific (values v)
  where
    fromScientific d
        | e d < 0   = scientificBuilder d
        | otherwise = L.decimal (coefficient d * 10 ^ e d)

    e = base10Exponent

putVal :: Identifier -> [Option] -> ValueList -> IO ()
putVal i o =
    T.putStrLn . L.toStrict . L.toLazyText . ("PUTVAL " <>) . serialise i o

putErr :: Text -> IO ()
putErr s = T.hPutStrLn stderr $ "ERROR: " <> s
