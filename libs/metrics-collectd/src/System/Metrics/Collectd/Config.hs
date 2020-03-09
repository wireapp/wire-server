{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module System.Metrics.Collectd.Config
    ( Config  (..)
    , Section (..)
    , Type
    , parse
    , readConfig
    , typename
    ) where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Ini
import Data.Text (Text)
import System.Metrics.Collectd.Json.Path

import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

newtype Type = Type Text deriving (Eq, Show, Monoid)

data Config = Config
    { interval :: !Int       -- milliseconds
    , hostname :: !Text
    , sections :: [Section]
    } deriving (Eq, Show)

data Section = Section
    { name  :: !Text
    , url   :: !Text
    , paths :: [(Path, Type)]
    } deriving (Eq, Show)

parse :: Int -> Text -> Text -> Either String Config
parse x h t = do
    i <- Map.map (map2 T.strip T.strip) . map2 T.strip id . unIni <$> parseIni t
    Config x h <$> mapM toSection (Map.toList i)
  where
    map2 :: (Eq c, Hashable c) => (a -> c) -> (b -> d) -> HashMap a b -> HashMap c d
    map2 kf vf = Map.foldlWithKey' (\m k v -> Map.insert (kf k) (vf v) m) Map.empty

    toSection :: (Text, HashMap Text Text) -> Either String Section
    toSection (k, m) = do
        u <- maybe (Left "missing key 'url'") Right (Map.lookup "url" m)
        p <- mapM toPaths . filter (not . reserved . fst) $ Map.toList m
        return (Section k u p)

    toPaths :: (Text, Text) -> Either String (Path, Type)
    toPaths (k, v) = (, Type v) <$> compile k

    reserved = (== "url")

readConfig :: Int -> Text -> FilePath -> IO (Either String Config)
readConfig i h = fmap (parse i h) . T.readFile

typename :: Type -> Text
typename (Type t) = t
