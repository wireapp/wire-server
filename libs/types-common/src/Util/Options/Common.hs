{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Util.Options.Common where

import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.Char (toLower, isUpper)
import Data.Text (Text)
import Options.Applicative
import System.Environment

import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T

toFieldName :: Options
toFieldName = defaultOptions{ fieldLabelModifier = lowerFirst . dropPrefix }
  where
    lowerFirst :: String -> String
    lowerFirst (x:xs) = toLower x : xs
    lowerFirst []     = ""

    dropPrefix :: String -> String
    dropPrefix = dropWhile (not . isUpper)

optOrEnv :: (a -> b) -> (Maybe a) -> (String -> b) -> String -> IO b
optOrEnv getter conf reader var = case conf of
    Nothing -> reader <$> getEnv var
    Just c  -> pure $ getter c

bytesOption :: Mod OptionFields String -> Parser ByteString
bytesOption = fmap C.pack . strOption

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap T.pack . strOption
