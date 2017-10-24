{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Util.Options.Common where

import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Text (Text)
import Options.Applicative
import System.Environment

import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T

toFieldName :: Int -> Options
toFieldName n = defaultOptions{ fieldLabelModifier = fn . drop n }
  where
    fn :: String -> String
    fn (x:xs) = toLower x : xs
    fn []     = ""

optOrEnv :: (a -> b) -> (Maybe a) -> (String -> b) -> String -> IO b
optOrEnv getter conf reader var = case conf of
    Nothing -> reader <$> getEnv var
    Just c  -> pure $ getter c

bytesOption :: Mod OptionFields String -> Parser ByteString
bytesOption = fmap C.pack . strOption

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap T.pack . strOption
