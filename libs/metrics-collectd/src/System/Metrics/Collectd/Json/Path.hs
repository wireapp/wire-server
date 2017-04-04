{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module System.Metrics.Collectd.Json.Path
    ( Path
    , Segment  (..)
    , Modifier (..)
    , path
    , compile
    , render
    , toText
    ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.List (intersperse)
import Data.Text (Text, stripPrefix)

newtype Path = Path
    { path :: [Segment]
    } deriving (Eq, Show)

data Segment
    = Name !Text (Maybe Modifier)
    | Star (Maybe Modifier)
    deriving (Eq, Show)

data Modifier
    = Drop
    | Rename !Text
    deriving (Eq, Show)

compile :: Text -> Either String Path
compile = eitherResult . flip feed "" . parse parser

parser :: Parser Path
parser = Path <$> many1 (char '/' >> literal <|> segment)
  where
    segment = do
        s <- takeWhile1 (\ c -> c /= '/' && c /= '{')
        m <- optional modifier
        pure $ case s of
            "*" -> Star m
            _   -> Name s m

    literal = Name
        <$> (char '"' *> takeWhile1 (/= '"') <* char '"')
        <*> pure Nothing

    modifier = do
      m <- char '{' *> takeWhile1 (/= '}') <* char '}'
      case m of
          (stripPrefix "rename->" -> Just rep) -> pure (Rename rep)
          "drop" -> pure Drop
          x      -> fail $ "Unknown modifier: " ++ show x

render :: Path -> Text
render = mconcat . ("/":) . intersperse "/" . map toText . path

toText :: Segment -> Text
toText (Star _  ) = "*"
toText (Name t _) = t
