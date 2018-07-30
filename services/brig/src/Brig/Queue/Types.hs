{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Brig.Queue.Types
    ( Queue (..)
    ) where

import Data.Aeson
import Data.Monoid
import Data.Text (Text)

-- | A remote queue that you can publish to and listen from.
data Queue = StompQueue Text | SqsQueue Text
    deriving (Eq, Show)

instance FromJSON Queue where
    parseJSON = withObject "Queue" $ \o ->
        o .: "queueType" >>= \case
            "stomp" -> StompQueue <$> parseJSON (Object o)
            "sqs"   -> SqsQueue <$> parseJSON (Object o)
            other   -> fail ("unknown 'queueType': " <> other)
