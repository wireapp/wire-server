{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Brig.Queue.Types
    ( Queue (..)
    ) where

import Imports
import Data.Aeson

-- | A remote queue that you can publish to and listen from.
data Queue = StompQueue Text | SqsQueue Text
    deriving (Eq, Show)

instance FromJSON Queue where
    parseJSON = withObject "Queue" $ \o ->
        o .: "queueType" >>= \case
            "stomp" -> StompQueue <$> o .: "queueName"
            "sqs"   -> SqsQueue <$> o .: "queueName"
            other   -> fail ("unknown 'queueType': " <> other)
