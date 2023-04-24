module Data.MessageQueue
  ( MessageQueueSettings (..)
  ) where

import Prelude (Show, String)
import Data.Text (Text)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

-- | Options for connecting to the message queue system
data MessageQueueSettings = MessageQueueSettings
  { mqHost  :: String
  , mqVHost :: Text
  , mqUser  :: Text
  , mqPass  :: Text
  , mqQueue :: Text
  } deriving (Show, Generic)
instance FromJSON MessageQueueSettings