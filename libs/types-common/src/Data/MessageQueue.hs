module Data.MessageQueue
  ( MessageQueueSettings (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude (Show, String)

-- | Options for connecting to the message queue system
data MessageQueueSettings = MessageQueueSettings
  { mqHost :: String,
    mqVHost :: Text,
    mqUser :: Text,
    mqPass :: Text,
    mqQueue :: Text
  }
  deriving (Show, Generic)

instance FromJSON MessageQueueSettings
