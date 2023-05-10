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
  { host :: String,
    vHost :: Text,
    user :: Text,
    pass :: Text,
    queue :: Text
  }
  deriving (Show, Generic)

instance FromJSON MessageQueueSettings
