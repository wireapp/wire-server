module Wire.API.Federation where

import GHC.Generics (Generic)
import Servant.API.Generic ((:-), AsApi, ToServant)
import qualified Wire.API.Federation.Conversation as Conversation (Api)

data Api routes
  = Api
      { conversation :: routes :- ToServant Conversation.Api AsApi
      }
  deriving stock (Generic)
