module Wire.API.Federation where

import Servant.API.Generic ((:-))
import qualified Wire.API.Federation.Conversation as Conversation (Api)

data Api routes
  = Api
      { conversation :: routes :- Conversation.Api routes
      }
