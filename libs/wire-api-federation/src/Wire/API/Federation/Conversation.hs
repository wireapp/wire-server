module Wire.API.Federation.Conversation where

import Data.Id (ConvId)
import Data.Qualified (Qualified)
import Servant.API ((:>), Capture, JSON, Post)
import Servant.API.Generic ((:-))
import Wire.API.Federation.Types.Event (Event, MemberJoin)

data Api routes
  = Api
      { joinConversationById ::
          routes
            :- "conversation"
            :> Capture "cnv" (Qualified ConvId)
            :> "join"
            :> Post '[JSON] (Event MemberJoin)
      }
