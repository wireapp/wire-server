module Wire.ConversationSubsystem.Interpreter where

import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Conversation as API
import Wire.ConversationSubsystem as Sub (ConversationSubsystem (..))

data ConversationSubsystemConfig = ConversationSubsystemConfig
  { maxConvSize :: Word16
  }

data ConversationSubsystemError = 

interpretConversationSubsystem :: InterpreterFor ConversationSubsystem r
interpretConversationSubsystem = interpret $ \case
  Sub.CreateGroupConversation lusr conn newConv -> createGroupConvImpl lusr conn newConv
  GetConversation {} -> _

createGroupConvImpl :: Local UserId -> Maybe ConnId -> NewConv -> Sem r CreateGroupConversation
createGroupConvImpl = _
