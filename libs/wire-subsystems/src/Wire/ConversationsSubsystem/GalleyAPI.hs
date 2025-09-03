module Wire.ConversationsSubsystem.GalleyAPI
  ( interpretConversationsSubsystemToGalleyAPI,
  )
where

import Imports
import Polysemy
import Wire.ConversationsSubsystem
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess

interpretConversationsSubsystemToGalleyAPI :: (Member GalleyAPIAccess r) => InterpreterFor ConversationsSubsystem r
interpretConversationsSubsystemToGalleyAPI =
  interpret $
    \case
      InternalLeaveConversationsFrom tid uid -> GalleyAPIAccess.leaveConversationsFrom tid uid
