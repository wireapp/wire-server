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
      InternalCloseConversationsFrom tid uid -> GalleyAPIAccess.closeConversationsFrom tid uid
