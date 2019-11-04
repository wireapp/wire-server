module Brig.InternalEvent.Process
    ( onEvent
    ) where

import Imports
import Brig.App
import Brig.InternalEvent.Types
import Control.Monad.Catch
import Data.ByteString.Conversion
import System.Logger.Class (field, msg, (~~), val)
import UnliftIO (timeout)

import qualified Brig.API.User          as API
import qualified Brig.Provider.API      as API
import qualified System.Logger.Class    as Log

-- | Handle an internal event.
--
-- Has a one-minute timeout that should be enough for anything that it does.
onEvent :: InternalNotification -> AppIO ()
onEvent n = handleTimeout $ case n of
    DeleteUser uid -> do
        Log.info $ msg (val "Processing user delete event")
                ~~ field "user" (toByteString uid)
        API.lookupAccount uid >>= mapM_ API.deleteAccount
        -- As user deletions are expensive resource-wise in the context of
        -- bulk user deletions (e.g. during team deletions),
        -- wait 50ms before processing the next event
        liftIO $ threadDelay 50000
    DeleteService pid sid -> do
        Log.info $ msg (val "Processing service delete event")
                ~~ field "provider" (toByteString pid)
                ~~ field "service" (toByteString sid)
        API.finishDeleteService pid sid
  where
    handleTimeout act = timeout 60000000 act >>= \case
        Just x  -> pure x
        Nothing -> throwM (InternalEventTimeout n)

data InternalEventException
    -- | 'onEvent' has timed out
    = InternalEventTimeout InternalNotification
  deriving Show

instance Exception InternalEventException
