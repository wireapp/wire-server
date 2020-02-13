module Brig.InternalEvent.Process
  ( onEvent,
  )
where

import qualified Brig.API.User as API
import Brig.App
import Brig.InternalEvent.Types
import Brig.Options (defDeleteThrottleMillis, setDeleteThrottleMillis)
import qualified Brig.Provider.API as API
import Control.Lens (view)
import Control.Monad.Catch
import Data.ByteString.Conversion
import Imports
import System.Logger.Class (field, msg, val, (~~))
import qualified System.Logger.Class as Log
import UnliftIO (timeout)

-- | Handle an internal event.
--
-- Has a one-minute timeout that should be enough for anything that it does.
onEvent :: InternalNotification -> AppIO ()
onEvent n = handleTimeout $ case n of
  DeleteUser uid -> do
    Log.info $
      msg (val "Processing user delete event")
        ~~ field "user" (toByteString uid)
    API.lookupAccount uid >>= mapM_ API.deleteAccount
    -- As user deletions are expensive resource-wise in the context of
    -- bulk user deletions (e.g. during team deletions),
    -- wait 'delay' ms before processing the next event
    delay <- fromMaybe defDeleteThrottleMillis . setDeleteThrottleMillis <$> view settings
    liftIO $ threadDelay (1000 * delay)
  DeleteService pid sid -> do
    Log.info $
      msg (val "Processing service delete event")
        ~~ field "provider" (toByteString pid)
        ~~ field "service" (toByteString sid)
    API.finishDeleteService pid sid
  where
    handleTimeout act = timeout 60000000 act >>= \case
      Just x -> pure x
      Nothing -> throwM (InternalEventTimeout n)

data InternalEventException
  = -- | 'onEvent' has timed out
    InternalEventTimeout InternalNotification
  deriving (Show)

instance Exception InternalEventException
