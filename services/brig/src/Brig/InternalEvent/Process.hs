{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Brig.InternalEvent.Process
    ( onEvent
    , Brig.InternalEvent.Process.listen
    ) where

import Brig.App
import Brig.InternalEvent.Types
import Brig.Stomp as Stomp
import Control.Lens
import Control.Monad.Catch
import Data.ByteString.Conversion
import System.Logger.Class (field, msg, (~~), val)
import UnliftIO (timeout)

import qualified Brig.API.User          as API
import qualified System.Logger.Class    as Log

-- | Handle an internal event.
--
-- Has a one-minute timeout that should be enough for anything that it does
-- (currently it only deletes users).
onEvent :: InternalNotification -> AppIO ()
onEvent n = handleTimeout $ case n of
    DeleteUser uid -> do
        Log.info $ field "user" (toByteString uid) ~~ msg (val "Processing delete event")
        API.lookupAccount uid >>= mapM_ API.deleteAccount
  where
    handleTimeout act = timeout 60000000 act >>= \case
        Just x  -> pure x
        Nothing -> throwM (InternalEventTimeout n)

listen :: AppIO ()
listen = view stompEnv >>= \e ->
    Stomp.listen (broker e) (internalQueue e) onEvent

data InternalEventException
    -- | 'onEvent' has timed out
    = InternalEventTimeout InternalNotification
  deriving Show

instance Exception InternalEventException
