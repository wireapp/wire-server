{-# LANGUAGE OverloadedStrings #-}

module Brig.InternalEvent.Process
    ( onEvent
    , Brig.InternalEvent.Process.listen
    ) where

import Brig.App
import Brig.InternalEvent.Types
import Brig.Stomp as Stomp
import Control.Lens
import Data.ByteString.Conversion
import System.Logger.Class (field, msg, (~~), val)

import qualified Brig.API.User          as API
import qualified System.Logger.Class    as Log

onEvent :: InternalNotification -> AppIO ()
onEvent (DeleteUser uid) = do
    Log.info $ field "user" (toByteString uid) ~~ msg (val "Processing delete event")
    API.lookupAccount uid >>= mapM_ API.deleteAccount

listen :: AppIO ()
listen = view stompEnv >>= \e ->
    Stomp.listen (broker e) (internalQueue e) onEvent
