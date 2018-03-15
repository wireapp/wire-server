{-# LANGUAGE OverloadedStrings #-}

module Brig.AWS.InternalEvent where

import Brig.App
import Brig.AWS.Types (InternalNotification (..))
import Data.ByteString.Conversion
import System.Logger.Class (field, msg, (~~), val)

import qualified Brig.API.User          as API
import qualified System.Logger.Class    as Log

onEvent :: InternalNotification -> AppIO ()
onEvent (DeleteUser uid) = do
    Log.info $ field "user" (toByteString uid) ~~ msg (val "Processing delete event")
    API.lookupAccount uid >>= mapM_ API.deleteAccount
