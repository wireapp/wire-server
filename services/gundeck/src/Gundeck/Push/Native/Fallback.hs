{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Push.Native.Fallback
    ( prepare
    , execute
    ) where

import Imports
import Control.Lens ((?~), (.~))
import Gundeck.Monad
import Gundeck.Types.Notification
import Gundeck.Types.Push
import Gundeck.Push.Native.Types

import qualified Gundeck.Push.Native as Native

-- | Screen native push results for fallback candidates.
-- | Ensure we never send a fallback to the origin user.
prepare :: [Result s] -> [Address s]
prepare rs = foldl' go [] rs
  where
    go :: [Address s] -> Result s -> [Address s]
    go now r = case r of
        Failure PayloadTooLarge a -> (a : now)
        Failure MissingKeys     a -> (a : now)
        _                         -> now

-- | Send a fallback notification to the given candidates.
execute :: NotificationId -- ^ The ID of the fallback notification.
        -> Priority       -- ^ The priority of the fallback notification.
        -> [Address s]    -- ^ The candidates for receiving the fallback notification.
        -> Gundeck [Result s]
execute nid prio now = do
    let m = Native.Notice nid prio (Just apsFallback)
    Native.push m now

apsFallback :: ApsData
apsFallback = apsData (ApsLocKey "push.notification.new_message") []
            & apsSound ?~ ApsSound "new_message_apns.caf"
            & apsBadge .~ False
