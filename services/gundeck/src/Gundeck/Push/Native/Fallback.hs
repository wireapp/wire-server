{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Push.Native.Fallback
    ( Candidates
    , prepare
    , execute
    ) where

import Imports
import Control.Lens ((?~), (.~))
import Data.Id
import Gundeck.Monad
import Gundeck.Types.Notification
import Gundeck.Types.Push
import Gundeck.Push.Native.Types

import qualified Gundeck.Push.Native                as Native

-- | Candidates for fallback notifications.
data Candidates s = Candidates
    { _canRetry :: [Address s]
    }

-- | Screen native push results for fallback candidates.
-- | Ensure we never send a fallback to the origin user.
--
-- REFACTOR: this can be syntactically sweetened some more, and _orig can be removed entirely.
prepare :: UserId -> [Result s] -> Maybe (Candidates s)
prepare _orig rs = case foldl' go [] rs of
    [] -> Nothing
    ns -> Just (Candidates ns)
  where
    go cs@now r = case r of
        Failure PayloadTooLarge a -> (a : now)
        Failure MissingKeys     a -> (a : now)
        _                         -> cs

-- | Send a fallback notification to the given candidates.
execute :: NotificationId -- ^ The ID of the fallback notification.
        -> Priority       -- ^ The priority of the fallback notification.
        -> Candidates s   -- ^ The candidates for receiving the fallback notification.
        -> Gundeck [Result s]
execute nid prio (Candidates now) = do
    let m = Native.Notice nid prio (Just apsFallback)
    Native.push m now

apsFallback :: ApsData
apsFallback = apsData (ApsLocKey "push.notification.new_message") []
            & apsSound ?~ ApsSound "new_message_apns.caf"
            & apsBadge .~ False
