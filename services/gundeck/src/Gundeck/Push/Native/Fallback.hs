{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Push.Native.Fallback
    ( Candidates
    , prepare
    , execute
    , cancel
    ) where

import Imports
import Control.Lens ((?~), (.~), view)
import Data.ByteString.Conversion
import Data.Id
import Gundeck.Monad
import Gundeck.Types.Notification
import Gundeck.Types.Push
import Gundeck.Push.Native.Types
import System.Logger.Class (val, (~~), (.=))

import qualified Gundeck.Push.Native.Fallback.Data  as Data
import qualified Gundeck.Push.Native                as Native
import qualified Gundeck.Push.Native.Fallback.Queue as Q
import qualified System.Logger.Class                as Log

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

-- | Cancel pending fallback notifications.
cancel :: UserId -> NotificationId -> Gundeck ()
cancel u n = do
    q  <- view fbQueue
    ok <- Q.cancel q u n
    if ok then Log.debug
        $  "user"         .= toByteString u
        ~~ "notification" .= toByteString n
        ~~ Log.msg (val "Fallback notification cancelled early")
    else
        -- Either some other node has the pending notification or the
        -- cancellation came before we even scheduled the fallback, so we
        -- have to write a (short-lived) db record signaling the cancellation.
        -- The db record needs to live long enough for all nodes
        -- to see it, including some tolerance for clock drift.
        let ttl = fromIntegral (Q.delayTime (Q.queueDelay q) * 10)
        in Data.cancel u n ttl

apsFallback :: ApsData
apsFallback = apsData (ApsLocKey "push.notification.new_message") []
            & apsSound ?~ ApsSound "new_message_apns.caf"
            & apsBadge .~ False
