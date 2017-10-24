{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Push.Native.Fallback
    ( Candidates
    , prepare
    , execute
    , cancel
    ) where

import Control.Lens ((^.), (&), (?~), (.~), view)
import Control.Monad
import Control.Monad.Reader
import Data.ByteString.Conversion
import Data.Foldable (for_)
import Data.Id
import Gundeck.Aws.Arn (toText)
import Gundeck.Monad
import Gundeck.Options
import Gundeck.Types.Notification
import Gundeck.Types.Push
import Gundeck.Push.Native.Types
import System.Logger.Class (val, (~~), (.=))

import qualified Data.List                          as List
import qualified Data.Metrics                       as Metrics
import qualified Gundeck.Push.Native.Fallback.Data  as Data
import qualified Gundeck.Push.Data                  as Push
import qualified Gundeck.Push.Native                as Native
import qualified Gundeck.Push.Native.Fallback.Queue as Q
import qualified System.Logger.Class                as Log

-- | Candidates for fallback notifications.
data Candidates s = Candidates
    { _canRetry    :: [Address s]
    , _canFallback :: [(UserId, ClientId, AppName, Transport)]
    }

-- | Screen native push results for fallback candidates.
-- | Ensure we never send a fallback to the origin user.
prepare :: UserId -> [Result s] -> Maybe (Candidates s)
prepare orig rs = case List.foldl' go ([], []) rs of
    ([], []) -> Nothing
    (ns, qs) -> Just (Candidates ns qs)
  where
    go cs@(now, queue) r = case r of
        Failure PayloadTooLarge a -> (a : now, queue)
        Failure MissingKeys     a -> (a : now, queue)
        Success a
            | Just t <- a^.addrFallback
            , a^.addrUser /= orig
            -> (now, (a^.addrUser, a^.addrClient, a^.addrApp, t) : queue)
        _ -> cs

-- | Send a fallback notification to the given candidates.
execute :: NotificationId -- ^ The ID of the fallback notification.
        -> Priority       -- ^ The priority of the fallback notification.
        -> Candidates s   -- ^ The candidates for receiving the fallback notification.
        -> Gundeck [Result s]
execute nid prio (Candidates now queue) = do
    let m = Native.Notice nid prio (Just apsFallback)
    r <- Native.push m now
    unless (null queue) $ do
        e <- ask
        n <- foldM (schedule e m) (0 :: Word) queue
        Metrics.counterAdd n (Metrics.path "push.fallback.schedule") (e^.monitor)
    return r
  where
    schedule e msg !n (usr, clt, app, trp) = do
        Log.debug $ logMsg usr clt app trp "Scheduling fallback notification"
        ok <- Q.schedule (e^.fbQueue) usr nid $
            runDirect e (send usr clt app trp msg (e^.options.fallback.fbSkipFallbacks))
        if ok then return (n + 1) else do
            Log.err $ logMsg usr clt app trp "Failed to schedule fallback notification"
            return n

    send usr clt app trp msg skip = do
        cancelled <- Data.isCancelled usr nid
        unless cancelled $ do
            -- TODO: We could avoid looking up the addresses here again, if we
            --       retain the fallback addresses in `Gundeck.Push.nativeTargets`.
            addr <- List.find (fallbackAddress clt app trp)
                <$> Push.lookup usr Push.One
            for_ addr $ \a -> do
                Log.debug $ logMsg usr clt app trp "Sending fallback notification"
                    ~~ "arn" .= toText (a^.addrEndpoint)
                unless skip $
                    void $ Native.push msg [a]
                Metrics.counterIncr (Metrics.path "push.fallback.send")
                    =<< view monitor

    fallbackAddress clt app trp a =
        a^.addrClient    == clt &&
        a^.addrApp       == app &&
        a^.addrTransport == trp

    logMsg usr clt app trp msg =
           "user"      .= toByteString usr
        ~~ "client"    .= toByteString clt
        ~~ "app"       .= appNameText app
        ~~ "transport" .= show trp
        ~~ Log.msg (val msg)

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
