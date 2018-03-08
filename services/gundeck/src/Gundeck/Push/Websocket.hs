{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Gundeck.Push.Websocket (push) where

import Bilge
import Bilge.Retry (rpcHandlers)
import Bilge.RPC
import Control.Exception.Enclosed (handleAny)
import Control.Monad (foldM, when)
import Control.Monad.Catch (SomeException (..), throwM)
import Control.Lens ((^.), view)
import Control.Retry
import Data.Aeson (encode)
import Data.ByteString.Conversion
import Data.Foldable (toList)
import Data.Id
import Data.List1
import Data.Set (Set)
import Data.Time.Clock.POSIX
import Gundeck.Monad
import Gundeck.Types.Notification
import Gundeck.Types.Presence
import Gundeck.Util
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Types (StdMethod (POST), status200, status410)
import System.Logger.Class ((~~), val, (+++))

import qualified Data.Metrics                 as Metrics
import qualified Data.Set                     as Set
import qualified Gundeck.Presence.Data        as Presence
import qualified Network.HTTP.Client.Internal as Http
import qualified System.Logger.Class          as Log

push :: Notification
     -> List1 NotificationTarget
     -> UserId -- Origin user.
     -> Maybe ConnId -- Origin device connection.
     -> Set ConnId -- Only target these connections.
     -> Gundeck [Presence]
push notif (toList -> tgts) originUser originConn conns = do
    pp <- handleAny noPresences listPresences
    (ok, gone) <- foldM onResult ([], []) =<< send notif pp
    Presence.deleteAll gone
    return ok
  where
    listPresences = excludeOrigin
                  . filterByConnection
                  . concat
                  . filterByClient
                  . zip tgts
                 <$> Presence.listAll (view targetUser <$> tgts)

    noPresences exn = do
        Log.err $ Log.field "error" (show exn)
               ~~ Log.msg (val "Failed to get presences.")
        return []

    filterByClient = map $ \(tgt, ps) -> let cs = tgt^.targetClients in
        if null cs then ps
        else filter (maybe True (`elem` cs) . clientId) ps

    filterByConnection =
        if Set.null conns then id
        else filter ((`Set.member` conns) . connId)

    excludeOrigin =
        let
            neqUser p = originUser /= userId p
            neqConn p = originConn /= Just (connId p)
        in
            filter (\p -> neqUser p || neqConn p)

    onResult (ok, gone) (PushSuccess p) = do
        Log.debug $ logPresence p ~~ Log.msg (val "WebSocket push success")
        return (p:ok, gone)

    onResult (ok, gone) (PushGone p) = do
        Log.debug $ logPresence p ~~ Log.msg (val "WebSocket presence gone")
        return (ok, p:gone)

    onResult (ok, gone) (PushFailure p _) = do
        view monitor >>= Metrics.counterIncr (Metrics.path "push.ws.unreachable")
        Log.info $ logPresence p
            ~~ Log.field "created_at" (ms $ createdAt p)
            ~~ Log.msg (val "WebSocket presence unreachable: " +++ toByteString (resource p))
        now <- posixTime
        if now - createdAt p > 10 * posixDay
           then return (ok, p:gone)
           else return (ok, gone)

    posixDay = Ms (round (1000 * posixDayLength))

-----------------------------------------------------------------------------
-- Internal

data PushResult
    = PushSuccess Presence
    | PushGone    Presence
    | PushFailure Presence SomeException

send :: Notification -> [Presence] -> Gundeck [PushResult]
send n pp =
    let js = encode n in
    zipWith eval pp <$> mapAsync (fn js) pp
  where
    fn js p = do
        req <- Http.setUri empty (fromURI (resource p))
        recovering x1 rpcHandlers $ const $
            rpc' "cannon" (check req)
                $ method POST
                . contentJson
                . lbytes js
                . timeout 3000 -- ms

    check r = r { Http.checkResponse = \rq rs ->
        when (responseStatus rs `notElem` [status200, status410]) $
            let ex = StatusCodeException (rs { responseBody = () }) mempty
            in throwM $ HttpExceptionRequest rq ex
    }

    eval p (Left  e) = PushFailure p e
    eval p (Right r) = if statusCode r == 200 then PushSuccess p else PushGone p

    x1 = limitRetries 1

logPresence :: Presence -> Log.Msg -> Log.Msg
logPresence p =
       Log.field "user"   (toByteString (userId p))
    ~~ Log.field "zconn"  (toByteString (connId p))
