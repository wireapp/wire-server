{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Gundeck.Push.Websocket (push, pushBulk) where

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
import Data.List
import Data.List1
import Data.Set (Set)
import Data.Time.Clock.POSIX
import Gundeck.Monad
import Gundeck.Types.BulkPush
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
     -> UserId
     -> Maybe ConnId
     -> Set ConnId
     -> Gundeck [Presence]
push n (toList -> ts) ou oc cs = do
    prs <- pushOne $ PushParams n ts ou oc cs
    return prs

pushOne :: PushParams -> Gundeck [Presence]
pushOne params = do
    ps <- getPresences params
    (ok, gone) <- foldM onResult ([], []) =<< send (notif params) ps
    Presence.deleteAll gone
    return ok

pushBulk :: [PushParams] -> Gundeck [[Presence]]
pushBulk paramss = do
    notifPrs <- concat <$> mapM mapPresences paramss
    let nByCannon = groupBy (\(_, prsa) (_, prsb) -> cannonhost prsa == cannonhost prsb) notifPrs
    resps <- mapM sendBulk nByCannon
    return $ mapResponses resps
  where
    mapPresences params = do
        ps <- getPresences params
        return [ (notif params, presence) | presence <- ps ]

    mapResponses resps = [[]]

getPresences :: PushParams-> Gundeck [Presence]
getPresences params = do
    pp <- handleAny noPresences listPresences
    return pp
  where
    listPresences = excludeOrigin
                  . filterByConnection
                  . concat
                  . filterByClient
                  . zip (targets params)
                 <$> Presence.listAll (view targetUser <$> targets params)

    noPresences exn = do
        Log.err $ Log.field "error" (show exn)
               ~~ Log.msg (val "Failed to get presences.")
        return []

    filterByClient = map $ \(tgt, ps) -> let cs = tgt^.targetClients in
        if null cs then ps
        else filter (maybe True (`elem` cs) . clientId) ps

    filterByConnection =
        if Set.null (conns params) then id
        else filter ((`Set.member` (conns params)) . connId)

    excludeOrigin =
        let
            neqUser p = originUser params /= userId p
            neqConn p = originConn params /= Just (connId p)
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

    eval p (Left  e) = PushFailure p e
    eval p (Right r) = if statusCode r == 200 then PushSuccess p else PushGone p

sendBulk :: [(Notification, Presence)] -> Gundeck [PushResult]
sendBulk nps = do
    let cannonUrl = getCannonUrl $ Data.List.head nps
    case cannonUrl of
        Nothing -> concat <$> mapM sendOld nps
        Just cu -> do
            let js = encode $ BulkPush $ map makePayload nps
            req <- Http.setUri empty (fromURI cu)
            res <- recovering x1 rpcHandlers $ const $
                rpc' "cannon" (check req)
                    $ method POST
                    . contentJson
                    . lbytes js
                    . timeout 3000 -- ms
            return []
  where
    makePayload (n, p) = UserDevicePayload (userId p) (connId p) n

    getPField (_, p) f = f p

    getCannonUrl np = let mch = getPField np cannonhost
        in case mch of
            Nothing -> Nothing
            Just  _ -> getPField np resourceb

    sendOld (n, p) = send n [p]

logPresence :: Presence -> Log.Msg -> Log.Msg
logPresence p =
       Log.field "user"   (toByteString (userId p))
    ~~ Log.field "zconn"  (toByteString (connId p))

check r = r { Http.checkResponse = \rq rs ->
    when (responseStatus rs `notElem` [status200, status410]) $
        let ex = StatusCodeException (rs { responseBody = () }) mempty
        in throwM $ HttpExceptionRequest rq ex
}

x1 = limitRetries 1
