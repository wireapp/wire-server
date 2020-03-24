{-# LANGUAGE StrictData #-}

module Galley.Intra.Push
  ( -- * Push
    Push,
    newPush,
    newPush1,
    push,
    push1,
    pushSome,
    PushEvent (..),

    -- * Push Configuration
    pushConn,
    pushTransient,
    pushRoute,
    pushNativePriority,
    pushAsync,

    -- * Push Recipients
    Recipient,
    recipient,
    userRecipient,
    recipientUserId,
    recipientClients,

    -- * Re-Exports
    Gundeck.Route (..),
    Gundeck.Priority (..),
  )
where

import Bilge hiding (options)
import Bilge.RPC
import Bilge.Retry
import Control.Lens ((&), (.~), (^.), makeLenses, set, view)
import Control.Monad.Catch
import Control.Retry
import Data.Aeson (Object)
import Data.Id
import Data.Json.Util
import Data.List.Extra (chunksOf)
import Data.List1
import Data.Misc
import Data.Range
import qualified Data.Set as Set
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT
import Galley.App
import Galley.Options
import Galley.Types
import qualified Galley.Types.Teams as Teams
import Gundeck.Types.Push.V2 (RecipientClients (..))
import qualified Gundeck.Types.Push.V2 as Gundeck
import Imports
import Network.HTTP.Types.Method
import Safe (headDef, tailDef)
import System.Logger.Class hiding (new)
import UnliftIO (mapConcurrently)
import Util.Options

data PushEvent
  = ConvEvent Event
  | TeamEvent Teams.Event

pushEventJson :: PushEvent -> Object
pushEventJson (ConvEvent e) = toJSONObject e
pushEventJson (TeamEvent e) = toJSONObject e

data Recipient
  = Recipient
      { _recipientUserId :: UserId,
        _recipientClients :: RecipientClients
      }

makeLenses ''Recipient

recipient :: Member -> Recipient
recipient m = Recipient (memId m) RecipientClientsAll

userRecipient :: UserId -> Recipient
userRecipient u = Recipient u RecipientClientsAll

data Push
  = Push
      { _pushConn :: Maybe ConnId,
        _pushTransient :: Bool,
        _pushRoute :: Gundeck.Route,
        _pushNativePriority :: Maybe Gundeck.Priority,
        _pushAsync :: Bool,
        pushOrigin :: UserId,
        pushRecipients :: List1 Recipient,
        pushJson :: Object
      }

makeLenses ''Push

newPush1 :: UserId -> PushEvent -> List1 Recipient -> Push
newPush1 from e rr =
  Push
    { _pushConn = Nothing,
      _pushTransient = False,
      _pushRoute = Gundeck.RouteAny,
      _pushNativePriority = Nothing,
      _pushAsync = False,
      pushJson = pushEventJson e,
      pushOrigin = from,
      pushRecipients = rr
    }

newPush :: UserId -> PushEvent -> [Recipient] -> Maybe Push
newPush _ _ [] = Nothing
newPush u e (r : rr) = Just $ newPush1 u e (list1 r rr)

-- | Asynchronously send a single push, chunking it into multiple
-- requests if there are more than 128 recipients.
push1 :: Push -> Galley ()
push1 p = push (list1 p [])

pushSome :: [Push] -> Galley ()
pushSome [] = return ()
pushSome (x : xs) = push (list1 x xs)

-- | Asynchronously send multiple pushes, aggregating them into as
-- few requests as possible, such that no single request targets
-- more than 128 recipients.
push :: List1 Push -> Galley ()
push ps = do
  let (async, sync) = partition _pushAsync (toList ps)
  forM_ (pushes async) $ gundeckReq >=> callAsync "gundeck"
  void $ mapConcurrently (gundeckReq >=> call "gundeck") (pushes sync)
  return ()
  where
    pushes = fst . foldr chunk ([], 0)
    chunk p (pss, !n) =
      let r = recipientList p
          nr = length r
       in if n + nr > maxRecipients
            then
              let pss' = map (pure . toPush p) (chunksOf maxRecipients r)
               in (pss' ++ pss, 0)
            else
              let hd = headDef [] pss
                  tl = tailDef [] pss
               in ((toPush p r : hd) : tl, n + nr)
    maxRecipients = 128
    recipientList p = map (toRecipient p) . toList $ pushRecipients p
    toPush p r =
      let pload = Gundeck.singletonPayload (pushJson p)
       in Gundeck.newPush (pushOrigin p) (unsafeRange (Set.fromList r)) pload
            & Gundeck.pushOriginConnection .~ _pushConn p
            & Gundeck.pushTransient .~ _pushTransient p
            & maybe id (set Gundeck.pushNativePriority) (_pushNativePriority p)
    toRecipient p r =
      Gundeck.recipient (_recipientUserId r) (_pushRoute p)
        & Gundeck.recipientClients .~ _recipientClients r

-----------------------------------------------------------------------------
-- Helpers

gundeckReq :: [Gundeck.Push] -> Galley (Request -> Request)
gundeckReq ps = do
  o <- view options
  return $
    host (encodeUtf8 $ o ^. optGundeck . epHost)
      . port (portNumber $ fromIntegral (o ^. optGundeck . epPort))
      . method POST
      . path "/i/push/v2"
      . json ps
      . expect2xx

callAsync :: LT.Text -> (Request -> Request) -> Galley ()
callAsync n r = void . forkIO $ void (call n r) `catches` handlers
  where
    handlers =
      [ Handler $ \(x :: RPCException) -> err (rpcExceptionMsg x),
        Handler $ \(x :: SomeException) -> err $ "remote" .= n ~~ msg (show x)
      ]

call :: LT.Text -> (Request -> Request) -> Galley (Response (Maybe LByteString))
call n r = recovering x3 rpcHandlers (const (rpc n r))

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000
